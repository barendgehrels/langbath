// Language Bath - Common
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This unit contains a decorator for BASS, the 3rd party mp3 playing library
//  - It only uses seconds in its interface
//  - It uses a timer (member of this class) to stop after X seconds
//  - With this timer, the main usage to play a small part of an mp3
//  - Note that BASS only supports constant bitrate (CBR) mp3's, others can be played
//    but their timings are incorrect and can change if you combine two mp3's afterwards.
unit lb_bass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  {$IFDEF MSWINDOWS}
  Windows,
  {$endif}
  BASS;

type

  {$IFDEF MSWINDOWS}
  TBassOwner = HWND;
  {$ELSE}
  TBassOwner = Pointer;
  {$ENDIF}

  TLevel = record
    positionSeconds : double;
    level : double;
  end;
  TArrayOfLevel = array of TLevel;

  TLoopEvent = function(Sender: TObject; repeatCount : integer) : boolean of object;
  TLoopEndEvent = procedure(Sender: TObject) of object;
  TBassReportEvent = procedure(Sender: TObject; pos, level : double) of object;

  { TLbBass }

  TLbBass = class

    iFileLoaded : boolean;
    iEnableGetLevels : boolean;

    iChannel : HSTREAM;
    iLoopSamples : TArrayOfLevel;

    // Extra channel, not for playing but for returning samples.
    // Only opened on request. A request will open file twice.
    iChannelOnlyDecode : HSTREAM;

    iPaused : boolean;
    iTimer : TTimer;

    iTimeBeginBytes : QWORD;
    iTimeEndBytes : QWORD;
    iTimeBeginSeconds : double;
    iTimeEndSeconds : double;

    iRepeatIndex : integer;
    iOnLoop : TLoopEvent;
    iOnLoopEnd : TLoopEndEvent;

  private
    function IsPlayable : boolean;

    procedure LoopTimer(Sender: TObject);
    procedure OnceTimer(Sender: TObject);

    procedure StartPlaySelection(timeBeginSeconds, timeEndSeconds : double);
    function Loop : boolean;

  public

    constructor Create(const filename : string; enableGetLevels : boolean);
    destructor Destroy; override;

    function GetReport(out pos, loopFraction, level : double) : boolean;
    function CurrentLevel(period : double) : double;

    // Play the whole file
    // (unless pauze/stop is called)
    procedure PlayFromStart;

    // Play the sound from the specified position until the end
    // (unless pauze/stop is called)
    procedure PlayFromPosition(posSeconds : double);

    // Play a selection (begin..end) of the specified soundfile.
    procedure PlaySelection(timeBeginSeconds, timeEndSeconds : double);

    // Play a selection over and over
    procedure LoopSelection(timeBeginSeconds, timeEndSeconds : double);

    // Stop playback (if active)
    procedure Stop;

    // Pauze playback
    procedure PauseMusic;

    // Return the length of the mp3. Also if position started from a specified position,
    // or a selection is played, the length of the whole mp3 is returned.
    function LengthSeconds : double;

    // Return the current position
    function GetPositionSeconds : double;

    // Return true if actively playing.
    function Active : boolean;

    function GetSamples(p1, p2 : double) : TArrayOfLevel;

    property RepeatIndex : integer read iRepeatIndex;

    property OnLoop: TLoopEvent read iOnLoop write iOnLoop;
    property OnLoopEnd: TLoopEndEvent read iOnLoopEnd write iOnLoopEnd;
  end;

procedure InitBass(win : TBassOwner);

implementation

uses lb_lib;

const
  // Peek at 80% of playing time
  KFraction : double = 0.8;

  // Stop if just before end of the selection
  KStopBeforeMilliSeconds : cardinal = 20;

var
  gInitialized : boolean;
  gLevelLock: TRTLCriticalSection;

procedure InitBass(win: TBassOwner);
begin
  {$IFDEF MSWINDOWS}
  // check the correct BASS was loaded
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
  begin
    Log('An incorrect version of BASS.DLL was loaded');
    exit;
  end;
  {$endif}

  // initialize BASS - default device
  if not BASS_Init(-1, 44100, 0, win, nil) then
  begin
    Log('Can''t initialize device');
    exit;
  end;
  gInitialized := true;
end;

constructor TLbBass.Create(const filename : string; enableGetLevels : boolean);
begin
  iChannel := 0;
  iChannelOnlyDecode := 0;

  if gInitialized and FileExists(filename) then
  begin
    iChannel := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_SPEAKER_FRONT);

    // Create timer for playing or looping a selection
    iTimer := TTimer.Create(nil);
    iTimer.Enabled := false;

    iFileLoaded := iChannel > 0;

    if enableGetLevels then
    begin
      iChannelOnlyDecode := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_STREAM_DECODE);
      iEnableGetLevels := iChannelOnlyDecode > 0;
    end
  end;
end;

destructor TLbBass.Destroy;
begin
  BASS_StreamFree(iChannel);
  BASS_StreamFree(iChannelOnlyDecode);
  iTimer.Free;
  inherited Destroy;
end;

function TLbBass.Loop : boolean;
begin
  result := true;
  inc(iRepeatIndex);
  if not assigned(iOnLoop) or iOnLoop(self, iRepeatIndex) then
  begin
    // Event returned true, this means: keep looping
    log('loop');
    BASS_ChannelSetPosition(iChannel, iTimeBeginBytes, BASS_POS_BYTE);
  end
  else
  begin
    // Event returned false, this means: stop looping
    Stop;
    if assigned(iOnLoopEnd) then
    begin
      log('loop end');
      iOnLoopEnd(self);
      result := false;
    end;
  end;
end;

function TLbBass.IsPlayable : boolean;
begin
  result := iFileLoaded;
end;

procedure TLbBass.LoopTimer(Sender: TObject);
var timeCurSeconds : double;
  timeCurBytes : QWORD;
  milliSecondsLeft : integer;
  continue : boolean;
begin
  iTimer.Enabled := false;

  if not iFileLoaded then exit;

  continue := false;
  timeCurBytes := BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE);
  milliSecondsLeft := trunc(1000 * (iTimeEndSeconds - BASS_ChannelBytes2Seconds(iChannel, timeCurBytes)));

  if (timeCurBytes > iTimeEndBytes) or (milliSecondsLeft < KStopBeforeMilliSeconds) then
  begin
    // Time is over, or (within some ms) nearly over. Play it again
    log(format('TIMER loop %d %d', [ord(timeCurBytes > iTimeEndBytes), milliSecondsLeft]));

    continue := Loop;

    timeCurBytes := iTimeBeginBytes;
  end;

  if not continue then
  begin
    // Looping stopped
    log('TIMER end');
  end;

  if BASS_ChannelIsActive(iChannel) <> 1 then
  begin
    // Short mp3's, completely played, might stop. Play again.
    BASS_ChannelPlay(iChannel, False);
  end;

  // It is still playing. Update timer
  timeCurSeconds := BASS_ChannelBytes2Seconds(iChannel, timeCurBytes);

  if timeCurSeconds < iTimeEndBytes then
  begin
    iTimer.Interval := round(kFraction * 1000 * (iTimeEndSeconds  - timeCurSeconds));
    log(format('TIMER current %.3f %.3f -> %d', [timeCurSeconds, iTimeEndSeconds, iTimer.interval]));
  end
  else
  begin
    iTimer.Interval := 100;
    log(format('TIMER wait %.3f %.3f -> %d', [timeCurSeconds, iTimeEndSeconds, iTimer.interval]));
  end;
  iTimer.Enabled := true;
end;

procedure TLbBass.OnceTimer(Sender: TObject);
var timeCurSeconds : double;
  timeCurBytes : QWORD;
  milliSecondsLeft : integer;
begin
  iTimer.Enabled := false;

  if iFileLoaded and (BASS_ChannelIsActive(iChannel) = 1) then
  begin
    timeCurBytes := BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE);
    milliSecondsLeft := trunc(1000 * (iTimeEndSeconds - BASS_ChannelBytes2Seconds(iChannel, timeCurBytes)));

    if (timeCurBytes > iTimeEndBytes) or (milliSecondsLeft < KStopBeforeMilliSeconds) then
    begin
      log(format('TIMER one end %d %d', [ord(timeCurBytes > iTimeEndBytes), milliSecondsLeft]));
      Stop;
    end
    else
    begin
      // It is still playing. Update timer
      timeCurSeconds := BASS_ChannelBytes2Seconds(iChannel, timeCurBytes);
      iTimer.Interval := round(kFraction * 1000 * (iTimeEndSeconds - timeCurSeconds));
      log(format('TIMER once current %.3f %.3f -> %d', [timeCurSeconds, iTimeEndSeconds, iTimer.interval]));
      iTimer.Enabled := true;
    end;
  end;
end;

procedure TLbBass.PlayFromStart;
begin
  Stop;
  if IsPlayable then
  begin
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.PlayFromPosition(posSeconds : double);
var p : QWORD;
begin
  Stop;
  if IsPlayable then
  begin
    p := BASS_ChannelSeconds2Bytes(iChannel, posSeconds);
    LOG('Play from ' + floattostr(posSeconds) + ' = ' + inttostr(p));
    BASS_ChannelSetPosition(iChannel, p, BASS_POS_BYTE);
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.StartPlaySelection(timeBeginSeconds, timeEndSeconds : double);
begin
  Stop;

  if IsPlayable and (timeEndSeconds < 0) then timeEndSeconds := LengthSeconds;

  if IsPlayable and (timeEndSeconds > timeBeginSeconds) then
  begin
    iRepeatIndex := 0;
    iTimeBeginBytes := BASS_ChannelSeconds2Bytes(iChannel, timeBeginSeconds);
    iTimeEndBytes := BASS_ChannelSeconds2Bytes(iChannel, timeEndSeconds);
    iTimeBeginSeconds := timeBeginSeconds;
    iTimeEndSeconds := timeEndSeconds;

    iLoopSamples := GetSamples(timeBeginSeconds, timeEndSeconds);

    // Set the timer to just before the end
    iTimer.Interval := round(kFraction * 1000 * (timeEndSeconds - timeBeginSeconds));
    iTimer.Enabled := true;
    log(format('TIMER start %.3f %.3f -> %d', [timeBeginSeconds, timeEndSeconds, iTimer.interval]));

    BASS_ChannelSetPosition(iChannel, iTimeBeginBytes, BASS_POS_BYTE);
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.PlaySelection(timeBeginSeconds, timeEndSeconds : double);
begin
  // As if it is a loop, but stop
  iTimer.OnTimer := @OnceTimer;
  StartPlaySelection(timeBeginSeconds, timeEndSeconds);
end;

procedure TLbBass.LoopSelection(timeBeginSeconds, timeEndSeconds : double);
begin
  iTimer.OnTimer := @LoopTimer;
  StartPlaySelection(timeBeginSeconds, timeEndSeconds);
end;

procedure TLbBass.Stop;
begin
  iTimer.Enabled := false;
  iLoopSamples := [];

  if IsPlayable then
  begin
    BASS_ChannelStop(iChannel);
  end;
end;

procedure TLbBass.PauseMusic;
begin
  if IsPlayable then
  begin
    if iPaused then
    begin
      BASS_ChannelPlay(iChannel, FALSE);
    end
    else
    begin
      BASS_ChannelPause(iChannel);
    end;
    iPaused := not iPaused;
  end;
end;

function TLbBass.LengthSeconds : double;
begin
  result := 0;
  if iFileLoaded then
  begin
    result := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetLength(iChannel, BASS_POS_BYTE));
  end;
end;

function TLbBass.Active : boolean;
begin
  result := IsPlayable and (BASS_ChannelIsActive(iChannel) = 1);
end;

function TLbBass.GetPositionSeconds : double;
begin
  result := 0;
  if iFileLoaded then
  begin
    result := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE));
  end;
end;

function TLbBass.CurrentLevel(period : double) : double;
var level : single;
begin
  result := 0;
  if Active and (period < 1.0) then
  begin
    // If sampling is called in combination with sync points, the program often hangs.
    // But sync points are gone, timer is now.
    // Anyway, getting levels is protected with a lock.
    EnterCriticalSection(gLevelLock);
    try
      BASS_ChannelGetLevelEx(iChannel, @level, period, BASS_LEVEL_MONO);
    finally
      LeaveCriticalSection(gLevelLock);
    end;
    result := level;
  end;
end;

function TLbBass.GetReport(out pos, loopFraction, level : double) : boolean;
var i : integer;
  d, minD : double;
begin
  level := 0;
  pos := 0;
  loopFraction := 0;

  result := BASS_ChannelIsActive(iChannel) = 1;
  if result then
  begin
    pos := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE));
    if (pos >= iTimeBeginSeconds)
    and (pos <= iTimeEndSeconds)
    and (iTimeEndSeconds > iTimeBeginSeconds)
    then
    begin
      loopFraction := (pos - iTimeBeginSeconds) / (iTimeEndSeconds - iTimeBeginSeconds);
    end;

    // Use the samples (decoded in the alternative channel) to report the current level.
    // (TODO: if the sentences are short, otherwise it will be a lot).
    if length(iLoopSamples) > 0 then
    begin
      minD := 999.0;
      for i := low(iLoopSamples) to high(iLoopSamples) do
      begin
        d := abs(iLoopSamples[i].positionSeconds - pos);
        if d < minD then
        begin
          minD := d;
          level := iLoopSamples[i].level;
        end;
      end;
    end
    else
    begin
      level := CurrentLevel(0.02);
    end;
  end;
end;

function TLbBass.GetSamples(p1, p2 : double) : TArrayOfLevel;

  // Workaround to get rid of the warning "info is not initialized"
  // (it should actually be an out-parameter in BASS)
  function CreateInfo : BASS_CHANNELINFO;
  begin
    result.freq := 0;
    fillchar(result, 0, sizeof(result));
  end;

const period : double = 0.01; // 10 ms
var b1, b2 : QWORD;
  buf : array [0..2000] of BYTE;
  info: BASS_CHANNELINFO;
  bytesPerBatch : integer;
  level : single;
  pos : double;
  n : integer;
begin
  result := [];
  n := 0;
  if iFileLoaded and iEnableGetLevels and (iChannelOnlyDecode > 0) then
  begin
    EnterCriticalSection(gLevelLock);
    try
      log('Init sampling');
      b1 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p1);
      b2 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p2);

      info := CreateInfo;
      BASS_ChannelGetInfo(iChannelOnlyDecode, info);

      bytesPerBatch := info.freq div 50; // 882 for freq 44100
      if bytesPerBatch >= length(buf) then bytesPerBatch := length(buf) - 1;

      log(format('Start sampling %s: %f - %f (%d - %d) period: %f, batch: %d, freq: %d',
        [info.filename, p1, p2, b1, b2, period, bytesPerBatch, info.freq]));

      BASS_ChannelSetPosition(iChannelOnlyDecode, b1, BASS_POS_BYTE);
      // Get the levels
      while (BASS_ChannelIsActive(iChannelOnlyDecode) > 0) and (b1 < b2) do
      begin
        pos := BASS_ChannelBytes2Seconds(iChannelOnlyDecode, BASS_ChannelGetPosition(iChannelOnlyDecode, BASS_POS_BYTE));
        // TODO check this / comment why / 3
        BASS_ChannelGetData(iChannelOnlyDecode, @buf, bytesPerBatch div 3);
        BASS_ChannelGetLevelEx(iChannelOnlyDecode, @level, period, BASS_LEVEL_MONO);
        inc(b1, bytesPerBatch);
        //log(format('Read: [%d] at %d = %.4f : %.4f', [bytesRead, b1, pos, level]));
        SetLength(result, n + 1);
        result[n].positionSeconds := pos;
        result[n].Level := level;
        inc(n);
      end;
    finally
      log('End sampling');
      LeaveCriticalSection(gLevelLock);
    end;
  end;
end;

initialization
begin
  gInitialized := false;
  InitCriticalSection(gLevelLock);
end;

finalization
begin
  DoneCriticalSection(gLevelLock);
  BASS_Free;
end;

end.


