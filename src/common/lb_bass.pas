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

  TLbBass = class

    iFileLoaded : boolean;
    iEnableGetLevels : boolean;

    iChannel : HSTREAM;

    // Extra channel, not for playing but for returning samples.
    // Only opened on request. A request will open file twice.
    iChannelOnlyDecode : HSTREAM;

    iPaused : boolean;
    iTimer : TTimer;

  private
    procedure StopTimer(Sender: TObject);
    function IsPlayable : boolean;

  public
    constructor Create(const filename : string; enableGetLevels : boolean);
    destructor Destroy; override;

    function CurrentLevel(period : double) : double;

    // Play the whole file
    // (unless pauze/stop is called)
    procedure PlayFromStart;

    // Play the sound from the specified position until the end
    // (unless pauze/stop is called)
    procedure PlayFromPosition(posSeconds : double);

    // Play a selection (begin..end) of the specified soundfile.
    // Internally it uses a timer to stop playing after the specified time is over.
    procedure PlaySelection(timeBeginSeconds, timeEndSeconds : double);

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

  end;

procedure InitBass(win : TBassOwner);
procedure EndBass;

implementation

uses lb_lib;

var gInitialized : boolean;

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

procedure EndBass;
begin
  BASS_Free;
end;

constructor TLbBass.Create(const filename : string; enableGetLevels : boolean);
begin
  iChannel := 0;
  iChannelOnlyDecode := 0;

  if gInitialized and FileExists(filename) then
  begin
    iChannel := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_SPEAKER_FRONT);
    iTimer := TTimer.Create(nil);
    iTimer.Enabled := false;
    iTimer.OnTimer := @StopTimer;

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

function TLbBass.IsPlayable : boolean;
begin
  result := iFileLoaded and (iTimer <> nil);
end;

procedure TLbBass.PlayFromStart;
begin
  if IsPlayable then
  begin
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.PlayFromPosition(posSeconds : double);
var p : QWORD;
begin
  if IsPlayable then
  begin
    iTimer.Enabled := false;
    p := BASS_ChannelSeconds2Bytes(iChannel, posSeconds);
    LOG('Play from ' + floattostr(posSeconds) + ' = ' + inttostr(p));
    BASS_ChannelSetPosition(iChannel, p, BASS_POS_BYTE);
    BASS_ChannelPlay(iChannel, False);
  end;
end;


procedure TLbBass.StopTimer(Sender: TObject);
begin
  if iTimer <> nil then iTimer.Enabled := false;
  Stop;
end;

procedure TLbBass.PlaySelection(timeBeginSeconds, timeEndSeconds : double);
var p : QWORD;
begin
  if IsPlayable and (timeEndSeconds > timeBeginSeconds) then
  begin
    iTimer.Enabled := false;
    p := BASS_ChannelSeconds2Bytes(iChannel, timeBeginSeconds);
    BASS_ChannelSetPosition(iChannel, p, BASS_POS_BYTE);
    BASS_ChannelPlay(iChannel, False);
    iTimer.Interval := 1 + trunc(1000.0 * (timeEndSeconds - timeBeginSeconds));
    iTimer.Enabled := true;
  end;
end;

procedure TLbBass.Stop;
begin
  if IsPlayable then
  begin
    iTimer.Enabled := false;
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

function TLbBass.GetPositionSeconds : double;
begin
  result := 0;
  if iFileLoaded then
  begin
    result := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE));
  end;
end;

function TLbBass.Active : boolean;
begin
  result := IsPlayable and (BASS_ChannelIsActive(iChannel) = 1);
end;

function TLbBass.CurrentLevel(period : double) : double;
var level : single;
begin
  result := 0;
  if Active and (period < 1.0) then
  begin
    BASS_ChannelGetLevelEx(iChannel, @level, period, BASS_LEVEL_MONO);
    result := level;
  end;
end;

function TLbBass.GetSamples(p1, p2 : double) : TArrayOfLevel;
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
    b1 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p1);
    b2 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p2);

    BASS_ChannelGetInfo(iChannelOnlyDecode, info);

    bytesPerBatch := info.freq div 50; // measures every 10ms=1/100
    if bytesPerBatch >= length(buf) then bytesPerBatch := length(buf) - 1;

    log(format('Start sampling %s: %f - %f (%d - %d) period: %f, batch: %d, freq: %d',
    [info.filename, p1, p2, b1, b2, period, bytesPerBatch, info.freq]));

    BASS_ChannelSetPosition(iChannelOnlyDecode, b1, BASS_POS_BYTE);
    // Get the levels
    while (BASS_ChannelIsActive(iChannelOnlyDecode) > 0) and (b1 < b2) do
    begin
      pos := BASS_ChannelBytes2Seconds(iChannelOnlyDecode, BASS_ChannelGetPosition(iChannelOnlyDecode, BASS_POS_BYTE));
      BASS_ChannelGetData(iChannelOnlyDecode, @buf, bytesPerBatch div 3);
      BASS_ChannelGetLevelEx(iChannelOnlyDecode, @level, period, BASS_LEVEL_MONO);
      inc(b1, bytesPerBatch);
      //log(format('Read: [%d] at %d = %.4f : %.4f', [bytesRead, b1, pos, level]));
      SetLength(result, n + 1);
      result[n].positionSeconds := pos;
      result[n].Level := level;
      inc(n);
    end;
  end;
end;


end.

