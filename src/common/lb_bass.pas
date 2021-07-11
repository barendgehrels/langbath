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
    iLoopSamples : TArrayOfLevel;

    // Extra channel, not for playing but for returning samples.
    // Only opened on request. A request will open file twice.
    iChannelOnlyDecode : HSTREAM;

    // Extra channel for short samples
    iChannelSample : HSTREAM;

    // Variables for samples
    iSample : HSAMPLE;
    iSampleBeginSeconds : double;
    iSampleLengthSeconds : double;

    iPaused : boolean;

    function GetBassChannelInfo(channel : DWORD) : BASS_CHANNELINFO;
    procedure CreateSample(timeBeginSeconds, timeEndSeconds, timeFillStartSeconds : double; flags : DWORD);

  public

    constructor Create(const filename : string; enableDecoding : boolean);
    destructor Destroy; override;

    function GetReport(out pos, loopFraction, level : double) : boolean;
    function CurrentLevel(period : double) : double;

    // Play the whole file
    // (unless pauze/stop is called)
    procedure PlayFromStart;

    // Play the sound from the specified position until the end
    // (unless pauze/stop is called)
    procedure PlayFromPosition(posSeconds : double);

    // Play a selection (begin..end) of the specified soundfile,
    // using a sample
    procedure PlaySelection(timeBeginSeconds, timeEndSeconds : double;
        timeFillStartSeconds : double = 0;
        loop : boolean = false);

    // Stop playback (if active)
    procedure Stop;

    // Return the length of the mp3. Also if position started from a specified position,
    // or a selection is played, the length of the whole mp3 is returned.
    function LengthSeconds : double;

    // Return the current position, relatively to the whole file.
    function GetPositionSeconds : double;

    // Return true if actively playing.
    function Active : boolean;

    function GetLevels(p1, p2 : double) : TArrayOfLevel;
  end;

procedure InitBass(win : TBassOwner);

implementation

uses Math, lb_lib;

const
  // Specifies the time for each level record.
  // A samplePeriod of 20ms looks smooth enough.
  // Other code doesn't depend on this, a period of 10ms is fine too; the graph is more detailed.
  samplePeriodMs : single = 0.02; // milliseconds

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

constructor TLbBass.Create(const filename : string; enableDecoding : boolean);
begin
  iChannel := 0;
  iChannelOnlyDecode := 0;
  iChannelSample := 0;

  if gInitialized and FileExists(filename) then
  begin
    iChannel := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_SPEAKER_FRONT);

    iFileLoaded := iChannel > 0;

    if enableDecoding then
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
  BASS_StreamFree(iChannelSample);
  BASS_SampleFree(iSample);
  inherited Destroy;
end;

procedure TLbBass.PlayFromStart;
begin
  Stop;
  if iFileLoaded then
  begin
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.PlayFromPosition(posSeconds : double);
var p : QWORD;
begin
  Stop;
  if iFileLoaded then
  begin
    p := BASS_ChannelSeconds2Bytes(iChannel, posSeconds);
    LOG('Play from ' + floattostr(posSeconds) + ' = ' + inttostr(p));
    BASS_ChannelSetPosition(iChannel, p, BASS_POS_BYTE);
    BASS_ChannelPlay(iChannel, False);
  end;
end;

// Workaround to get rid of the warning "info is not initialized"
// (it should actually be an out-parameter in BASS)
function TLbBass.GetBassChannelInfo(channel : DWORD) : BASS_CHANNELINFO;
begin
  result.freq := 0;
  fillchar(result, 0, sizeof(result));
  BASS_ChannelGetInfo(channel, result);
end;

procedure TLbBass.CreateSample(timeBeginSeconds, timeEndSeconds, timeFillStartSeconds : double;
    flags : DWORD);
const bufferSize = 10000;
var memo : TMemoryStream;
  b1, b2 : QWORD;
  bytesRead : DWORD;
  buffer : array [1..bufferSize] of byte;
  info: BASS_CHANNELINFO;

begin
  if (CompareValue(iSampleBeginSeconds, timeBeginSeconds) = 0)
  and (CompareValue(iSampleLengthSeconds, timeEndSeconds - timeBeginSeconds) = 0)
  and (iSample > 0) then
  begin
    // No need to create the sample with the same information.
    exit;
  end;

  // Don't sample longer than a minute
  if timeEndSeconds - timeBeginSeconds > 60 then exit;


  BASS_SampleFree(iSample);
  iSample := 0;

  b1 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, timeBeginSeconds);
  b2 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, timeEndSeconds);

  info := GetBassChannelInfo(iChannelOnlyDecode);
  BASS_ChannelSetPosition(iChannelOnlyDecode, b1, BASS_POS_BYTE);

  memo := TMemoryStream.Create;
  try

    // Optionally add a bit of space at the start,
    // because, for example, on Linux, on VirtualBox, the start is often omitted
    Initialize(buffer);
    fillchar(buffer, bufferSize, #0);
    memo.Write(buffer, min(bufferSize, trunc(timeFillStartSeconds * info.freq)));

    while (BASS_ChannelIsActive(iChannelOnlyDecode) = 1) and (b1 < b2) do
    begin
      bytesRead := BASS_ChannelGetData(iChannelOnlyDecode, @buffer, min(bufferSize, b2 - b1));
      memo.Write(buffer, bytesRead);
      inc(b1, bytesRead);
    end;

    iSample := BASS_SampleCreate(memo.size, info.freq, info.chans, 1, flags);
    BASS_SampleSetData(iSample, memo.memory);

    iSampleBeginSeconds := timeBeginSeconds;
    iSampleLengthSeconds := timeEndSeconds - timeBeginSeconds;
  finally
    memo.Free;
  end;

  // TODO: This could be combined with code above too, because now it gets the same data again
  iLoopSamples := GetLevels(timeBeginSeconds, timeEndSeconds);
end;

procedure TLbBass.PlaySelection(timeBeginSeconds, timeEndSeconds : double;
    timeFillStartSeconds : double; loop : boolean);
var flags : DWORD;
begin
  if not iFileLoaded then exit;

  Stop;

  if timeBeginSeconds < 0 then timeBeginSeconds := 0;
  if timeEndSeconds < 0 then timeEndSeconds := LengthSeconds;
  if timeFillStartSeconds > 1 then timeFillStartSeconds := 1.0;
  if loop then flags := BASS_SAMPLE_LOOP else flags := 0;

  if timeEndSeconds > timeBeginSeconds then
  begin
    CreateSample(timeBeginSeconds, timeEndSeconds, timeFillStartSeconds, flags);

    // Get the channel (required for every repeat) and play the channel
    iChannelSample := BASS_SampleGetChannel(iSample, true);
    BASS_ChannelPlay(iChannelSample, true);
  end;
end;

procedure TLbBass.Stop;
begin
  if iFileLoaded then
  begin
    BASS_ChannelStop(iChannel);
    BASS_ChannelStop(iChannelSample);
  end;
end;

function TLbBass.LengthSeconds : double;
begin
  result := 0;
  if iFileLoaded then
  begin
    // Always use iChannel, don't use the length of the sample
    result := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetLength(iChannel, BASS_POS_BYTE));
  end;
end;

function TLbBass.Active : boolean;
begin
  result := iFileLoaded
    and ((BASS_ChannelIsActive(iChannel) = 1)
          or (BASS_ChannelIsActive(iChannelSample) = 1));
end;

function TLbBass.GetPositionSeconds : double;
begin
  result := 0;
  if iFileLoaded then
  begin
    if BASS_ChannelIsActive(iChannel) = 1 then
    begin
      result := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE));
    end
    else if BASS_ChannelIsActive(iChannelSample) = 1 then
    begin
      result := iSampleBeginSeconds
        + BASS_ChannelBytes2Seconds(iChannelSample, BASS_ChannelGetPosition(iChannelSample, BASS_POS_BYTE));
    end;
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
  distance, minDistance : double;
begin
  level := 0;
  pos := 0;
  loopFraction := 0;

  result := true;

  if BASS_ChannelIsActive(iChannel) = 1 then
  begin
    pos := BASS_ChannelBytes2Seconds(iChannel, BASS_ChannelGetPosition(iChannel, BASS_POS_BYTE));
  end
  else if BASS_ChannelIsActive(iChannelSample) = 1 then
  begin
    pos := BASS_ChannelBytes2Seconds(iChannelSample, BASS_ChannelGetPosition(iChannelSample, BASS_POS_BYTE));
    loopFraction := pos / iSampleLengthSeconds;
    pos := iSampleBeginSeconds + pos;
  end
  else
  begin
    result := false;
    exit;
  end;

  // Use the samples (decoded in the alternative channel) to report the current level.
  // (TODO: only if the sentences are short, to avoid too long loops).
  if length(iLoopSamples) > 0 then
  begin
    minDistance := MaxDouble;
    for i := low(iLoopSamples) to high(iLoopSamples) do
    begin
      distance := abs(iLoopSamples[i].positionSeconds - pos);
      if distance < minDistance then
      begin
        minDistance := distance;
        level := iLoopSamples[i].level;
      end;
    end;
  end
  else
  begin
    level := CurrentLevel(samplePeriodMs);
  end;
end;

function TLbBass.GetLevels(p1, p2 : double) : TArrayOfLevel;
const maxBufferSize = 5000;
var b1, b2 : QWORD;
  buffer : array [1..maxBufferSize] of byte;
  info: BASS_CHANNELINFO;
  bytesRead, bytesPerSample : DWORD;
  level : single;
  pos : double;
  n : integer;
begin

  // Don't sample longer than a minute
  if p2 - p1 > 60 then exit;

  result := [];
  n := 0;
  if iFileLoaded and iEnableGetLevels and (iChannelOnlyDecode > 0) then
  begin
    EnterCriticalSection(gLevelLock);
    try
      log('Init sampling');
      b1 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p1);
      b2 := BASS_ChannelSeconds2Bytes(iChannelOnlyDecode, p2);

      info := GetBassChannelInfo(iChannelOnlyDecode);

      bytesPerSample := min(round(samplePeriodMs * info.freq), maxBufferSize);

      log(format('Start sampling %s: %f - %f (%d - %d) period: %f, batch: %d, freq: %d',
        [info.filename, p1, p2, b1, b2, samplePeriodMs, bytesPerSample, info.freq]));

      BASS_ChannelSetPosition(iChannelOnlyDecode, b1, BASS_POS_BYTE);
      while (BASS_ChannelIsActive(iChannelOnlyDecode) = 1) and (b1 < b2) do
      begin
        pos := BASS_ChannelBytes2Seconds(iChannelOnlyDecode,
            BASS_ChannelGetPosition(iChannelOnlyDecode, BASS_POS_BYTE));
        bytesRead := BASS_ChannelGetData(iChannelOnlyDecode, @buffer, min(bytesPerSample, b2 - b1));
        BASS_ChannelGetLevelEx(iChannelOnlyDecode, @level, samplePeriodMs, BASS_LEVEL_MONO);
        inc(b1, bytesRead);

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

