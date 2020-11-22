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

  TLbBass = class

    iChannel : HSTREAM;
    iFileLoaded : boolean;
    iPaused : boolean;
    iTimer : TTimer;

  private
    procedure StopTimer(Sender: TObject);

  public
    constructor Create(const filename : string);
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

  end;

procedure InitBass(win : TBassOwner);
procedure EndBass;

implementation

uses lb_lib;

var gInitialized : boolean;

procedure InitBass(win: TBassOwner);
begin
  // check the correct BASS was loaded
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
  begin
    Log('An incorrect version of BASS.DLL was loaded');
    exit;
  end;

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

constructor TLbBass.Create(const filename : string);
begin
  if gInitialized and FileExists(filename) then
  begin
    iChannel := BASS_StreamCreateFile(False, PChar(filename), 0, 0, BASS_SPEAKER_FRONT);
    iTimer := TTimer.Create(nil);
    iTimer.Enabled := false;
    iTimer.OnTimer := @StopTimer;

    iFileLoaded := true;
  end;
end;

destructor TLbBass.Destroy;
begin
  BASS_StreamFree(iChannel);
  iTimer.Free;
  inherited Destroy;
end;


procedure TLbBass.PlayFromStart;
begin
  if iFileLoaded then
  begin
    BASS_ChannelPlay(iChannel, False);
  end;
end;

procedure TLbBass.PlayFromPosition(posSeconds : double);
var p : QWORD;
begin
  if iFileLoaded then
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
  iTimer.Enabled := false;
  Stop;
end;

procedure TLbBass.PlaySelection(timeBeginSeconds, timeEndSeconds : double);
var p : QWORD;
begin
  if iFileLoaded and (timeEndSeconds > timeBeginSeconds) then
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
  if iFileLoaded then
  begin
    iTimer.Enabled := false;
    BASS_ChannelStop(iChannel);
  end;
end;

procedure TLbBass.PauseMusic;
begin
  if iFileLoaded then
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
  result := iFileLoaded and (BASS_ChannelIsActive(iChannel) = 1);
end;

function TLbBass.CurrentLevel(period : double) : double;
var level : single;
begin
  result := 0;
  if Active and (period < 1.0) then
  begin
    //result := BASS_ChannelGetLevel(iChannel);
    BASS_ChannelGetLevelEx(iChannel, @level, period, BASS_LEVEL_MONO or BASS_LEVEL_RMS);
    result := level;
  end;
end;

end.

