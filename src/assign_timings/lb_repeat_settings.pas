unit lb_repeat_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRepeatSettings = record
    showOriginal : boolean;
    showTranslation : boolean;
    playAudio : boolean;
  end;

  TArrayOfRepeatSettings = array of TRepeatSettings;

// Converts repeat settings to a string
function RepeatSettingsAsString(const r : TRepeatSettings) : string;
function RepeatSettingsAsString(const ar : TArrayOfRepeatSettings) : string;

// Converts a string to repeat settings
function StringAsRepeatSettings(const s : string) : TRepeatSettings;
function StringAsArrayOfRepeatSettings(const s : string) : TArrayOfRepeatSettings;


implementation

uses lb_lib;

function RepeatSettingsAsString(const r : TRepeatSettings) : string;
begin
  result := '';
  if r.showOriginal then result := result + 'o';
  if r.showTranslation then result := result + 't';
  if r.playAudio then result := result + 'a';
  // Avoid an empty string
  if result = '' then result := '!';
end;

function RepeatSettingsAsString(const ar : TArrayOfRepeatSettings) : string;
var i : integer;
begin
  result := '';
  for i := low(ar) to high(ar) do
  begin
    if i > low(ar) then result := result + '-';
    result := result + RepeatSettingsAsString(ar[i]);
  end;
end;

function StringAsRepeatSettings(const s : string) : TRepeatSettings;
begin
  if s = '' then
  begin
    result.ShowOriginal := true;
    result.showTranslation := true;
    result.playAudio := true;
  end
  else
  begin
    result.showOriginal := pos('o', s) > 0;
    result.showTranslation  := pos('t', s) > 0;
    result.playAudio := pos('a', s) > 0;
  end;
end;

function StringAsArrayOfRepeatSettings(const s : string) : TArrayOfRepeatSettings;
var ar : TArrayOfString;
  i : integer;
begin
  ar := SplitString(s, '-');

  result := [];
  SetLength(result, length(ar));
  for i := low(ar) to high(ar) do
  begin
    result[i] := StringAsRepeatSettings(ar[i]);
  end;
end;

end.

