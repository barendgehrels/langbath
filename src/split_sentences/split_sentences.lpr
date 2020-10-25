// Language Bath
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program split_sentences;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  lb_split_textfile_into_sentences;

var error : boolean;
begin
  if ParamCount <> 2 then
  begin
    writeln('Usage: ' + ParamStr(0) + ' [input textfile] [output textfile]');
    halt;
  end;
  error := false;
  if not FileExists(ParamStr(1)) then
  begin
    error := true;
    writeln('File not found: ' + ParamStr(1));
  end;
  if FileExists(ParamStr(2)) then
  begin
    error := true;
    writeln('File already exists: ' + ParamStr(2));
  end;
  if not error then
  begin
    SplitSentences(ParamStr(1), ParamStr(2));
  end;
end.

