// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program analyse_text;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  lb_analyse_textfile;

var error : boolean;
begin
  if ParamCount <> 1 then
  begin
    writeln('Usage: ' + ParamStr(0) + ' [input textfile]');
    halt;
  end;
  error := false;
  if not FileExists(ParamStr(1)) then
  begin
    error := true;
    writeln('File not found: ' + ParamStr(1));
  end;
  if not error then
  begin
    AnalyzeTextfile(ParamStr(1));
  end;
end.

