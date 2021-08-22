// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program analyse_text;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  lb_analyse_textfile, lb_frequency_list, lb_string_counter, lb_frequency_counter;

function GetParam(index : integer; out filename : string) : boolean;
begin
  filename := '';
  result := true;

  if ParamCount >= index then
  begin
    filename := paramstr(index);
    if not FileExists(filename) then
    begin
      result := false;
      writeln('File not found: ' + filename);
    end;
  end;
end;

var error : boolean;
  frequencyFilename, nameFilename, commonFilename : string;
begin
  // TODO move to unit test
  //if Preprocess('баа' + #9 + 'р  ен..д') = 'баа р ен.д' then writeln('OK') else writeln('ERROR');
  //exit;

  if ParamCount < 1 then
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
  if not GetParam(2, frequencyFileName)
  or not GetParam(3, nameFilename)
  or not GetParam(4, commonFilename)
  then
  begin
    error := true;
  end;
  if not error then
  begin
    AnalyzeTextfile(ParamStr(1), frequencyFilename, nameFilename, commonFilename);
  end;
end.

