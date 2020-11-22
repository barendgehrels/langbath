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
  inputFilename, outputFilename : string;
begin
  if ParamCount <> 2 then
  begin
    writeln('Usage: ' + ParamStr(0) + ' [input textfile] [output textfile]');
    halt;
  end;

  inputFilename := Paramstr(1);
  outputFilename := Paramstr(2);

  if outputFilename = 'auto' then
  begin
    outputFilename := StringReplace(inputFilename, '.txt', '_splitted.txt', [rfIgnoreCase]);
    {$I-} DeleteFile(outputFilename); {$I+}
  end;

  error := false;
  if not FileExists(inputFilename) then
  begin
    error := true;
    writeln('File not found: ' + inputFilename);
  end;
  if FileExists(outputFilename) then
  begin
    error := true;
    writeln('File already exists: ' + outputFilename);
  end;
  if not error then
  begin
    SplitSentences(inputFilename, outputFilename);
  end;
end.

