// Language Bath
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program split_sentences;

{$mode objfpc}{$H+}

uses
  Classes,
  lb_split_textfile_into_sentences;


begin
  if ParamCount <> 2 then
  begin
    writeln('Usage: ' + ParamStr(0) + ' [input textfile] [output textfile]');
    halt;
  end;
  SplitSentences(ParamStr(1), ParamStr(2));
end.

