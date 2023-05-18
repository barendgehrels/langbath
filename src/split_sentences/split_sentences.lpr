// Language Bath
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// See description in the main unit

program split_sentences;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, lb_argument_parser,
  lb_split_textfile_into_sentences, lb_split_string_into_sentences;


type
  TProgramOptions = record
    help : boolean;
    input_filename : string;
    output_filename : string;
    honorifics : boolean;
    honorifics_filename : string;
  end;


function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('i', 'input', true, 'Input filename of ASCII book'),
     Option('o', 'output', true, 'Output filename of split text or analysis'),
     Option('a', 'analyse', false, 'Analyse honorifics (mr, mrs, prof, dr, sr, srta, etc)'),
     Option('c', 'honorifics', false, 'Honorifics filename')
     ];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  result.help := VariantAsBoolean(values[0]);
  result.input_filename := values[1];
  result.output_filename := values[2];
  result.honorifics := VariantAsBoolean(values[3]);
  result.honorifics_filename := values[4];
end;


var args : array of TProgramArgument;
  values : array of variant;
  opts : TProgramOptions;
  error : boolean;
  autoTerm : string;
begin

  args := GetPossibleArguments;
  if not ParseArguments(args, values) then
  begin
    writeln('Wrong argument(s)');
    PrintValues(args, values);
    PrintHelp(args);
    exit;
  end;

  opts := ArgumentsToRecord(values);
  if opts.help then
  begin
    PrintHelp(args);
    exit;
  end;

  if opts.honorifics then autoTerm := 'honorifics' else autoTerm := 'split';

  if opts.output_filename = 'auto' then
  begin
    opts.output_filename := StringReplace(opts.input_filename, '.txt', '_' + autoTerm + '.txt', [rfIgnoreCase]);
    {$I-} DeleteFile(opts.output_filename); {$I+}
  end;

  if not HasAllRequiredOptions(args, values) then
  begin
    halt;
  end;

  error := false;
  if not FileExists(opts.input_filename) then
  begin
    error := true;
    writeln('File not found: ' + opts.input_filename);
  end;
  if FileExists(opts.output_filename) then
  begin
    error := true;
    writeln('File already exists: ' + opts.output_filename);
  end;
  if not error then
  begin
    SplitSentences(opts.input_filename, opts.output_filename, opts.honorifics_filename, opts.honorifics);
  end;
end.

