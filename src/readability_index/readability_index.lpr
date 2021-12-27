// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program readability_index;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, LazUtf8,
  lb_argument_parser,
  lb_readability_measurements,
  lb_split_string_into_sentences,
  lb_lib_string_clean,
  lb_string_counter;

type
  TProgramOptions = record
    help : boolean;
    filename : string;
    oneline : boolean;
    csv : boolean;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'filename', true, 'Filename to read', true),
     Option('o', 'oneline', false, 'True if output should be put on one line'),
     Option('c', 'csv', false, 'True if CSV is true')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  try
    result.help := VariantAsBoolean(values[0]);
    result.filename := values[1];
    result.oneline := values[2];
    result.csv := values[3];
  except
    on e : Exception do
    begin
      writeln('Error in parsing argument value');
      initialize(result);
      exit;
    end;
  end;
end;


function CreateBagOfWords(const s: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  Result.StrictDelimiter := True;
  Result.DelimitedText := s;
end;

procedure AnalyzeText(const textString, base : string; csv, oneLine : boolean);
var
  splitted, bag: TStringList;
  rm: TReadabilityMeasurements;

  title, fmt, term, sentence : string;
  i,j : integer;

begin
  splitted := SplitStringIntoSentences(CleanText(textString));

  rm := TReadabilityMeasurements.Create();

  try
    for i := 0 to splitted.Count - 1 do
    begin
      if not splitted[i].StartsWith('#') then
      begin
        sentence := RemovePunctuations(splitted[i]);
        bag := CreateBagOfWords(sentence);
        rm.AddSentence(bag.Count);
        try
          for j := 0 to bag.Count - 1 do
          begin
            term := UTF8Trim(NormalizeWord(bag[j]));
            if term <> '' then
            begin
              rm.AddWord(term);
            end;
          end;
        finally
          bag.Free;
        end;
      end;
    end;

    title := base;
    if oneLine or csv then
    begin
      if csv then fmt := '%s,%6d,%.2f,%.2f,%.2f,%.2f'
      else
      begin
        fmt := '%.36s  %7d %7.2f %7.2f %7.2f %7.2f';
        title := title.PadLeft(36);
      end;

      writeln(format(fmt,
      [title,
        rm.wordcount,
        rm.TextReadabilityIndex,
        rm.AutomatedReadabilityIndex,
        rm.ColemanLiauIndex,
        rm.LIX]));
    end
    else
    begin
      writeln(base);
      writeln(format('Text readability index      : %.2f', [rm.TextReadabilityIndex]));
      writeln(format('Automated readability index : %.2f', [rm.AutomatedReadabilityIndex]));
      writeln(format('Coleman Liau index          : %.2f', [rm.ColemanLiauIndex]));
      writeln(format('LIX (läsbarhetsindex)       : %.2f', [rm.LIX]));
    end;

  finally
    rm.Free;
    splitted.Free;
  end;
end;


procedure AnalyzeTextfile(const inputFilename : string; csv, oneLine : boolean);
var list : TStringList;
  base, textString : string;
begin
  if not FileExists(inputFilename) then
  begin
    exit;
  end;
  list := TStringList.Create;

  try
    list.LoadFromFile(inputFileName);
    textString := CleanText(list.Text);
  finally
    list.Free;
  end;

  base := ExtractFileName(ChangeFileExt(inputFilename, ''));
  AnalyzeText(textString, base, csv, oneLine);
end;

var args : array of TProgramArgument;
  values : array of variant;
  opts : TProgramOptions;

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
  if opts.help or (opts.filename = '') then
  begin
    PrintHelp(args);
    exit;
  end;

  if not FileExists(opts.filename) then
  begin
    writeln('File not found: ' + opts.filename);
    exit;
  end;
  AnalyzeTextfile(opts.filename, opts.csv, opts.oneline);
end.

