// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Analyze a textfile (book)
//   - it splits the text into sentences (TODO, now it expects a pre split)
//   - it counts the number of sentences
//   - it counts the average number of words per sentence
//   - it counts the number of words, and the number of unique words
//   - that ratio is reported as "uniqueness"
// (TODO)
//   - it optionally can use a list of words, ordered by frequency of usage
//   - it optionally can use a list of names, to exclude them from frequency analysis
//   - with these, it counts the average frequency per sentence

unit lb_analyse_textfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure AnalyzeTextfile(const inputFilename : string);

implementation

uses LazUtf8;

function CreateBagOfWords(const s : string) : TStringList;
begin
  result := TStringList.Create;
  result.Delimiter := ' ';
  result.StrictDelimiter := True;
  result.DelimitedText := s;
end;

function CleanString(const s : string) : string;
const punctuations : array of string = ('.', ';', ':', ',', '!', '?', '»', '«', '—', '"', '''');
var i : integer;
begin
  result := s;
  for i := low(punctuations) to high(punctuations) do
  begin
    result := UTF8StringReplace(result, punctuations[i], '', [rfReplaceAll]);
  end;

  // Don't replace dashes in e.g. 'кто-то'
  result := UTF8StringReplace(result, ' - ', ' ', [rfReplaceAll]);
  result := UTF8StringReplace(result, '- ', ' ', [rfReplaceAll]);
  result := trim(UTF8StringReplace(result, ' -', ' ', [rfReplaceAll]));
end;


procedure AnalyzeTextfile(const inputFilename : string);
var list, bag, words, unique : TStringList;
  i, j : integer;
  s : string;
  totalWordCount, totalSentenceCount : integer;
begin
  if not FileExists(inputFilename) then
  begin
    exit;
  end;

  totalWordCount := 0;
  totalSentenceCount := 0;

  list := TStringList.Create;
  words := TStringList.Create;
  unique := TStringList.Create;
  try
    list.LoadFromFile(inputFileName);

    for i := 0 to list.Count - 1 do
    begin
      if copy(list[i], 1, 1) <> '#' then
      begin
        s := CleanString(list[i]);
        bag := CreateBagOfWords(s);
        try
          for j := 0 to bag.Count - 1 do
          begin
            s := trim(bag[j]);
            if s <> '' then
            begin
              inc(totalWordCount);
              words.Append(UTF8LowerString(s));
            end;
          end;
          inc(totalSentenceCount);
        finally
          bag.free;
        end;
      end;
    end;

    unique.Sorted := true;
    unique.Duplicates := dupIgnore;
    unique.AddStrings(words);

    //words.SaveToFile('words.txt');
    //unique.SaveToFile('unique.txt');

    writeln(ExtractFileName(inputFilename));
    writeln('Number of sentences     : ' + inttostr(totalSentenceCount));
    writeln('Words per sentence (avg): ' + floattostr(totalWordCount / totalSentenceCount));
    writeln('Word uniqueness         : ' + floattostr(words.count / unique.count));
    writeln;

  finally
    list.Free;
    words.free;
    unique.free;
  end;

end;

end.

