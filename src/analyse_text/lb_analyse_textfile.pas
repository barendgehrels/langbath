// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Analyze a textfile (book)
//   - it splits the text into sentences (TODO, now it expects a pre split)
//   - it counts the number of sentences
//   - it counts the average number of words per sentence
//   - it optionally can use a list of words, ordered by frequency of usage
//   - it optionally can use a list of names, to exclude them from frequency analysis
//   - with these, it counts the average "rank" of a book, giving some indication about the easiness

// See also about sentence length:
// https://www.reddit.com/r/writing/comments/gpx9me/an_analysis_of_sentence_length/
// Where of several books one paragraph is analyzed, results:
//   The Hero of Ages / Brandon Sanderson / avg: 14.5 / min: 10 / max: 27
//   Neuromancer / William Gibson         / avg: 21.3 / min: 12 / max: 27
//   Fallen Dragon / Peter Hamilton       / avg: 11.4 / min: 6 / max: 18
//   Eisenhorn / Dan Abnett               / avg: 12 / min: 2 / max: 22
//   Name of the Wind / Patrick Rothfuss  / avg: 12 / min: 6 / max: 16
//   Rainbow Six / Tom Clancy             / avg: 17 / min: 4 / max: 27
//   Altered Carbon / Richard Morgan      / avg: 19 / min: 8 / max: 25
// For a whole book, the min does not say so much - but the max might do.

unit lb_analyse_textfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure AnalyzeTextfile(const inputFilename, frequencyFilename, nameFilename, commonFilename : string);
function Preprocess(const s : string) : string;

implementation

uses LazUtf8, lb_lib, lb_frequency_list, lb_split_string_into_sentences, lb_string_counter,
  lb_frequency_counter;

//{$define WITH_DEBUG}

function SourceToString(const f : TFrequencyEntry) : string;
begin
  result := '?';
  case f.source of
    1 : result := 'Brown';
    2 : result := 'common';
    3 : result := 'names';
  end;
end;

function CreateBagOfWords(const s : string) : TStringList;
begin
  result := TStringList.Create;
  result.Delimiter := ' ';
  result.StrictDelimiter := True;
  result.DelimitedText := s;
end;

function Preprocess(const s : string) : string;
// This code was first implemented with 7 UTF8StringReplace statements
// but that takes several minutes on huge strings (e.g. 15000 sentences)
// Therefore it is rewritten below in a more specific way, which runs in 2 seconds instead.
var
  currentPosition, endPosition: PChar;
  i, codePointSize: Integer;
  codePoint: string;
  queue : array of string;
  skip : boolean;
begin
  queue := [];
  result := '';
  currentPosition := PChar(s);
  endPosition := currentPosition + length(s);
  while currentPosition < endPosition do
  begin
    codePointSize := UTF8CodepointSize(currentPosition);
    codePoint := '';
    SetLength(codePoint, codePointSize);
    Move(currentPosition^, codePoint[1], codePointSize);

    // Make replacements of CR, tab, formfeed
    if (codePoint = #13)
    or (codePoint = #10)
    or (codePoint = #12)
    or (codePoint = #9) then codePoint := ' ';

    // Replace an ellipsis just with one dot
    if codePoint = '…' then codePoint := '.';

    // Add to the queue, but avoid adding "  " or ".."
    skip := ((codePoint = ' ') or (codePoint = '.'))
      and (length(queue) > 0) and (queue[length(queue) - 1] = codePoint);

    // Add current character to the queue
    if not skip then
    begin
      SetLength(queue, length(queue) + 1);
      queue[length(queue) - 1] := codePoint;
    end;

    // Add start of the queue to the result, keep 3 entries
    while length(queue) > 3 do
    begin
      result := result + queue[0];
      delete(queue, 0, 1);
    end;

    // Check 3 entries
    // For dialogs (: « or : " or : - or similar),
    // force a sentence split (maybe move this to the split functionality)
    if (length(queue) = 3)
    and (queue[0] = ':')
    and (queue[1] = ' ')
    and ((queue[2] = '«')
          or (queue[2] = '-')
          or (queue[2] = '—')
          or (queue[2] = '–')
          or (queue[2] = '"'))
    then queue[0] := '.';

    inc(currentPosition, codePointSize);
  end;

  for i := low(queue) to high(queue) do result := result + queue[i];
end;

function CleanString(const s : string) : string;
const punctuations : array of string = ('.', ';', ':', ',', '!', '?', '»', '«',
  '#', '"', '''', '|', '„', '“',
  '(', ')', '[', ']');
var i : integer;
begin
  result := s;
  for i := low(punctuations) to high(punctuations) do
  begin
    result := UTF8StringReplace(result, punctuations[i], '', [rfReplaceAll]);
  end;
end;

function NormalizeWord(const s : string) : string;
const punctuations : array of string = ('—', '–', '-');
var i : integer;
begin
  result := UTF8LowerString(s);
  for i := low(punctuations) to high(punctuations) do
  begin
    result := UTF8StringReplace(result, punctuations[i], '', [rfReplaceAll]);
  end;
end;

function GetUnique(list : TStringList) : TStringList;
begin
  result := TStringList.Create;
  result.Sorted := true;
  result.Duplicates := dupIgnore;
  result.AddStrings(list);
end;

procedure WriteUnique(list : TStringList; const filename : string);
var unique : TStringList;
begin
  unique := GetUnique(list);
  try
    unique.SaveToFile(filename);
  finally
    unique.free;
  end;
end;

procedure AnalyzeTextfile(const inputFilename, frequencyFilename, nameFilename, commonFilename : string);
var list, splitted, bag : TStringList;
  i, j : integer;
  s, sentence, term, lowerCaseTerm, base : string;
  totalWordCount, totalSentenceCount : integer;
  isLowerCase : boolean;
  freqList, f2 : TArrayOfFrequencyEntry;

  index, rank, sumrank, countrank : integer;
  wordsNotFound, namesNotFound : TStringList;

  stringCounter : TStringCounter;
  frequencyCounter1 : TFrequencyCounter;
  frequencyCounter2 : TFrequencyCounter;

  {$ifdef WITH_DEBUG}
  debugList : TStringList;
  {$endif}

begin
  if not FileExists(inputFilename)
  or ((frequencyFilename <> '') and not FileExists(frequencyFilename))
  or ((nameFilename <> '') and not FileExists(nameFilename))
  then
  begin
    exit;
  end;

  {$ifdef WITH_DEBUG}
  debugList := TStringList.Create;
  {$endif}

  sumrank := 0;
  countrank := 0;

  frequencyCounter1 := TFrequencyCounter.Create(10, 500, 10000);
  frequencyCounter2 := TFrequencyCounter.Create([2500,5000,7500,10000]);
  stringCounter := TStringCounter.create;

  freqList := ReadFrequencyList(frequencyFilename);

  // All common currently gets "100"
  f2 := GetOtherFrequencyList(commonFileName, 100, 2);
  AppendList(freqList, f2);

  // All names currently get rank "1" because they are easy to read
  f2 := GetOtherFrequencyList(nameFileName, 1, 3);
  AppendList(freqList, f2);

  SortByEntryAndRank(freqList);

  //for i := 0 to 1000 do
  //begin
  //  debugList.Add(format('FIRST entries : %d, %d, "%s"', [i, freqList[i].rank, freqList[i].entry]));
  //end;
  //
  totalWordCount := 0;
  totalSentenceCount := 0;

  list := TStringList.Create;

  wordsNotFound := TStringList.Create;
  namesNotFound := TStringList.Create;
//
//  writeln('Goud: ', FindByEntry(freqList, 'золото'));
//  writeln('Kwam1: ', FindByEntry(freqList, 'шёл'));
//  writeln('Kwam2: ', FindByEntry(freqList, 'шел'));
//  writeln('My: ', FindByEntry(freqList, 'мне'));
//  writeln('My in common: ', commonList.IndexOf('мне'));
//  index := FindByEntry(freqList, 'а');
//  writeln('And: ', index);
//  if index >= 0 then
//  begin
//    debugList.Add(format('FIRST Found "and" in Brown: %d, %d, "%s"', [index, freqList[index].rank, freqList[index].entry]));
//  end;
//
  try
    list.LoadFromFile(inputFileName);
    s := Preprocess(list.text);
    splitted := SplitStringIntoSentences(s);
  finally
    list.free;
  end;

  try
    for i := 0 to splitted.Count - 1 do
    begin
      if copy(splitted[i], 1, 1) <> '#' then
      begin
        sentence := CleanString(splitted[i]);
        {$ifdef WITH_DEBUG}
        debugList.Add(format('Process "%s"', [sentence]));
        {$endif}
        bag := CreateBagOfWords(sentence);
        try
          for j := 0 to bag.Count - 1 do
          begin
            term := trim(bag[j]);
            lowerCaseTerm := NormalizeWord(term);
            if (lowerCaseTerm <> '') and ((lowerCaseTerm[1] < '0') or (lowerCaseTerm[1] > '9')) then
            begin
              inc(totalWordCount);

              isLowerCase := lowerCaseTerm = term;

              rank := 0;
              index := FindByEntry(freqList, lowerCaseTerm);
              if index >= 0 then
              begin
                // 1: Found in frequency list
                rank := freqList[index].rank;
                stringCounter.AddOccurence(freqList[index].mainEntry);
                {$ifdef WITH_DEBUG}
                debugList.Add(format('- Found in %s: %d "%s", "%s"', [SourceToString(freqList[index]), rank, lowerCaseTerm, term]));
                {$endif}
              end
              else
              begin
                if (j > 0) and not isLowerCase then
                begin
                  // 4: Not found, but it has upper case, is not the first word
                  // in a sentence, and therefore it might be a name
                  namesNotFound.add(lowerCaseTerm);
                  {$ifdef WITH_DEBUG}
                  debugList.Add(format('- Add to names-not-found: %d, "%s", "%s"', [rank, lowerCaseTerm, term]));
                  {$endif}
                end
                else
                begin

                  // Not found at all
                  wordsNotFound.add(lowerCaseTerm);
                  //stringCounter.AddOccurence(lowerCaseTerm);
                  {$ifdef WITH_DEBUG}
                  debugList.Add(format('- Not found: "%s", "%s" in "%s"', [lowerCaseTerm, term, sentence]));
                  {$endif}
                end;
              end;

              if rank > 0 then
              begin
                inc(sumrank, rank);
                inc(countrank);
                //wordsFound.add(lowerCaseTerm);

                frequencyCounter1.Add(rank);
                frequencyCounter2.Add(rank);
              end;
            end;
          end;
          inc(totalSentenceCount);
        finally
          bag.free;
        end;
      end;
    end;

    base := ExtractFileName(ChangeFileExt(inputFilename, ''));
    WriteUnique(wordsNotFound, base + '_words_not_found.txt');
    WriteUnique(namesNotFound, base + '_names_not_found.txt');
    {$ifdef WITH_DEBUG}
    debugList.SaveToFile(base  + '_debug.txt');
    {$endif}

    stringCounter.Report(base + '_occurences.txt');

    writeln('------------------------------');
    writeln(base);
    writeln('Number of sentences     : ' + inttostr(totalSentenceCount));
    writeln('Words per sentence (avg): ' + floattostr(totalWordCount / totalSentenceCount));
    if countrank > 0 then writeln('Average rank            : ' + floattostr(sumrank / countrank));
    writeln('Words found             : ' + inttostr(round(100 * countrank / totalWordCount)) + ' %');

    frequencyCounter1.ReportFrom(70, 10);
    frequencyCounter2.ReportAll;

    writeln;

  finally

    frequencyCounter1.free;
    frequencyCounter2.free;
    stringCounter.free;
    splitted.free;

    wordsNotFound.free;
    namesNotFound.free;
    {$ifdef WITH_DEBUG}
    debugList.free;
    {$endif}
  end;



end;

//procedure test1;
//var v : TFieldRenameList;
//  e : TFrequencyEntry;
//begin
//  Initialize(e);
//  v := TFieldRenameList.create;
//  v.Add(e);
//  FreeAndNil(v);
//end;
//

end.

