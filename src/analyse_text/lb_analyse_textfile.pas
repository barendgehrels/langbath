// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Analyze a textfile (book)
//   - it splits the text into sentences (TODO, now it expects a pre split)
//   - it counts the number of sentences
//   - it counts the average number of words per sentence
//   - it lists some readability indices
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

procedure AnalyzeTextfile(const inputFilename, frequencyFilename,
  nameFilename, commonFilename: string);

implementation

uses LazUtf8, lb_frequency_list,
  lb_split_string_into_sentences, lb_lib_string_clean,
  lb_string_counter,
  lb_frequency_counter, lb_readability_measurements;

//{$define WITH_DEBUG}

function SourceToString(const f: TFrequencyEntry): string;
begin
  Result := '?';
  case f.Source of
    1: Result := 'Brown';
    2: Result := 'common';
    3: Result := 'names';
  end;
end;

function CreateBagOfWords(const s: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  Result.StrictDelimiter := True;
  Result.DelimitedText := s;
end;

function GetUnique(list: TStringList): TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  Result.AddStrings(list);
end;

procedure WriteUnique(list: TStringList; const filename: string);
var
  unique: TStringList;
begin
  unique := GetUnique(list);
  try
    unique.SaveToFile(filename);
  finally
    unique.Free;
  end;
end;

procedure AnalyzeTextfile(const inputFilename, frequencyFilename,
  nameFilename, commonFilename: string);
const wordsPerPage = 250;
var
  list, splitted, bag: TStringList;
  i, j: integer;
  s, sentence, term, lowerCaseTerm, base, title: string;
  totalWordCount, totalSentenceCount: integer;
  isLowerCase, isCountingForFrequency: boolean;
  freqList, f2: TArrayOfFrequencyEntry;

  index, rank, sumrank, countrank, maxrank: integer;
  wordsNotFound, namesNotFound: TStringList;

  stringCounter: TStringCounter;
  frequencyCounter1: TFrequencyCounter;
  frequencyCounter2: TFrequencyCounter;
  readabilityMeasurements: TReadabilityMeasurements;

  {$ifdef WITH_DEBUG}
  debugList: TStringList;
  {$endif}

begin
  if not FileExists(inputFilename) or ((frequencyFilename <> '') and
    not FileExists(frequencyFilename)) or ((nameFilename <> '') and not
    FileExists(nameFilename)) then
  begin
    exit;
  end;

  {$ifdef WITH_DEBUG}
  debugList := TStringList.Create;
  {$endif}

  maxRank := 0;
  sumrank := 0;
  countrank := 0;

  freqList := ReadFrequencyList(frequencyFilename, false);

  // All common currently gets "100"
  if commonFileName <> '' then
  begin
    f2 := GetOtherFrequencyList(commonFileName, 100, 2);
    AppendList(freqList, f2);
  end;

  // All names currently get rank "1" because they are easy to read
  if nameFileName <> '' then
  begin
    f2 := GetOtherFrequencyList(nameFileName, 1, 3);
    AppendList(freqList, f2);
  end;

  if length(freqList) > 0 then
  begin
    SortByEntryAndRank(freqList);
    maxrank := freqList[length(freqList)-1].rank;
    //for i := 0 to 1000 do
    //begin
    //  debugList.Add(format('FIRST entries : %d, %d, "%s"', [i, freqList[i].rank, freqList[i].entry]));
    //end;
  end;

  readabilityMeasurements := TReadabilityMeasurements.Create();
  frequencyCounter1 := TFrequencyCounter.Create(10, 500, 10000);
  frequencyCounter2 := TFrequencyCounter.Create([2500, 5000, 7500, 10000]);
  stringCounter := TStringCounter.Create;

  totalWordCount := 0;
  totalSentenceCount := 0;

  list := TStringList.Create;

  wordsNotFound := TStringList.Create;
  namesNotFound := TStringList.Create;

  try
    list.LoadFromFile(inputFileName);
    s := CleanText(list.Text);
    splitted := SplitStringIntoSentences(s);
  finally
    list.Free;
  end;

  try
    for i := 0 to splitted.Count - 1 do
    begin
      if not splitted[i].StartsWith('#') then
      begin
        sentence := RemovePunctuations(splitted[i]);
        {$ifdef WITH_DEBUG}
        debugList.Add(format('Process "%s"', [sentence]));
        {$endif}
        bag := CreateBagOfWords(sentence);
        readabilityMeasurements.AddSentence(bag.count);
        try
          for j := 0 to bag.Count - 1 do
          begin
            term := NormalizeWord(bag[j]);
            if term <> '' then
            begin
              lowerCaseTerm := UTF8LowerCase(term);
              readabilityMeasurements.AddWord(term);
              if (term[1] < '0') or (term[1] > '9') then
              begin
                Inc(totalWordCount);

                isLowerCase := lowerCaseTerm = term;
                isCountingForFrequency := true;

                rank := 0;
                index := FindByEntry(freqList, lowerCaseTerm);
                if index >= 0 then
                begin
                  // 1: Found in frequency list
                  rank := freqList[index].rank;
                  stringCounter.AddOccurence(freqList[index].mainEntry);
                  {$ifdef WITH_DEBUG}
                  debugList.Add(format('- Found in %s: %d "%s", "%s"',
                    [SourceToString(freqList[index]), rank, lowerCaseTerm, term]));
                  {$endif}
                end
                else
                begin
                  if (j > 0) and not isLowerCase then
                  begin
                    // 4: Not found, but it has upper case, is not the first word
                    // in a sentence, and therefore it might be a name
                    namesNotFound.add(lowerCaseTerm);
                    isCountingForFrequency := false;
                    {$ifdef WITH_DEBUG}
                    debugList.Add(format('- Add to names-not-found: %d, "%s", "%s"',
                      [rank, lowerCaseTerm, term]));
                    {$endif}
                  end
                  else
                  begin

                    // Not found at all
                    wordsNotFound.add(lowerCaseTerm);
                    //stringCounter.AddOccurence(lowerCaseTerm);
                    {$ifdef WITH_DEBUG}
                    debugList.Add(format('- Not found: "%s", "%s" in "%s"',
                      [lowerCaseTerm, term, sentence]));
                    {$endif}
                  end;
                end;

                if rank > 0 then
                begin
                  Inc(sumrank, rank);
                  Inc(countrank);
                  //wordsFound.add(lowerCaseTerm);

                  frequencyCounter1.Add(rank);
                  frequencyCounter2.Add(rank);
                end
                else if isCountingForFrequency then
                begin
                  frequencyCounter1.Add(maxrank + 1);
                  frequencyCounter2.Add(maxrank + 1);
                end;
              end;

            end;
          end;
          Inc(totalSentenceCount);
        finally
          bag.Free;
        end;
      end;
    end;

    base := ExtractFileName(ChangeFileExt(inputFilename, ''));
    WriteUnique(wordsNotFound, base + '_words_not_found.txt');
    WriteUnique(namesNotFound, base + '_names_not_found.txt');
    {$ifdef WITH_DEBUG}
    debugList.SaveToFile(base + '_debug.txt');
    {$endif}

    stringCounter.Report(base + '_occurences.txt');

    title := base;
    if false then
    begin
      title := title.PadLeft(36);

      writeln(format('%.36s  %5d %5d %% %7.2f %7.2f %7.2f %7.2f',
        [title,
          round(readabilityMeasurements.wordCount / wordsPerPage),
          round(100 * (countrank / totalWordCount) * frequencyCounter2.FractionOfOneLimit(2500)),
          readabilityMeasurements.TextReadabilityIndex,
          readabilityMeasurements.AutomatedReadabilityIndex,
          readabilityMeasurements.ColemanLiauIndex,
          readabilityMeasurements.LIX]));
    end
    else
    begin
      writeln('------------------------------');
      writeln(base);
      writeln('Number of sentences     : ' + IntToStr(totalSentenceCount));
      writeln(format('Words per sentence (avg): %.2f', [totalWordCount / totalSentenceCount]));
      writeln(format('Text readability index  : %.2f', [readabilityMeasurements.TextReadabilityIndex]));
      writeln(format('Aut. readability index  : %.2f', [readabilityMeasurements.AutomatedReadabilityIndex]));
      writeln(format('Coleman Liau index      : %.2f', [readabilityMeasurements.ColemanLiauIndex]));
      writeln(format('LIX (läsbarhetsindex)   : %.2f', [readabilityMeasurements.LIX]));

      if countrank > 0 then
      begin
        writeln(format('Words found             : %d %%', [round(100 * countrank / totalWordCount)]));
        writeln(format('Average rank            : %.2f', [sumrank / countrank]));
        frequencyCounter1.ReportFrom(70, 10);
        frequencyCounter2.ReportAll;
      end;

      writeln;
    end;

  finally

    readabilityMeasurements.Free;
    frequencyCounter1.Free;
    frequencyCounter2.Free;
    stringCounter.Free;
    splitted.Free;

    wordsNotFound.Free;
    namesNotFound.Free;
    {$ifdef WITH_DEBUG}
    debugList.Free;
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


end.
