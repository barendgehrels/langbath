// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Readability measurements:
// - Automated Readability Index (wikipedia)
// - Coleman-Liau_index (wikipedia)
// - LIX (wikipedia)
// - Text Readability Index (own adaptation of Automated Readability Index)

unit lb_readability_measurements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TReadabilityMeasurements = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSentence(wordCount : integer);
    procedure AddWord(const s : string);

    function WordCount : integer;

    function GetCharPerWordHistogram(len : integer) : double;
    function GetWordPerSentenceHistogram(len : integer) : double;

    function TextReadabilityIndex : double;
    function AutomatedReadabilityIndex : double;
    function ColemanLiauIndex : double;
    function LIX : double;
  private
    countCharacters : integer;
    countWords : integer;
    countLongWords : integer;
    wordHistogram : array of integer;
    sentenceHistogram : array of integer;
    countPowerCharacters : double;
    countSentences : integer;
    allWords : TStringList;
  end;

implementation

uses LazUtf8, Math;

constructor TReadabilityMeasurements.Create;
begin
  countCharacters := 0;
  countWords := 0;
  countLongWords := 0;
  countSentences := 0;
  setLength(wordHistogram, 30);
  setLength(sentenceHistogram, 30);
  countPowerCharacters := 0;
  allWords := TStringList.Create;
end;

destructor TReadabilityMeasurements.Destroy;
begin
  allWords.free;
  inherited Destroy;
end;

procedure TReadabilityMeasurements.AddSentence(wordCount : integer);
var i : integer;
begin
  inc(countSentences);
  for i := low(sentenceHistogram) to high(sentenceHistogram) do
  begin
    if wordCount > i then inc(sentenceHistogram[i]);
  end;
end;

procedure TReadabilityMeasurements.AddWord(const s: string);
var i, len : integer;
begin
  allWords.Add(s);
  len := Utf8Length(s);
  inc(countWords);
  inc(countCharacters, len);
  if len > 6 then
  begin
    inc(countLongWords);
  end;
  for i := low(wordHistogram) to high(wordHistogram) do
  begin
    if len > i then inc(wordHistogram[i]);
  end;
  countPowerCharacters := countPowerCharacters + Math.Power(len, 1.5);
end;

function TReadabilityMeasurements.WordCount: integer;
begin
  result := countWords;
end;

function TReadabilityMeasurements.GetCharPerWordHistogram(len: integer): double;
begin
  result := wordHistogram[len] / countWords;
end;
function TReadabilityMeasurements.GetWordPerSentenceHistogram(len: integer): double;
begin
  result := sentenceHistogram[len] / countSentences;
end;

function TReadabilityMeasurements.TextReadabilityIndex: double;
const
  // TODO: finetune constants
  wcpw = 3;
  wwps = 6;
  wc8 = 6;
  wc10 = 5;
  wc12 = 4;

  ww10 = 3;
  ww15 = 4;
  ww20 = 6;
  ww25 = 7;
  wt = wcpw + wwps + wc8 + wc10 + wc12 + ww10 + ww15 + ww20 + ww25;

var c, w, s : double;
begin
  c := countCharacters;
  w := countWords;
  s := countSentences;

  result := 100.0 *  ((wcpw * (1 - w / c) // xxxx -> 1/4 -> 0.75, xxxxxxxxxx -> 1/10 -> 0.9
    + wwps * (1 - s / w) // ~0.25 for 4 words per sentence
    + wc8 * GetCharPerWordHistogram(8)
    + wc10 * GetCharPerWordHistogram(10)
    + wc12 * GetCharPerWordHistogram(12)
    + ww10 * GetWordPerSentenceHistogram(10)
    + ww15 * GetWordPerSentenceHistogram(15)
    + ww20 * GetWordPerSentenceHistogram(20)
    + ww25 * GetWordPerSentenceHistogram(25)
    ) / wt);

  // Let the result have the same scale as the AutomatedReadabilityIndex
  result := result / 1.5 - 10;
end;

function TReadabilityMeasurements.AutomatedReadabilityIndex : double;
var c, w, s : double;
begin
  // See https://en.wikipedia.org/wiki/Automated_readability_index
  c := countCharacters;
  w := countWords;
  s := countSentences;
  result := 4.71 * (c / w) + 0.5 * (w / s) - 21.43;
end;

function TReadabilityMeasurements.ColemanLiauIndex : double;
var c, w, cs, LF, SF : double;
begin
  // See https://en.wikipedia.org/wiki/Coleman%E2%80%93Liau_index
  c := countCharacters;
  w := countWords;
  cs := countSentences;
  LF := (c / w) * 100;
  SF := (cs / w) * 100;
  result := 0.0588 * LF - 0.296 * SF - 15.8;
end;

function TReadabilityMeasurements.LIX: double;
var A, B, C : double;
begin
  // See https://ru.wikipedia.org/wiki/LIX
  A := countWords;
  B := countSentences;
  C := countLongWords;
  result := A / B + (C * 100) / A;

  (*
  < 30 	Очень простые тексты, детская литература
  30-40 Простые тексты, художественная литература, газетные статьи
  40-50 Тексты средней сложности, журнальные статьи
  50-60 Сложные тексты, научно-популярные статьи, профессиональная литература, официальные тексты
  > 60  Очень сложные тексты, написанные канцелярским языком, законы
  *)
end;


end.

