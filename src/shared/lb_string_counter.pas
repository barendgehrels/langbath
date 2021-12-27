// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Contains a "map" or "dictionary" of string -> integer
// Lazarus generics could have been used instead, but it gives loads of warnings.
// Delphi/Lazarus TStrings functionality seems to work equally well for this purpose

// https://stackoverflow.com/questions/8947400/tstringlists-addobject-method

unit lb_string_counter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TEntryAndCount = record
    entry : string;
    count : integer;
  end;

  { TStringCounter }

  TStringCounter = class
  private

    list : TStringList;
    sortedOccurences : array of TEntryAndCount;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOccurence(const s : string);
    procedure PostProcess;

    // Returns the proportion of words occuring <= X occurences w.r.t. the total
    // 1.0 means all words are "rare". 0.0 means there are no rare words, all words
    // occur more than X times.
    function RareWordFraction(occurenceCount : integer) : double;

    function GetOccurence(const s : string) : integer;
    procedure Report(const filename : string);
  end;

procedure selftest;


implementation

uses lb_quicksort;

type
  TMyObject = class(TObject)
    public
    count : integer;
    constructor create;
  end;

  TCompareByCountDesc = class
    function Less(const a, b : TEntryAndCount) : boolean;
  end;

function TCompareByCountDesc.Less(const a, b : TEntryAndCount) : boolean;
begin
  if a.count = b.count then result := a.entry < b.entry else result := a.count > b.count;
end;

constructor TStringCounter.Create;
begin
  inherited create;
  list := TStringList.Create;
  list.sorted := true;
end;

destructor TStringCounter.Destroy;
var i : integer;
begin
  for i := 0 to list.count - 1 do
  begin
    list.objects[i].free;
  end;
  FreeAndNil(list);
  inherited destroy;
end;

procedure TStringCounter.AddOccurence(const s: string);
var index : integer;
  o : TMyObject;
begin
  index := list.IndexOf(s);
  if index >= 0 then
  begin
    o := list.Objects[index] as TMyObject;
    inc(o.count);
  end
  else
  begin
    o := TMyObject.Create;
    inc(o.count);
    list.AddObject(s, o);
  end;
end;

procedure TStringCounter.PostProcess;
  function countof(index : integer) : Integer;
  var o : TMyObject;
  begin
    o := list.Objects[index] as TMyObject;
    result := o.count;
  end;

var i : integer;
begin
  // Convert stringlist to array to sort it
  sortedOccurences := [];
  SetLength(sortedOccurences, list.count);
  for i := 0 to list.count - 1 do
  begin
    sortedOccurences[i].entry := list[i];
    sortedOccurences[i].count := countof(i);
  end;

  specialize CallQuickSort<TEntryAndCount, TCompareByCountDesc>(sortedOccurences);
end;

function TStringCounter.GetOccurence(const s: string): integer;
var i : integer;
begin
  for i := low(sortedOccurences) to high(sortedOccurences) do
  begin
    if sortedOccurences[i].entry = s then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TStringCounter.RareWordFraction(occurenceCount : integer) : double;

  // TODO: move generalized version to lb_quicksort
  function Less(const entry : TEntryAndCount; value : integer) : boolean;
  begin
    // Values are sorted descending
    result := entry.count > value;
  end;
  function Greater(const entry : TEntryAndCount; value : integer) : boolean;
  begin
    result := entry.count < value;
  end;
  function Equal(const entry : TEntryAndCount; value : integer) : boolean;
  begin
    result := entry.count = value;
  end;

  function BinarySearch(const ar : array of TEntryAndCount; value : integer): integer;

  var lo, hi, mid : integer;
  begin
    lo := low(ar);
    hi := high(ar);
    result := -1;

    while lo <= hi do
    begin
      mid := (lo + hi) div 2;
      if Less(ar[mid], value) then lo := mid + 1
      else if Greater(ar[mid], value) then hi := mid - 1
      else
      begin
        repeat
           dec(mid);
        until (mid = 0) or not Equal(ar[mid - 1], value);

        result := mid;
        exit;
      end
    end;
  end;

var index : integer;
begin
  index := BinarySearch(sortedOccurences, occurenceCount);
  // Suppose there are 1000 occurences, sorted from high-frequent to just once
  // The higher the result, the more rare occurences there are
  // If index = 100 it means that all below (900) is low-frequent -> ix/size=0.1 -> result 0.9
  // If index = 700 then there are just 300 low-frequent values -> ix/size=0.7   -> result 0.3
  result := 1.0 - double(index) / double(length(sortedOccurences));
  //writeln(format('    RARE %d of %d -> %f', [index, length(sortedOccurences), result]));
  //for i := low(sortedOccurences) to high(sortedOccurences) do
  //begin
  //  if sortedOccurences[i].count = occurenceCount then
  //  begin
  //    writeln('RANK ' + inttostr(index) + ' I ' + inttostr(i));
  //    result := 1.0 - double(i) / double(length(sortedOccurences));
  //    exit;
  //  end;
  //end;
  //
  //
end;

procedure TStringCounter.Report(const filename : string);
var i : integer;
  olist : TStringList;
begin
  olist := TStringList.Create;
  try
    i := low(sortedOccurences);
    while i <= high(sortedOccurences) do
    begin
      olist.add(format('%s : len: %d count %d',
      [sortedOccurences[i].entry, length(sortedOccurences[i].entry), sortedOccurences[i].count]));
      inc(i);
    end;
    olist.SaveToFile(filename);
  finally
    olist.free;
  end;
end;

constructor TMyObject.create;
begin
  inherited create;
  count := 0;
end;

procedure selftest;
var c : TStringCounter;
begin
  c := TStringCounter.create;
  try

    c.AddOccurence('d');
    c.AddOccurence('a');
    c.AddOccurence('c');
    c.AddOccurence('a');
    c.AddOccurence('d');
    c.AddOccurence('d');

    c.AddOccurence('a');
    c.AddOccurence('a');
    c.AddOccurence('c');

    c.Report('temp.txt');
  finally
    c.free;
  end;
end;

end.

