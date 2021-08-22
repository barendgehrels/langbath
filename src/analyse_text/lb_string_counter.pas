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

  { TStringCounter }

  TStringCounter = class
  private
    list : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOccurence(const s : string);
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

  TMyEntry = record
    count : integer;
    entry : string;
  end;

  TCompareByCountDesc = class
    function Less(const a, b : TMyEntry) : boolean;
  end;

function TCompareByCountDesc.Less(const a, b : TMyEntry) : boolean;
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

procedure TStringCounter.Report(const filename : string);
  function countof(index : integer) : Integer;
  var o : TMyObject;
  begin
    o := list.Objects[index] as TMyObject;
    result := o.count;
  end;

var i : integer;
  olist : TStringList;
  ar : array of TMyEntry;
begin
  // Convert stringlist to sort it
  ar := [];
  SetLength(ar, list.count);
  for i := 0 to list.count - 1 do
  begin
    ar[i].entry := list[i];
    ar[i].count := countof(i);
  end;

  specialize CallQuickSort<TMyEntry, TCompareByCountDesc>(ar);

  olist := TStringList.Create;
  try
    i := low(ar);
    while (i <= high(ar)) and (ar[i].count >= 5) do
    begin
      olist.add(format('%s : count %d', [ar[i].entry, ar[i].count]));
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

