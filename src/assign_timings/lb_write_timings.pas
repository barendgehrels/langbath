// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit writes the contents of a listview into a file

unit lb_write_timings;

{$mode objfpc}{$H+}

interface

uses ComCtrls;

// Writes the timings (including rating) from a listview into a file
procedure WriteTimingsFromListView(ListView : TListView; const filename : string);

// Writes a column from a listview into a file
procedure WriteColumnFromListView(ListView : TListView; colIndex : integer; const filename : string);

implementation

uses SysUtils, Classes;

const KTimeSeparator : char = #9;

procedure WriteTimingsFromListView(ListView : TListView; const filename : string);

  function TimeString(const s : string) : string;
  var v : double;
  begin
    result := '';
    if TryStrToFloat(s, v) then
    begin
      result := format('%.3f', [v]);
    end;
  end;

  function TimeItem(item : TListItem) : string;
  begin
    // Store even unchecked timings.
    result := TimeString(item.Caption);
    if item.SubItems.Count > 0 then result := result + KTimeSeparator + TimeString(item.SubItems[0]);
    if item.SubItems.Count > 3 then result := result + KTimeSeparator + item.SubItems[3];
  end;

var list : TStringList;
  i : integer;
begin
  // Creates a tab separated list of begin-end (and rating) and stores it in a file
  // Stored times can be (partly) empty

  list := TStringList.Create;
  try
    for i := 0 to ListView.Items.Count - 1 do
    begin
      list.Append(TimeItem(ListView.Items[i]));
    end;

    list.SaveToFile(filename);
  finally
    list.free;
  end;
end;

procedure WriteColumnFromListView(ListView : TListView; colIndex : integer; const filename : string);
var list : TStringList;
  i : integer;
  item : TListItem;
begin
  list := TStringList.Create;
  try
    for i := 0 to ListView.Items.Count - 1 do
    begin
      item := ListView.Items[i];
      if colIndex < item.SubItems.Count then list.Append(item.SubItems[colIndex])
      else list.Append('');
    end;

    list.SaveToFile(filename);
  finally
    list.free;
  end;
end;

end.

