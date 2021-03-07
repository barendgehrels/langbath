// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit reads contents from a file and writes it into a listview

unit lb_read_timings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

procedure ReadTargetIntoListView(listView : TListView;
    const filename : string; subItemIndex : integer);
procedure ReadTranslationIntoListView(listView : TListView;
    const filename : string; subItemIndex : integer);
procedure ReadTimingsIntoListView(listView : TListView;
    const filename : string; ratingSubItemIndex : integer);

implementation

uses lb_lib, lb_ui_lib;

const KTimeSeparator : char = #9;

procedure ReadFileIntoListView(listView : TListView;
    const filename : string; subItemIndex : integer; clear : boolean);
var list : TStringList;
  k : integer;
  item : TListItem;
begin
  if not FileExists(filename) then exit;

  listView.Items.BeginUpdate;
  list := TStringList.Create;

  try
    list.LoadFromFile(filename);

    if clear then listView.Items.Clear;

    for k := 0 to list.count - 1 do
    begin
      if k >= listView.Items.Count then item := listView.Items.Add
      else item := listView.Items[k];

      SetMinimumSubItems(item, subItemIndex + 1);
      item.SubItems[subItemIndex] := list[k];
    end;

  finally
    listView.Items.EndUpdate;
    list.free;
  end;
end;


procedure ReadTargetIntoListView(listView : TListView;
    const filename : string; subItemIndex : integer);
begin
  ReadFileIntoListView(listView, filename, subItemIndex, true);
end;

procedure ReadTranslationIntoListView(listView : TListView;
    const filename : string; subItemIndex : integer);
begin
  ReadFileIntoListView(listView, filename, subItemIndex, false);
end;

procedure ReadTimingsIntoListView(listView : TListView;
    const filename : string; ratingSubItemIndex : integer);
var list : TStringList;
  k : integer;
  item : TListItem;
  fields : TArrayOfString;
begin
  if not FileExists(filename) then exit;

  listView.Items.BeginUpdate;
  list := TStringList.Create;

  try
    list.LoadFromFile(filename);

    for k := 0 to list.count - 1 do
    begin
      if k < ListView.Items.count then item := ListView.Items[k] else item := nil;
      if (item <> nil) and (list[k] <> '') then
      begin
        item.Checked := false;

        fields := SplitString(list[k], KTimeSeparator);

        if length(fields) >= 2 then
        begin
          SetMinimumSubItems(item, 1);

          item.Caption := trim(fields[0]);
          item.SubItems[0] := trim(fields[1]);

          item.Checked := (item.Caption <> '') and (item.SubItems[0] <> '');
          if length(fields) >= 3 then
          begin
            SetMinimumSubItems(item, ratingSubItemIndex + 1);
            item.SubItems[ratingSubItemIndex] := trim(fields[2]);
          end;
        end;
      end;
    end;

    while RemoveItemIfEmpty(listView) do ;

  finally
    listView.Items.EndUpdate;
    list.free;
  end;
end;

end.

