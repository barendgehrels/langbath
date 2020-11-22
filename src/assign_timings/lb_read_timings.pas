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

procedure ReadTargetIntoListView(listView : TListView; const filename : string);
procedure ReadTranslationIntoListView(listView : TListView; const filename : string);
procedure ReadTimingsIntoListView(listView : TListView; const filename : string);

implementation

uses lb_lib, lb_ui_lib;

const KTimeSeparator : char = #9;

procedure ReadTargetIntoListView(listView : TListView; const filename : string);
var list : TStringList;
  k : integer;
  it : TListItem;
  sentence : string;
begin
  if not FileExists(filename) then exit;

  listView.Items.BeginUpdate;
  list := TStringList.Create;

  try
    list.LoadFromFile(filename);

    listView.Items.Clear;
    for k := 0 to list.count - 1 do
    begin
      sentence := list[k];
      it := listView.Items.Add;
      it.Caption := ''; // begin time
      it.SubItems.add(''); // end time
      it.SubItems.add(sentence);
    end;

  finally
    listView.Items.EndUpdate;
    list.free;
  end;
end;

procedure ReadTranslationIntoListView(listView : TListView; const filename : string);
var list : TStringList;
  k : integer;
  it : TListItem;
begin
  if not FileExists(filename) then exit;

  listView.Items.BeginUpdate;
  list := TStringList.Create;

  try
    list.LoadFromFile(filename);

    for k := 0 to list.count - 1 do
    begin
      if k < listView.Items.count then it := listView.Items[k] else it := nil;
      if it <> nil then
      begin
        if it.SubItems.Count > 2 then
        begin
          it.SubItems[2] := list[k];
        end
        else
        begin
          it.SubItems.Add(list[k]);
        end;
      end;
    end;

  finally
    listView.Items.EndUpdate;
    list.free;
  end;
end;

procedure ReadTimingsIntoListView(listView : TListView; const filename : string);
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
          AddMinimal(item, 1);
          item.Caption := trim(fields[0]);
          item.SubItems[0] := trim(fields[1]);
          item.Checked := (item.Caption <> '') and (item.SubItems[0] <> '');
          if length(fields) >= 3 then
          begin
            AddMinimal(item, 4);
            item.SubItems[3] := trim(fields[2]);
          end;
        end;
      end;
    end;

  finally
    listView.Items.EndUpdate;
    list.free;
  end;
end;

end.

