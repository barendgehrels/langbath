unit lb_copy_paste;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

function SelectionAsStringList(list : TListView; many : boolean; subItemIndex, maxChars : integer) : TStringList;
procedure PasteList(listView : TListView; list : TStrings; many : boolean; subItemIndex : integer);
procedure CorrectNames(listView : TListView; list : TStrings; index1, index2 : integer);

implementation

uses LazUtf8, lb_ui_lib, lb_replace_names_in_translations;

function SelectionAsStringList(list : TListView; many : boolean; subItemIndex, maxChars : integer) : TStringList;
var item : TListItem;
  i, itemIndex : integer;
  length : integer;
begin
  result := TStringList.Create;
  length := 0;
  item := list.Selected;
  while item <> nil do
  begin
    result.Add(item.SubItems[subItemIndex]);
    itemIndex := item.Index;
    length := length + Utf8Length(item.SubItems[subItemIndex]);
    item := list.GetNextItem(item, sdBelow, [lisSelected]);
  end;
  if many and (subItemIndex = 1) then
  begin
    for i := itemIndex + 1 to list.Items.Count - 1 do
    begin
      item := list.items[i];
      length := length + Utf8Length(item.SubItems[subItemIndex]);
      if length > maxChars then exit;
      result.Add(item.SubItems[subItemIndex]);
    end;
  end;
end;

procedure PasteList(listView : TListView; list : TStrings; many : boolean; subItemIndex : integer);
var item : TListItem;
  itemIndex, i, k : integer;
begin
  item := listView.Selected;
  i := 0;
  while (item <> nil) and (i < list.count) do
  begin
    SetMinimumSubItems(item, subItemIndex + 1);
    item.SubItems[subItemIndex] := list[i];
    itemIndex := item.index;
    item := listView.GetNextItem(item, sdBelow, [lisSelected]);
    inc(i);
  end;
  if (i + 1 < list.count) and many then
  begin
    // Copy the other lines too
    for k := itemIndex + 1 to listView.items.count - 1 do
    begin
      item := listView.Items[k];
      SetMinimumSubItems(item, subItemIndex + 1);
      item.SubItems[subItemIndex] := list[i];
      inc(i);
      if i = list.count then exit;
    end;
  end;
end;

procedure CorrectNames(listView : TListView; list : TStrings; index1, index2 : integer);
var item : TListItem;
  itemIndex, i, j, k, c, count : integer;
  s : string;
  tca : TCorrectedTranslationArray;
  tc : TCorrectedTranslation;
begin
  tca := ReadTranslationCorrections('c:\mdata\languages\russian\books\Nosov\DunnoToTheMoon\DunnoToTheMoon_corrections.txt');

  count := 0;
  item := listView.Selected;
  itemIndex := item.index;
  for k := 0 to list.Count - 1 do
  begin
    item := listView.Items[itemIndex + k];
    for i := 0 to length(tca) - 1 do
    begin
      if Utf8Pos(tca[i].Original, item.subitems[index1]) > 0 then
      begin
        tc := tca[i];
        for j := low(tc.WrongTranslation) to high(tc.WrongTranslation) do
        begin
          s := StringReplace(item.subitems[index2],
            tc.WrongTranslation[j], tc.CorrectedTranslation, [rfReplaceAll], c);
          if c > 0 then
          begin
            item.subitems[index2] := s;
            inc(count, c);
          end;
        end;
      end;
    end;
  end;
  //showmessage(inttostr(count) + ' replaced');
end;


end.

