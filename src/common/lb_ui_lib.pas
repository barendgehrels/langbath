// Language Bath - Assign Timings
// Copyright (c) 2020-2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit contains common functionality for ui elements

unit lb_ui_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

// Take care a list item has a minimum of <count> subitems
procedure SetMinimumSubItems(item : TListItem; count : integer);

// Moves items in range [low, high] one up (so keep low > 0, because low-1 gets an item too)
procedure MoveItemsUp(list : TListView; lowIndex, highIndex, subItemIndex : integer);

// Move items in range [low, high] one down
procedure MoveItemsDown(list : TListView; lowIndex, highIndex, subItemIndex : integer);

// Remove last item if its caption and all its subitems are empty
function RemoveItemIfEmpty(list : TListView) : boolean;

implementation

uses lb_lib;

procedure SetMinimumSubItems(item : TListItem; count : integer);
var i : integer;
begin
  for i := item.SubItems.Count + 1 to count do
  begin
    item.SubItems.Add('');
  end;
end;

procedure MoveSubItem(source, target : TListItem; subItemIndex : integer);
begin
  if source.SubItems.Count > subItemIndex then
  begin
    SetMinimumSubItems(target, subItemIndex + 1);
    target.SubItems[subItemIndex] := source.SubItems[subItemIndex];
  end
  else if target.SubItems.Count > subItemIndex then
  begin
    // There is no source, empty target to avoid a duplicate line
    target.SubItems[subItemIndex] := '';
  end;
end;

procedure MoveItemsUp(list : TListView; lowIndex, highIndex, subItemIndex : integer);
var i : integer;
begin
  log(format('MoveItemsUp %d %d of %d', [lowIndex, highIndex, list.items.count]));
  assert(lowIndex > 0);
  assert(highIndex < list.Items.Count);
  for i := lowIndex to highIndex do
  begin
    MoveSubItem(list.items[i], list.items[i - 1], subItemIndex);
  end;

  // The very last item is already moved, and will now be unassigned
  if list.items[highIndex].SubItems.Count > subItemIndex then list.items[highIndex].SubItems[subItemIndex] := '';
  RemoveItemIfEmpty(list);
end;

procedure MoveItemsDown(list : TListView; lowIndex, highIndex, subItemIndex : integer);
var i : integer;
  lastItem : TListItem;
begin
  assert(lowIndex > 0);
  assert(highIndex < list.Items.Count);

  // If the last subitem to be moved down isn't empty, append a new item to the list
  lastItem := list.Items[list.Items.Count - 1];
  if (lastItem.SubItems.Count > subItemIndex) and (trim(lastItem.SubItems[subItemIndex]) <> '') then
  begin
    list.Items.Add;
    inc(highIndex);
  end;

  for i := highIndex downto lowIndex do
  begin
    MoveSubItem(list.items[i - 1], list.items[i], subItemIndex);
  end;
end;

function RemoveItemIfEmpty(list : TListView) : boolean;
var i : integer;
  item : TListItem;
begin
  result := false;
  if list.items.count = 0 then exit;
  item := list.items[list.items.count - 1];

  if trim(item.caption) <> '' then exit;

  for i := 0 to item.subitems.count - 1 do
  begin
    if trim(item.subitems[i]) <> '' then exit;
  end;

  // It's empty
  list.items.Delete(item.index);
  result := true;
end;

end.

