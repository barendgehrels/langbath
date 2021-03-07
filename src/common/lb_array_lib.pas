// Language Bath - Assign Timings
// Copyright (c) 2020-2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit implements generic array functionality

unit lb_array_lib;

{$mode objfpc}{$H+}

interface

// Returns true if the specified item is in the array
generic function IsInArray<T>(const s : T; const a : array of T) : boolean;

// Returns the index of the specified item is in the array, or -1 if it is not found
generic function ArrayIndex<T>(const s : T; const a : array of T) : integer;

// Swaps two items in an array
generic procedure Swap<T>(var a, b : T); inline;

// Shuffle items in an array, optionally until the specified index
generic procedure Shuffle<T>(var Values : array of T; highIndex : integer = -1);

implementation

generic function IsInArray<T>(const s : T; const a : array of T) : boolean;
var i : integer;
begin
  result := false;
  for i := low(a) to high(a) do
  begin
    if s = a[i] then result := true;
  end;
end;

generic function ArrayIndex<T>(const s : T; const a : array of T) : integer;
var i : integer;
begin
  result := -1;
  i := low(a);
  while (result = -1) and (i <= high(a)) do
  begin
    if s = a[i] then result := i;
    inc(i);
  end;
end;


generic procedure Swap<T>(var a, b : T); inline;
var temp : T;
begin
  temp := a;
  a := b;
  b := temp;
end;

generic procedure Shuffle<T>(var Values : array of T; highIndex : integer = -1);
var i : integer;
begin
  if highIndex = -1 then highIndex := high(values);
  // Adapted from https://forum.lazarus.freepascal.org/index.php?topic=36445.15
  for i := highIndex downto 1 do
  begin
    specialize Swap<T>(Values[i], Values[Random(i)]);
  end;
end;

end.

