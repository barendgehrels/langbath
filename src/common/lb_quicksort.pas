// Language Bath - Assign Timings
// Copyright (c) 2020-2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit implements generic quicksort and binary search functionality

unit lb_quicksort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // generic TGenericDefaultCompare, for now
  TIntegerLess = class
    function Less(const a, b : integer) : boolean;
  end;
  TIntegerGreater = class
    function Less(const a, b : integer) : boolean;
  end;

// Generic function to sort a dynamic array of any record type with a custom search function
generic procedure QuickSort<T, C>(var ar: array of T; cmp : C);

// Generic function to search in a dynamic array of any record type with a custom search function
generic function BinarySearch<T, C>(const ar : array of T; cmp : C; const value: T): integer;

// Implementation function, not to be called, but it should be declared in the interface
generic procedure DoQuickSort<T, C>(var ar: array of T; lowIndex, highIndex: Integer; cmp : C);

// Proxies for caes where it is not necessary to pass a constructed comparator
generic function CallFind<T, C>(const a : array of T; const value : T) : integer;

generic procedure CallQuickSort<T, C>(var a: array of T);

// Specializated quick sort for arrays of integers
procedure Sort(var a : array of integer);

// Specializated binary search for arrays of integers
function Find(const a : array of integer; value : integer) : integer;

implementation

uses lb_array_lib;

generic procedure DoQuickSort<T, C>(var ar: array of T;
  lowIndex, highIndex: integer; cmp : C);
var
  lo, hi, mid : integer;
  pivot : T;
begin
  lo := lowIndex;
  hi := highIndex;
  mid := (lo + hi) div 2;
  if (mid < lo) or (mid > hi) then
  begin
    exit;
  end;
  pivot := ar[mid];
  repeat
    while cmp.less(ar[lo], pivot) do inc(lo);
    while cmp.less(pivot, ar[hi]) do dec(hi);
    if lo <= hi then
    begin
      specialize Swap<T>(ar[lo], ar[hi]);
      inc(lo);
      dec(hi);
    end;
  until lo > hi;
  if hi > lowIndex then specialize DoQuickSort<T, C>(ar, lowIndex, hi, cmp);
  if lo < highIndex then specialize DoQuickSort<T, C>(ar, lo, highIndex, cmp);
end;

generic procedure QuickSort<T, C>(var ar: array of T; cmp : C);
begin
  if length(ar) > 0 then
  begin
    specialize DoQuickSort<T, C>(ar, low(ar), high(ar), cmp);
  end;
end;

generic function BinarySearch<T, C>(const ar : array of T; cmp : C; const value : T): integer;
var lo, hi, mid : integer;
begin
  lo := low(ar);
  hi := high(ar);
  result := -1;

  while lo <= hi do
  begin
    mid := (lo + hi) div 2;
    if cmp.less(ar[mid], value) then lo := mid + 1
    else if cmp.less(value, ar[mid]) then hi := mid - 1
    else
    begin
      result := mid;
      exit;
    end
  end;
end;


generic function CallFind<T, C>(const a : array of T; const value : T) : integer;
var cmp : C;
begin
  cmp := C.Create;
  result := specialize BinarySearch<T, C>(a, cmp, value);
  cmp.free;
end;

generic procedure CallQuickSort<T, C>(var a: array of T);
var cmp : C;
begin
  cmp := C.Create;
  specialize QuickSort<T, C>(a, cmp);
  cmp.free;
end;

function TIntegerLess.Less(const a, b : integer) : boolean;
begin
  result := a < b;
end;

function TIntegerGreater.Less(const a, b : integer) : boolean;
begin
  result := a > b;
end;

procedure Sort(var a : array of integer);
begin
  specialize CallQuickSort<integer, TIntegerLess>(a);
end;

function Find(const a : array of integer; value : integer) : integer;
begin
  result := specialize CallFind<integer, TIntegerLess>(a, value);
end;

end.

