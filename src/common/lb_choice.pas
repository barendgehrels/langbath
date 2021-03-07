// Language Bath - Choice functionality
// Copyright (c) 2020-2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// Code in this unit implements a function similar to Python's Random.Choice

unit lb_choice;

{$mode objfpc}{$H+}

interface

// Implements a function similar to Python's Random.Choice or NumPy's Choice.
// It returns the selected index in the array of weights.
// If the array is empty, it returns -1.
// Negative weights count as zero.
// If all weights are zero, it selects a random index (similar to all weights 1.0)
// It is not necessary that the sum of weights is one.
// Call Randomize before calling this function, or set a random seed for predictable results.
function RandomChoice(const weights : array of double) : integer;

implementation

uses Math;

function RandomChoice(const weights : array of double) : integer;
var r, sum, w : double;
  i : integer;
begin
  result := -1;
  if length(weights) = 0 then exit;

  sum := 0;
  for i := low(weights) to high(weights) do
  begin
    w := max(0.0, weights[i]);
    sum := sum + w;
  end;

  if sum <= 0 then
  begin
    result := random(length(weights));
    exit;
  end;

  r := random * sum;

  sum := 0;
  for i := low(weights) to high(weights) do
  begin
    w := max(0.0, weights[i]);
    sum := sum + w;
    if (w > 0) and (sum >= r) then
    begin
      result := i;
      exit;
    end;
  end;

  // Somehow the last one is missed (random=1.0 / FP-precision)
  // Return the highest index with non-zero weight.
  for i := high(weights) downto low(weights) do
  begin
    if weights[i] > 0 then
    begin
      result := i;
      exit;
    end;
  end;

  // This should not happen. It will return -1
  assert(false);
end;

end.
