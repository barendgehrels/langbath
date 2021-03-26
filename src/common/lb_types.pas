unit lb_types;

{$mode objfpc}{$H+}

interface

type
  // Array types (different names than Lazarus has (TIntegerArray)
  // because that is not a dynamic array)
  TArrayOfInteger = array of integer;
  TArrayOfString = array of string;

  TSetOfByte = set of byte;

implementation

end.

