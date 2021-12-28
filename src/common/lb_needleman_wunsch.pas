// Language Bath - Common
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Functions to compare two strings with Needleman-Wunsch alignment

unit lb_needleman_wunsch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Needleman-Wunsch alignment
// d (the gap penalty score) should be negative
procedure AlignWithNeedlemanWunsch(const A, B: string; const Aligner: string;
    d: integer; out AlignmentA, AlignmentB: string);

implementation

uses Math, LazUTF8;

procedure AlignWithNeedlemanWunsch (const A, B: string; const Aligner : string;
    d: integer; out AlignmentA, AlignmentB: string);

  //function combination(const s1, s2, letter1, letter2 : string) : boolean;
  //begin
  //  result := ((s1 = letter1) and (s2 = letter2))
  //         or ((s1 = letter2) and (s2 = letter1));
  //end;

  function IsVowel(const s : string) : boolean;
  begin
    result := (s = 'а') or (s = 'е') or (s = 'о') or (s = 'у') or (s = 'э')
      or (s = 'я')  or (s = 'ё') or (s = 'и') or (s = 'ы');
    //(, 'б', 'в', 'г', 'д', , , 'ж', 'з', , 'й',
    // 'к', 'л', 'м', 'н', , 'п', 'р', 'с', 'т', , 'ф',
    // 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', , 'ь', , 'ю',
  end;


  // S(a, b) is the similarity of characters a and b
  // The higher the result value, the more similar they are
  function S(const s1, s2: String): integer;
  begin
    if (s1 = ' ') and (s2 = ' ') then result := 20
    else if (s1 = 'т') and (s2 = 'т') then result := 15
    else if s1 = s2 then result := 10
    else if IsVowel(s1) and IsVowel(s2) then result := 9
    // Similar characters
    //else if combination(s1, s2, 'k', 'q') then result := 5
    //else if combination(s1, s2, 'd', 't') then result := 5
    //else if combination(s1, s2, 'p', 'b') then result := 5
    else result := -1;
  end;

var
  F: array of array of integer;
  lena, lenb, i, j, match, delete, insert : integer;
  ai : string;
begin
  F := [];
  lenA := UTF8Length(A);
  lenB := UTF8Length(B);
  SetLength (F, lenA + 1, lenB + 1);

  for i := 0 to lenA do F[i, 0] := d * i;
  for j := 0 to lenB do F[0, j] := d * j;

  for i := 1 to lenA do
  begin
    ai := UTF8Copy(a, i, 1);
    for j := 1 to lenB do
    begin
      match := F[i - 1, j - 1] + S(ai, UTF8Copy(b, j, 1));
      delete := F[i - 1, j] + d;
      insert := F[i, j - 1] + d;
      F[i, j] := max(match, max(delete, insert));
    end;
  end;

  AlignmentA := '';
  AlignmentB := '';
  i := lenA;
  j := lenB;
  while (i > 0) or (j > 0) do
  begin
    if (i > 0)
    and (j > 0)
    and (F[i, j] = F[i - 1, j - 1] + S(UTF8Copy(a, i, 1), UTF8Copy(b, j, 1))) then
    begin
      AlignmentA := UTF8Copy(a, i, 1) + AlignmentA;
      AlignmentB := UTF8Copy(b, j, 1) + AlignmentB;
      dec(i);
      dec(j);
    end
    else if (i > 0) and (F[i, j] = F[i - 1, j] + d) then
    begin
      AlignmentA := UTF8Copy(a, i, 1) + AlignmentA;
      AlignmentB := Aligner + AlignmentB;
      dec(i);
    end
    else
    begin
      assert(j > 0);
      AlignmentA := Aligner + AlignmentA;
      AlignmentB := UTF8Copy(b, j, 1) + AlignmentB;
      dec(j);
    end;
  end;
end;

end.

