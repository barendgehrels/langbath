unit lb_replace_names_in_translations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCorrectedTranslation = record
    Original : string;
    WrongTranslation : array of string;
    CorrectedTranslation : string;
  end;

  TCorrectedTranslationArray = array of TCorrectedTranslation;

function ReadTranslationCorrections(const filename : string) : TCorrectedTranslationArray;

implementation

uses lb_lib;

function ReadTranslationCorrections(const filename: string): TCorrectedTranslationArray;
var list : TStringList;
  j, k, n: integer;
  ar : TStringArray;
begin
  result := [];
  SetLength(result, 0);
  n := 0;

  list := TStringList.Create;
  try
    list.LoadFromFile(filename);

    SetLength(result, list.count);

    for k := 0 to list.count - 1 do
    begin
      ar := SplitString(list[k], ',');
      if length(ar) >= 3 then
      begin
        SetLength(result, n + 1);
        result[n].Original := ar[0];
        result[n].CorrectedTranslation := ar[1];
        SetLength(result[n].WrongTranslation, length(ar) - 2);
        for j := 0 to length(ar) - 3 do
        begin
          result[n].WrongTranslation[j] := ar[j + 2];
        end;
        inc(n);
      end;
    end;
  finally
    list.free;
  end;
end;

end.

