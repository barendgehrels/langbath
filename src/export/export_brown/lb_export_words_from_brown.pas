// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

unit lb_export_words_from_brown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ExportWordsFromBrown(cn : TSqlConnection; const filename : string) : boolean;

implementation

uses LazUtf8, LazLoggerBase;

function Cleaned(const s : string) : string;
begin
  result := StringReplace(s, '\t', ' ', [rfReplaceAll]);
  result := StringReplace(result, '*', '', [rfReplaceAll]);
  result := trim(result);
end;

function ExportBrownQueryToList(cn: TSqlConnection; const sql : string;
  useDbRank : boolean; var rank : integer) : TStringList;
var q : TSqlQuery;
  s : string;
  i, field0, previousField0 : integer;
begin
  result := TStringList.Create;

  previousField0 := -1;

  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    while not q.eof do
    begin
      // First list uses Brown rank (numerical)
      // Next lists contain word.word_id in first field,
      // an increase rank on every new id
      field0 := q.Fields[0].AsInteger;

      if useDbRank then rank := field0
      else if field0 <> previousField0 then inc(rank);

      previousField0 := field0;

      s := inttostr(rank);

      // The other fields might have remaining uncleaned stuff as \t
      // (from OpenRussian) and is cleaned here (to be fixed in import or).
      for i := 1 to q.FieldCount - 1 do
      begin
        s := s + #9 + Cleaned(q.Fields[i].AsString);
      end;

      result.add(s);
      q.next;
    end;
  finally
    q.free;
  end;
  DebugLn(format('Exported %d lines', [result.count]));
end;



function ExportWordsFromBrown(cn : TSqlConnection; const filename : string) : boolean;
const sqlBrownWords : string =
  'select distinct c.brown_id,c.brown_word,f.bare_word'
  + ' from brown_words_cleaned c'
  + ' left join words w on c.bare_word=w.bare_word'
  + ' left join forms f on w.word_id=f.word_id'
  + ' order by brown_id,w.word_id,f.bare_word';
  sqlOtherWords : string =
    'select w.word_id,w.bare_word,f.bare_word from words w'
    + ' left join forms f on w.word_id=f.word_id'
    + ' where w.rank %s'
    + ' and w.bare_word not like ''%% %%'''
    + ' and w.bare_word not in (select bare_word from brown_words_cleaned)'
    + ' order by w.rank';
var mainList, otherList : TStringList;
  i, index : integer;
  query : string;
begin
  result := false;
  if fileexists(filename) then exit;

  index := 0;
  mainList := ExportBrownQueryToList(cn, sqlBrownWords, true, index);

  if true then
  begin
    query := format(sqlOtherWords, ['is not null']);
    otherList := ExportBrownQueryToList(cn, query, false, index);
    for i := 0 to otherList.count - 1 do mainList.add(otherList[i]);
    otherList.free;

    query := format(sqlOtherWords, ['is null']);
    otherList := ExportBrownQueryToList(cn, query, false, index);
    for i := 0 to otherList.count - 1 do mainList.add(otherList[i]);
    otherList.free;
  end;
  mainList.SaveToFile(filename);
  mainList.free;

  result := true;
end;

end.

