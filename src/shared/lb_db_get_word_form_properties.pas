unit lb_db_get_word_form_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb, lb_word_form_properties;

function GetWordFormProperties(cn : TSqlConnection; permanentId : integer) : TArrayOfWordFormProperties;

implementation

uses lb_db_functions, lb_const;

function GetWordFormProperties(cn : TSqlConnection; permanentId : integer) : TArrayOfWordFormProperties;
var
  query : TSQLQuery;
  fields, sql, wordForm  : string;
  n : integer;
begin
  fields := 'number_id,case_id,person_id,tense_id,mood_id';
  sql := format('select w.type_id,form_id,%s,subrank,f.word from forms f'
  + ' join words w on f.word_id=w.word_id where w.permanent_id=%d'
  + ' order by %s,subrank', [fields, permanentId, fields]);

  n := 0;
  result := [];

  query := TSqlQuery.Create(nil);

  try
    query.database := cn;
    query.SQL.Text := sql;
    query.Open;

    while not query.Eof do
    begin
      // For now skip some fields such as MOOD imperative
      if query.FieldByName('mood_id').AsInteger <> 2 then
      begin
        SetLength(result, n + 1);
        result[n].typeId := query.FieldByName('type_id').AsInteger;
        result[n].formId := query.FieldByName('form_id').AsInteger;
        result[n].caseId := query.FieldByName('case_id').AsInteger;
        result[n].numberId := query.FieldByName('number_id').AsInteger;
        result[n].tenseId := query.FieldByName('tense_id').AsInteger;
        result[n].personId := query.FieldByName('person_id').AsInteger;
        result[n].wordForm := query.FieldByName('word').AsString;
        inc(n);
      end;

      query.Next;
    end;
  finally
    query.Free;
  end;

  if n = 0 then
  begin
    // No forms found. Get the main form, if any.
    wordForm := QueryAsString(cn, 'select word from words where permanent_id=%d', [permanentId], '');
    if wordForm <> '' then
    begin
      SetLength(result, 1);
      result[n].formId := -1;
      result[n].caseId := 1;
      result[n].numberId := NumberIdSingular;
      result[n].wordForm := wordForm;
    end;
  end;
end;

end.

