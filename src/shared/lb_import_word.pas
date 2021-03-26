// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Shared functionality to import words and word forms

unit lb_import_word;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ImportWord(cn: TSqlConnection; const wordAccented, wordBare : string;
      typeId, sourceId, rank : integer;
      const level : string;
      sourceTypeId : integer = 1;
      permanentId : integer = -1) : int64;

function ImportWordForm(cn: TSqlConnection; typeId, wordId, caseId, genderId, personId,
    numberId, tenseId, aspectId, moodId : integer;
    wordForm : string;
    animacyId, specificId, sourceTypeId : integer) : int64;

function ImportNounForm(cn: TSqlConnection; wordId, caseId, genderId, numberId : integer;
    wordForm : string;
    animacyId : integer = -1;
    sourceTypeId : integer = -1) : int64;

function ImportVerbForm(cn: TSqlConnection; wordId, personId, numberId, tenseId, aspectId, moodId : integer;
    wordForm : string;
    genderId : integer = -1;
    sourceTypeId : integer = -1) : int64;

function ImportAdjectiveForm(cn: TSqlConnection; wordId, caseId, genderId, numberId : integer;
    wordForm : string;
    specificId : integer = -1) : int64;

implementation

uses lb_const, lb_sql_dml_insert, lb_datamodel, lb_lib, lb_lib_string;

function ImportWord(cn: TSqlConnection; const wordAccented, wordBare : string;
      typeId, sourceId, rank : integer;
      const level : string;
      sourceTypeId, permanentId : integer) : int64;
begin
  if sourceTypeId = -1 then sourceTypeId := sourceTypeIdOpenRussian;

  result := SqlInsert(cn, KTableNameWords,
      [cv('word', wordAccented),
       cv('bare_word', wordBare),
       cv('type_id', typeId),
       cv('source_id', sourceId),
       cv('source_type_id', sourceTypeId),
       cvif('permanent_id', permanentId > 0, permanentId),
       cvif('rank', rank > 0, rank),
       cvif('level', (level <> '') and (level <> 'NONE'), level)]);
end;

function ImportWordForm(cn: TSqlConnection; typeId, wordId, caseId, genderId, personId,
    numberId, tenseId, aspectId, moodId : integer;
    wordForm : string;
    animacyId, specificId, sourceTypeId : integer) : int64;

  function Extract(const term : string; b1, b2 : char; out note : string) : string;
  var p1, p2, len : integer;
  begin
    note := '';
    result := term;
    p1 := pos(b1, term);
    p2 := pos(b2, term);
    if (p1 > 0) and (p2 > p1) then
    begin
      len := p2 - p1 - 1;
      note := copy(result, p1 + 1, len);
      delete(result, p1, len + 2);
    end;
  end;

var terms : TStringArray;
  term, n1, n2 : string;
  a : integer;
begin
  result := 0;
  if sourceTypeId = -1 then sourceTypeId := sourceTypeIdOpenRussian;

  wordForm := trim(wordForm);
  if wordForm = '' then exit;

  // Sometimes there are alternatives, sometimes with ';', sometimes with ','
  // or even '//' (TODO)
  if pos(';', wordForm) > 0 then terms := SplitString(wordForm, ';')
  else if pos('/', wordForm) > 0 then terms := SplitString(wordForm, '/')
  else terms := SplitString(wordForm, ',');

  for a := low(terms) to high(terms) do
  begin
    term := trim(terms[a]);
    term := trim(Extract(term, '(', ')', n1));
    term := trim(Extract(term, '[', ']', n2));

    if (term = '') and (n1 <> '') then
    begin
      // TODO: add and fill field "is_rare" in DB
      term := n1;
    end;

    if term <> '' then
    begin
      result := SqlInsert(cn, KTableNameForms,
          [cv('word_id', wordId),
           cv('word', term),
           cv('bare_word', BareString(term)),
           cv('type_id', typeId),
           cv('source_type_id', sourceTypeId),
           cvif('gender_id', genderId >= 0, genderId),
           cvif('person_id', personId >= 0, personId),
           cvif('number_id', numberId >= 0, numberId),
           cvif('tense_id', tenseId >= 0, tenseId),
           cvif('aspect_id', aspectId >= 0, aspectId),
           cvif('mood_id', moodId >= 0, moodId),
           cvif('case_id', caseId >= 0, caseId),
           cvif('animacy_id', animacyId >= 0, animacyId),
           cvif('specific_id', specificId >= 0, specificId),
           cvif('subrank', length(terms) > 1, a + 1)]);
    end;
  end;
end;

function ImportNounForm(cn: TSqlConnection; wordId, caseId, genderId, numberId : integer;
    wordForm : string;
    animacyId : integer = -1;
    sourceTypeId : integer = -1) : int64;
begin
  result := ImportWordForm(cn, TypeIdNoun, wordId, caseId, genderId, -1, numberId, -1, -1, -1,
    wordForm, animacyId, -1, sourceTypeId);
end;

function ImportVerbForm(cn: TSqlConnection; wordId, personId, numberId, tenseId, aspectId, moodId : integer;
    wordForm : string;
    genderId : integer = -1;
    sourceTypeId : integer = -1) : int64;
begin
  result := ImportWordForm(cn, TypeIdVerb, wordId, -1, genderId, personId, numberId, tenseId, aspectId, moodId,
    wordForm, -1, -1, sourceTypeId);
end;

function ImportAdjectiveForm(cn: TSqlConnection; wordId, caseId, genderId, numberId : integer;
    wordForm : string;
    specificId : integer = -1) : int64;
begin
  result := ImportWordForm(cn, TypeIdAdjective, wordId, caseId, genderId, -1, numberId, -1, -1, -1,
    wordForm, -1, specificId, sourceTypeIdOpenRussian);
end;

end.

