// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Imports CSV files from Open Russian into a SQLite database

unit lb_import_or_words;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ImportOpenRussianWords(cn : TSqlConnection; const folder : string;
  keepTemps : boolean = false; maxCsvRows : integer = -1) : boolean;

implementation

uses lb_lib, lb_import_word, lb_db_import_csv, lb_db_functions, lb_lib_string, lb_const;

const or_tables : array of string =
  ('words', 'nouns', 'verbs', 'adjectives', 'declensions', 'conjugations', 'translations');

function GetGenderId(const s : string) : integer;
begin
  if s = 'm' then result := GenderIdMale
  else if s = 'f' then result := GenderIdFemale
  else if s = 'n' then result := GenderIdNeutral
  else result := -1;
end;

function GetAspectId(const s : string) : integer;
begin
  if s = 'perfective' then result := AspectIdPerfective
  else if s = 'imperfective' then result := AspectIdImperfective
  else result := -1;
end;

function GetTypeId(const s : string) : integer;
begin
  if s = 'verb' then result := TypeIdVerb
  else if s = 'noun' then result := TypeIdNoun
  else if s = 'adjective' then result := TypeIdAdjective
  else if s = 'adverb' then result := TypeIdAdverb
  else if s = 'expression' then result := TypeIdExpression
  else result := TypeIdOther
end;

function GetValueOrMinusOne(const q : TSqlQuery; const column : string) : integer;
begin
  if not TryStrToInt(q.FieldByName(column).AsString, result) then result := -1;
end;

procedure ImportCases(cn : TSqlConnection; const q : TSqlQuery; const suffix : string;
  wordId, typeId, genderId, numberId, animacyId : integer);
const columns : array of string = ('nom', 'gen', 'dat', 'acc', 'inst', 'prep');
var c : integer;
begin
  for c := low(columns) to high(columns) do
  begin
    ImportWordForm(cn, typeId, wordId, c + 1, genderId, -1, numberId, -1, -1, -1,
       q.FieldByName(columns[c] + Suffix).AsString, animacyId, -1, sourceTypeIdOpenRussian);
  end;
end;

procedure ImportConjugations(cn : TSqlConnection; const q : TSqlQuery;
  wordId, aspectId : integer);
const columns : array of string = ('sg1', 'sg2', 'sg3', 'pl1', 'pl2', 'pl3');
var c, numberId, personId : integer;
begin
  for c := low(columns) to high(columns) do
  begin
    personId := specialize IfThen<integer>(c < 3, c + 1, c - 2); // 0,1,2 vs 3,4,5 -> 1,2,3
    numberId := specialize IfThen<integer>(c < 3, NumberIdSingular, NumberIdPlural);
    ImportVerbForm(cn, wordId, personId, numberId, TenseIdPresent, aspectId, -1,
       q.FieldByName(columns[c]).AsString, GenderIdNone, sourceTypeIdOpenRussian);
  end;
end;

procedure ImportPastTense(cn : TSqlConnection; const q : TSqlQuery;
  wordId, aspectId : integer);
const
  columns : array of string = ('past_m', 'past_f', 'past_n', 'past_pl');
  genderIds : array of integer = (GenderIdMale, GenderIdFemale, GenderIdNeutral, GenderIdNone);
var c, numberId : integer;
begin
  for c := low(columns) to high(columns) do
  begin
    numberId := specialize IfThen<integer>(c < high(columns), NumberIdSingular, NumberIdPlural);
    ImportVerbForm(cn, wordId, -1, numberId, TenseIdPast, aspectId, MoodIdIndicative,
       q.FieldByName(columns[c]).AsString, genderIds[c], sourceTypeIdOpenRussian);
  end;
end;

function DoImportWord(cn : TSqlConnection; q : TSqlQuery; typeId : integer) : int64;
begin
  result := ImportWord(cn, q.FieldByName('accented').AsString,
    q.FieldByName('bare').AsString,
    typeId,
    q.FieldByName('id').AsInteger,
    GetValueOrMinusOne(q, 'rank'),
    q.FieldByName('level').AsString,
    SourceTypeIdOpenRussian);
end;

procedure ImportOpenRussianNouns(cn: TSqlConnection);
const
  typeId = typeIdNoun;
  // Using several (left) joins, resulting in duplicate field names,
  // of which the second names are indicated by SqLite as '_1'
  sql : string = 'select * from or_words w'
    + ' join or_nouns f on w.id=f.word_id'
    + ' left join or_declensions d1 on f.decl_sg_id=d1.id'
    + ' left join or_declensions d2 on f.decl_pl_id=d2.id';

var q : TSqlQuery;
  newId : integer;
  animacyId, genderId : integer;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    while not q.eof do
    begin
      newId := DoImportWord(cn, q, typeId);
      genderId := GetGenderId(q.FieldByName('gender').AsString);
      animacyId := GetValueOrMinusOne(q, 'animate');

      // Forms: singular (without suffix) and plural (nom_1, gen_1, ...)
      ImportCases(cn, q, '', newId, typeId, genderId, numberIdSingular, animacyId);
      ImportCases(cn, q, '_1', newId, typeId, genderId, numberIdPlural, animacyId);

      q.next;
    end;
  finally
    q.free;
  end;
end;

procedure ImportOpenRussianAdjectives(cn: TSqlConnection);
const
  typeId : integer = typeIdAdjective;

  // It's a double join, resulting in duplicate field names,
  // of which the second names are indicated by SqLite as '_1'
  sql : string = 'select * from or_words w'
    + ' join or_adjectives f on w.id=f.word_id'
    + ' left join or_declensions d1 on f.decl_m_id=d1.id'
    + ' left join or_declensions d2 on f.decl_f_id=d2.id'
    + ' left join or_declensions d3 on f.decl_n_id=d3.id'
    + ' left join or_declensions d4 on f.decl_pl_id=d4.id'
    ;

var q : TSqlQuery;
  newId : integer;
  animacyId : integer;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    while not q.eof do
    begin
      newId := DoImportWord(cn, q, typeId);

      animacyId := -1;

      // Forms: male, female, neutral, plural
      ImportCases(cn, q, '', newId, typeId, genderIdMale, numberIdSingular, animacyId);
      ImportCases(cn, q, '_1', newId, typeId, genderIdFemale, numberIdSingular, animacyId);
      ImportCases(cn, q, '_2', newId, typeId, genderIdNeutral, numberIdSingular, animacyId);
      ImportCases(cn, q, '_3', newId, typeId, GenderIdNone, numberIdPlural, animacyId);

      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdNone, NumberIdNone,
          q.FieldByName('comparative').AsString, SpecificIdComparative);
      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdNone, NumberIdNone,
          q.FieldByName('superlative').AsString, SpecificIdSuperlative);
      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdMale, NumberIdSingular,
          q.FieldByName('short_m').AsString, SpecificIdShortForm);
      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdFemale, NumberIdSingular,
          q.FieldByName('short_f').AsString, SpecificIdShortForm);
      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdNeutral, NumberIdSingular,
          q.FieldByName('short_n').AsString, SpecificIdShortForm);
      ImportAdjectiveForm(cn, newId, CaseIdNone, GenderIdIrrelevant, NumberIdPlural,
          q.FieldByName('short_pl').AsString, SpecificIdShortForm);

      q.next;
    end;
  finally
    q.free;
  end;
end;

procedure ImportOpenRussianVerbs(cn: TSqlConnection);
const
  typeId = TypeIdVerb;
  sql : string = 'select * from or_words w'
    + ' join or_verbs f on w.id = f.word_id'
    + ' left join or_conjugations c on f.presfut_conj_id = c.id';

var q : TSqlQuery;
  aspectId, newId : integer;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    while not q.eof do
    begin
      newId := DoImportWord(cn, q, typeId);
      aspectId := GetAspectId(q.FieldByName('aspect').AsString);
      ImportConjugations(cn, q, newId, aspectId);

      ImportVerbForm(cn, newId, -1, numberIdSingular, TenseIdPresent, aspectId, MoodIdImperative,
         q.FieldByName('imperative_sg').AsString, GenderIdNone, sourceTypeIdOpenRussian);
      ImportVerbForm(cn, newId, -1, numberIdPlural, TenseIdPresent, aspectId, MoodIdImperative,
         q.FieldByName('imperative_pl').AsString, GenderIdNone, sourceTypeIdOpenRussian);

      ImportPastTense(cn, q, newId, aspectId);

      q.next;
    end;
  finally
    q.free;
  end;
end;

procedure ImportOpenRussianOther(cn: TSqlConnection);
const
  sql : string = 'select * from or_words w'
      + ' where id not in (select word_id from or_nouns)'
      + ' and id not in (select word_id from or_adjectives)'
      + ' and id not in (select word_id from or_verbs)';

var q : TSqlQuery;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    while not q.eof do
    begin
      DoImportWord(cn, q, GetTypeId(q.FieldByName('type').AsString));
      q.next;
    end;
  finally
    q.free;
  end;
end;

function ImportCsvTables(cn : TSqlConnection; const folder : string; maxCsvRows : integer) : boolean;
var i : integer;
  table, filename : string;
begin
  result := true;
  for i := low(or_tables) to high(or_tables) do
  begin
    filename := format('%s\%s.csv', [folder, or_tables[i]]);
    if fileexists(filename) then
    begin
      table := format('or_%s', [or_tables[i]]);
      log(format('Import csv %s', [table]));
      cn.executedirect(format('drop table if exists %s', [table]));
      ImportCsvFile(cn, filename, table, #9, 'id', maxCsvRows);
    end
    else
    begin
      writeln('File not found: ', filename);
      result := false;
    end;
  end;
end;

procedure DeleteCsvTables(cn : TSqlConnection);
var i : integer;
  table : string;
begin
  for i := low(or_tables) to high(or_tables) do
  begin
    table := format('or_%s', [or_tables[i]]);
    cn.executedirect(format('drop table if exists %s', [table]));
  end;
end;

procedure UpdateRanks(cn : TSqlConnection);
const levels : array of string = ('A1', 'A2', 'B1', 'B2', 'C1', 'C1');
  sql : string = 'update words set rank=(select round(avg(rank)) from words where level=%s) where rank is null and level=%s';
var i : integer;
  s : string;
begin
  // There are unranked words which are very easy. Fix this.
  for i := low(levels) to high(levels) do
  begin
    s := Quoted(levels[i]);
    ExecuteSql(cn, sql, [s, s]);
  end;
end;

function ImportOpenRussianWords(cn : TSqlConnection; const folder : string;
  keepTemps : boolean; maxCsvRows : integer) : boolean;
begin
  result := ImportCsvTables(cn, folder, maxCsvRows);
  if not result then exit;

  log('Import nouns');
  ImportOpenRussianNouns(cn);
  log('Import verbs');
  ImportOpenRussianVerbs(cn);
  log('Import adjectives');
  ImportOpenRussianAdjectives(cn);
  log('Import other');
  ImportOpenRussianOther(cn);

  UpdateRanks(cn);

  if not keepTemps then
  begin
    DeleteCsvTables(cn);
  end;
end;

end.
