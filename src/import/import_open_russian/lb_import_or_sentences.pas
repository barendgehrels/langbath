// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Imports CSV files from Open Russian into a SQLite database

unit lb_import_or_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ImportOpenRussianSentences(cn : TSqlConnection;
    const folder : string; keepTemps : boolean; maxCsvRows : integer) : boolean;

implementation

uses LazUtf8, Md5,
  lb_lib, lb_db_import_csv, lb_import_sentence,
  lb_lib_string, lb_const;

const
  or_tables : array of string = ('sentences');

function DoImportOpenRussianSentences(cn: TSqlConnection;
    batchSize : integer;
    var sourceId : integer) : integer;
const
  // Get them in chuncks, to avoid out of memory
  sql : string = 'select a.id,a.ru,a.tl as en,b.tl as de from or_sentences a'
      + ' left join or_sentences b on a.tatoeba_key=b.tatoeba_key and b.lang=%s'
      + ' where a.lang=%s and a.audio_url not like %s'
      + ' and a.id > %d'
      + ' order by a.id'
      + ' limit %d';

var q : TSqlQuery;
 sentenceId, soundId : integer;
 sentence, soundFileName : string;
begin
  result := 0;
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := format(sql, [Quoted('de'), Quoted('en'), Quoted('%tatoeb%'),
                               sourceId, batchSize]);
    q.Open;

    while not q.eof do
    begin
      sourceId := q.FieldByName('id').AsInteger;
      sentence := trim(q.FieldByName('ru').AsString);

      // Insert sentence
      sentenceId := InsertSentence(cn, sourceTypeIdOpenRussian, sourceId, sentence);

      // Insert sound
      soundFilename := 'md5_' + MD5Print(Md5String(RemoveQuotes(sentence))) + '.mp3';
      soundId := InsertSound(cn, SourceTypeIdOpenRussian, soundFilename, false);
      InsertSoundLink(cn, sentenceId, soundId, -1, -1);

      // Insert english and german translations
      sentence := trim(q.FieldByName('en').AsString);
      if sentence <> '' then InsertTranslation(cn, sentenceId, LanguageIdEnglish, sourceId, sentence);
      sentence := trim(q.FieldByName('de').AsString);
      if sentence <> '' then InsertTranslation(cn, sentenceId, LanguageIdGerman, sourceId, sentence);

      inc(result);

      q.next;
    end;
  finally
    q.free;
  end;
  if result > 0 then Writeln(format('Imported %d rows', [result]));
end;

function ImportCsvTables(cn : TSqlConnection; const folder : string;
    maxCsvRows : integer) : boolean;
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

function ImportOpenRussianSentences(cn : TSqlConnection;
    const folder : string; keepTemps : boolean; maxCsvRows : integer) : boolean;
const batchSize = 10000;
var sourceId, n : integer;
begin
  result := ImportCsvTables(cn, folder, maxCsvRows);
  if not result then exit;

  // OR also uses Tatoeba and translations can be joined on them
  // (because there is nothing else for a join)
  cn.ExecuteDirect('create index idx_ors_tatkey on or_sentences(tatoeba_key)');
  cn.Transaction.Commit;

  sourceId := 0;
  repeat
    n := DoImportOpenRussianSentences(cn, batchSize, sourceId);
    if n > 0 then cn.Transaction.Commit;
  until n = 0;

  if not keepTemps then
  begin
    DeleteCsvTables(cn);
    cn.Transaction.Commit;
  end;
end;

end.


