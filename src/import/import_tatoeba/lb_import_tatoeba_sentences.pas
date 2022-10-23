unit lb_import_tatoeba_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ImportTatoebaSentences(cn : TSqlConnection; ta : TSqlTransaction;
    const folder : string; isNew, keepTemps : boolean;
    maxCsvRows, maxLevel : integer) : boolean;

implementation

uses LazUtf8,
  lb_lib, lb_db_import_csv, lb_sql_ddl, lb_import_sentence,
  lb_lib_string, lb_db_functions, lb_const;

const
  tablePrefix = 'tat';

  columnFirstId = 'first_id';
  columnSecondId = 'second_id';

  // Define the tables ("lang_mapping" is ours, the rest is from Tatoeba)
  //
  // Our table lang_mapping.csv should look like:
  //   rus<tab>16
  //   cat<tab>44
  //   eng<tab>1
  // Where the three-character language code is how it is known to Tatoeba,
  // and the numeric value how it is known in this code base
  // (for example: LanguageIdEnglish = 1)

  tatoebaTables : array of string
      = ('sentences', 'links', 'sentences_with_audio', 'lang_mapping');

  // sentences_with_audio (description retrieved 2022-10-23):
  // Sentence id [tab] Audio id [tab] Username [tab] License [tab] Attribution URL

  // Specify definitions
  tatoebaColumns : array of array of string
      = (('tatoeba_id', 'lang','sentence'),
         (columnFirstId, columnSecondId),
         ('tatoeba_id', 'audio_id', 'username', 'license', 'url'),
         ('lang', 'language_id'));
  tatoebaTypes : array of array of TColumnType
      = ((ColumnInteger, ColumnString, ColumnString),
         (ColumnInteger, ColumnInteger),
         (ColumnInteger, ColumnInteger, ColumnString, ColumnString, ColumnString),
         (ColumnString, ColumnInteger));
  tatoebaIndices : array of array of boolean
      = ((true, true, false),
         (true, true),
         (true, false, false, false),
         (true, true));

function ImportCsvTables(cn : TSqlConnection; const folder : string; maxCsvRows : integer) : boolean;
var i : integer;
  table, filename : string;
begin
  result := true;

  // Import the three Tatoeba tables, plus our table.
  // Tatoeba tables are large and only a part (~6000 sentences out of 9 million) is needed,
  // but using the generic CSV approach saves code, and inserts are fast now.

  for i := low(tatoebaTables) to high(tatoebaTables) do
  begin
    filename := format('%s%s.csv', [folder, tatoebaTables[i]]);
    if fileexists(filename) then
    begin
      table := format('%s_%s', [tablePrefix, tatoebaTables[i]]);
      log(format('Import csv %s', [table]));
      Writeln(format('Import csv %s', [table]));
      cn.executedirect(format('drop table if exists %s', [table]));

      cn.transaction.Commit;

      cn.transaction.StartTransaction;
      ImportCsvFile(cn, filename, table,
        tatoebaColumns[i], tatoebaTypes[i], tatoebaIndices[i], #9, maxCsvRows);
      cn.transaction.Commit;
    end
    else
    begin
      writeln('File not found: ', filename);
      result := false;
    end;
  end;

  if result then
  begin
    // Create combined indices (forcing covering index for queries)
    RecreateIndex(cn, 'tat_links', [columnFirstId, columnSecondId]);
    RecreateIndex(cn, 'tat_lang_mapping', ['lang', 'language_id']);
  end;
end;

procedure DeleteTemporaryTatoebaTables(cn : TSqlConnection);
var i : integer;
  table : string;
begin
  for i := low(tatoebaTables) to high(tatoebaTables) do
  begin
    table := format('%s_%s', [tablePrefix, tatoebaTables[i]]);
    ExecuteSql(cn, 'drop table if exists %s', [table]);
  end;
end;

procedure UpdateTatoebaSentences(cn: TSqlConnection);
var q, u : TSqlQuery;
 sentenceId : integer;
 sentence : string;
begin
  q := TSQLQuery.Create(nil);
  u := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := format('select * from sentences where source_type_id=%d', [SourceTypeIdTatoeba]);
    q.Open;

    u.Database := cn;
    u.SQL.Text := 'update sentences set bare_sentence=:bs where sentence_id=:id';
    u.Prepare;

    while not q.eof do
    begin
      sentenceId := q.FieldByName('sentence_id').AsInteger;
      sentence := q.FieldByName('sentence').AsString;

      u.Params.Items[0].Value := BareString(sentence);
      u.Params.Items[1].Value := sentenceId;
      u.ExecSQL;

      q.next;
    end;
  finally
    q.free;
    u.free;
  end;
end;

procedure UpdateTatoebaAudioLinks(cn: TSqlConnection);
var q : TSqlQuery;
 sentenceId, audioId, soundId : integer;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := format('select s.*,t.audio_id from sentences s'
      + ' left join tat_sentences_with_audio t on s.source_id=t.tatoeba_id'
      + ' where s.source_type_id=%d', [SourceTypeIdTatoeba]);
    q.Open;

    while not q.eof do
    begin
      sentenceId := q.FieldByName('sentence_id').AsInteger;
      audioId := q.FieldByName('audio_id').AsInteger;

      soundId := InsertSound(cn, sourceTypeIdTatoeba, format('https://tatoeba.org/audio/download/%d', [audioId]));
      InsertSoundLink(cn, sentenceId, soundId, -1, -1);

      q.next;
    end;
  finally
    q.free;
  end;
end;

procedure InsertIntoLangBathSentenceTables(cn: TSqlConnection; targetLanguageId : byte;
    maxLevel : integer;
    back : boolean = false);
const
    baseQuery
      = 'insert into translations(sentence_id,language_id,sentence,source_id,distance)'
      + ' select distinct sentence_id,m.language_id,t.sentence,t.tatoeba_id,%d as dist from sentences s'
      + ' %s'
      + ' join %s_lang_mapping m on t.lang=m.lang'
      + ' where s.source_type_id=%d'
      + ' and m.language_id<>%d'
      + ' and (sentence_id,language_id) not in (select sentence_id,language_id from translations)'
      ;

    // TODO use prefix i/o "tat"
    firstLevelJoin
      = ' join tat_links k on s.source_id=k.%s'
      + ' join tat_sentences t on k.%s=t.tatoeba_id';

    secondLevelJoin
      = ' join tat_links k1 on s.source_id=k1.%s'
      + ' join tat_links k2 on k1.%s=k2.%s'
      + ' join tat_sentences t on k2.%s=t.tatoeba_id';

    thirdLevelJoin
      = ' join tat_links k1 on s.source_id=k1.%s'
      + ' join tat_links k2 on k1.%s=k2.%s'
      + ' join tat_links k3 on k2.%s=k3.%s'
      + ' join tat_sentences t on k3.%s=t.tatoeba_id';

    // There are ~19 million links. The 4th level is very slow (~ 1/2 hour)
    // But it is still useful, it delivers 4815 translations to current set.
    fourthLevelJoin
      = ' join tat_links k1 on s.source_id=k1.%s'
      + ' join tat_links k2 on k1.%s=k2.%s'
      + ' join tat_links k3 on k2.%s=k3.%s'
      + ' join tat_links k4 on k3.%s=k4.%s'
      + ' join tat_sentences t on k4.%s=t.tatoeba_id';

    logline = 'Translations of level %d (%d rows)';
    id1 = columnFirstId;
    id2 = columnSecondId;


var sql, join : string;
  rows, distance : integer;
begin
  sql := format('insert into sentences(sentence,temp_language_id,source_id,source_type_id)'
    + ' select sentence,%d,tatoeba_id,%d'
    + ' from tat_sentences s join tat_lang_mapping m on s.lang=m.lang'
    + ' where m.language_id = %d and s.tatoeba_id in'
    + ' (select tatoeba_id from tat_sentences_with_audio)',
      [targetLanguageId, SourceTypeIdTatoeba, targetLanguageId]);
  cn.ExecuteDirect(sql);

  // Insert direct translations of sentence in target language, e.g. rus->eng
  distance := 1;
  join := format(firstLevelJoin, [id1, id2]);
  rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
  cn.transaction.Commit;
  writeln(format(logline, [distance, rows]));

  if back then
  begin
    // Insert translations where target language is translation of sentence e.g. eng->rus
    // In the full database it is not necessary because all links are two-way:
    // "The reciprocal link is also present"
    inc(distance);
    join := format(firstLevelJoin, [id2, id1]);
    rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
    cn.transaction.Commit;
    writeln(format(logline, [distance, rows]));
  end;

  if (maxLevel > 0) and (distance >= maxLevel) then exit;

  // Insert indirect translation of sentence in target language, e.g. rus->fra->eng
  inc(distance);
  join := format(secondLevelJoin, [id1, id2, id1, id2]);
  rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
  cn.transaction.Commit;
  writeln(format(logline, [distance, rows]));

  // Insert where sentence in target language is indirect translation, e.g. eng->fra->rus
  if back then
  begin
    inc(distance);
    join := format(secondLevelJoin, [id2, id1, id2, id1]);
    rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
    cn.transaction.Commit;
    writeln(format(logline, [distance, rows]));
  end;
  if (maxLevel > 0) and (distance >= maxLevel) then exit;

  // e.g. rus->fra->spa->eng
  inc(distance);
  join := format(thirdLevelJoin, [id1, id2, id1, id2, id1, id2]);
  rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
  cn.transaction.Commit;
  writeln(format(logline, [distance, rows]));

  if back then
  begin
    inc(distance);
    join := format(thirdLevelJoin, [id2, id1, id2, id1, id2, id1]);
    rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
    cn.transaction.Commit;
    writeln(format(logline, [distance, rows]));
  end;
  if (maxLevel > 0) and (distance >= maxLevel) then exit;

  // e.g. rus->fra->spa->ita->eng
  inc(distance);
  join := format(fourthLevelJoin, [id1, id2, id1, id2, id1, id2, id1, id2]);
  rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
  cn.transaction.Commit;
  writeln(format(logline, [distance, rows]));

  if back then
  begin
    inc(distance);
    join := format(fourthLevelJoin, [id2, id1, id2, id1, id2, id1, id2, id1]);
    rows := ExecuteSql(cn, baseQuery, [distance, join, tablePrefix, SourceTypeIdTatoeba, targetLanguageId]);
    cn.transaction.Commit;
    writeln(format(logline, [distance, rows]));
  end;
end;

function ImportTatoebaSentences(cn : TSqlConnection; ta : TSqlTransaction;
    const folder : string; isNew, keepTemps : boolean;
    maxCsvRows, maxLevel : integer) : boolean;
begin
  result := ImportCsvTables(cn, folder + DirectorySeparator, maxCsvRows);
  if not result then exit;

  if not isNew then
  begin
    ExecuteSql(cn, 'delete from translations where sentence_id in'
      + ' (select sentence_id from sentences where source_type_id=%d)', [SourceTypeIdTatoeba]);
    ExecuteSql(cn, 'delete from sentences where source_type_id=%d', [SourceTypeIdTatoeba]);
    ta.Commit;
  end;

  maxLevel := 2;

  // TODO: specify the languages in the command line arguments
  writeln('Russian');
  InsertIntoLangBathSentenceTables(cn, LanguageIdRussian, maxLevel);
  //writeln('Spanish');
  //InsertIntoLangBathSentenceTables(cn, LanguageIdSpanish, maxLevel);
  //writeln('French');
  //InsertIntoLangBathSentenceTables(cn, LanguageIdFrench, maxLevel);
  //writeln('German');
  //InsertIntoLangBathSentenceTables(cn, LanguageIdGerman, maxLevel);
  writeln('Catalan');
  InsertIntoLangBathSentenceTables(cn, LanguageIdCatalan, maxLevel);
  writeln('Dutch');
  InsertIntoLangBathSentenceTables(cn, LanguageIdDutch, maxLevel);

  writeln('Updating sentences and sounds');
  UpdateTatoebaSentences(cn);
  UpdateTatoebaAudioLinks(cn);
  ta.Commit;

  if not keepTemps then
  begin
    // To avoid this, use -k
    writeln('Deleting temporary tables');
    DeleteTemporaryTatoebaTables(cn);
    ta.Commit;
  end;
end;

end.

