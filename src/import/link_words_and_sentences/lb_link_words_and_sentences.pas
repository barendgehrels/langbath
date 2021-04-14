unit lb_link_words_and_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

procedure UpdateLinkSentencesWords(cn: TSqlConnection; sourceTypeId : integer; keepTemps : boolean);

implementation

uses lb_datamodel, lb_datamodel_sentences,
  lb_db_functions, lb_lib_string, lb_lib, lb_types,
  lb_sql_dml_insert, lb_sql_ddl;

procedure BareLinkSentencesWords(cn: TSqlConnection; sourceTypeId : integer);
var
  q  : TSqlQuery;
  bareSentence : string;
  sentenceId, k, wordIndex : integer;
  v : double;
  bag : TArrayOfString;
  t0 : double;
  sql : string;
  inserter : TSqlInserter;
  vars : array of variant;

begin
  t0 := now;

  sql := 'select sentence_id, source_type_id, bare_sentence from sentences';
  if sourceTypeId > 0 then
  begin
    sql := sql + format(' where source_type_id=%d', [sourceTypeId]);
  end;

  q := TSQLQuery.Create(nil);
  inserter := TSqlInserter.Create(cn);

  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.Open;

    inserter.Prepare(KTableNameTemporarySentenceWords,
      ['bare_word', 'sentence_id', 'word_index']);

    while not q.eof do
    begin
      sentenceId := q.Fields[0].AsInteger;
      sourceTypeId := q.Fields[1].AsInteger;
      bareSentence := q.Fields[2].AsString;

      bag := CreateBagOfWords(bareSentence);
      wordIndex := 1;
      for k := low(bag) to high(bag) do
      begin
        if not TryStrToFloat(bag[k], v) then
        begin
          // It's a word, not a number.
          // TODO: we might verify if the characters correspond to the current language.

          vars := [bag[k], sentenceId, wordIndex];
          inserter.bind(vars);
          inserter.Execute;

          inc(wordIndex);
        end;
      end;

      q.Next();
    end;
  finally
    q.free;
    inserter.free;
  end;

  log(format('Barelink - time: %f', [24.0 * 3600.0 * (now - t0)]));
end;

procedure LinkSentencesWords(cn: TSqlConnection; sourceTypeId : integer);

  function JoinClause(const table, prefix : string) : string;
  begin
    result := format('join %s %s on s.bare_word=%s.bare_word', [table, prefix, prefix])
  end;

var t0 : double;
  sql : string;
  rows : integer;
begin
  t0 := now;

  // First forms, then words, then additional words from forms
  ExecuteSql(cn, 'delete from %s where source_type_id=%d',
     [KTableNameLnkSentencesForms, sourceTypeId]);

  sql := format('select %d,sentence_id,word_index,form_id from %s s %s',
    [sourceTypeId, KTableNameTemporarySentenceWords, JoinClause(KTableNameForms, 'f')]);
  log(sql);

  rows := ExecuteSql(cn, 'insert into %s(source_type_id,sentence_id,word_index,form_id) %s',
      [KTableNameLnkSentencesForms, sql]);
  log(format('Link a (%d) - time: %f', [rows, 24.0 * 3600.0 * (now - t0)]));
  t0 := now;

  // Words
  ExecuteSql(cn, 'delete from %s where source_type_id=%d',
    [KTableNameLnkSentencesWords, sourceTypeId]);

  sql := format('select %d,sentence_id,word_id,word_index from %s s %s',
      [sourceTypeId, KTableNameTemporarySentenceWords, JoinClause(KTableNameWords, 'w')]);
  //log(sql);
  rows := ExecuteSql(cn, 'insert into %s(source_type_id,sentence_id,word_id,word_index) %s',
      [KTableNameLnkSentencesWords, sql]);
  log(format('Link b (%d) - time: %f', [rows, 24.0 * 3600.0 * (now - t0)]));
  t0 := now;

  // Insert remaining words from forms, where nominal form of word was not yet found
  sql := format('select distinct %d,k.sentence_id,f.word_id,k.word_index from %s k'
    + ' join forms f on k.form_id=f.form_id'
    + ' left join %s kw on k.sentence_id=kw.sentence_id and k.word_index = kw.word_index'
    + ' where k.source_type_id=%d and kw.word_id is null',
    [sourceTypeId,KTableNameLnkSentencesForms, KTableNameLnkSentencesWords, sourceTypeId]);
  log(sql);
  rows := ExecuteSql(cn, 'insert into %s(source_type_id,sentence_id,word_id,word_index) %s',
    [KTableNameLnkSentencesWords, sql]);
  log(format('Link c (%d) - time: %f', [rows, 24.0 * 3600.0 * (now - t0)]));
end;

procedure AnalyseSentencesWords(cn: TSqlConnection; sourceTypeId: integer);
var commonQuery, sql : string;
  t0 : double;
begin
  t0 := now;
  cn.ExecuteDirect(format('delete from %s where sentence_id in (select sentence_id from sentences where source_type_id=%d)',
  [KTableNameSentencesProperties, sourceTypeId]));

  commonQuery := format('select b.bare_word,b.sentence_id,b.word_index' + #13
    + ',length(s.bare_sentence) as letter_count,s.bare_sentence,w.word_id,w.rank,w.level' + #13
    + ',case'
    + ' when w.level=''A1'' then 1 when w.level=''A2'' then 2'
    + ' when w.level=''B1'' then 3 when w.level=''B2'' then 4'
    + ' when w.level=''C1'' then 5 when w.level=''C2'' then 6'
    + ' else 7 end as numlevel' + #13
    + ' from %s b' + #13
    + ' left join %s k on b.sentence_id=k.sentence_id and b.word_index=k.word_index' + #13
    + ' left join sentences s on b.sentence_id=s.sentence_id' + #13
    + ' left join words w on k.word_id=w.word_id',
    //+ ' where b.source_type_id=%d',
    [KTableNameTemporarySentenceWords, KTableNameLnkSentencesWords]);//, sourceTypeId]);

  sql := format('select sentence_id'
    + ',max(word_index) as word_count'
    + ',letter_count' + #13
    //+ ',avg(length(bare_word)) as avg_letters_per_word'
    + ',avg(rank) as avg_rank'
    + ',avg(numlevel) as avg_level' + #13
    + ',max(word_index)/10 + abs(50-letter_count)/15 + avg(length(bare_word)) + avg(rank)/1000 + avg(numlevel) as suitability' + #13
    + ' from (%s) group by sentence_id', [commonQuery]);

  //log_query(sql);

  cn.ExecuteDirect(format('insert into %s(sentence_id,word_count,letter_count,avg_rank,avg_level,suitability) %s',
    [KTableNameSentencesProperties, sql]));

  //LbData.Commit(DbIdLanguage);
  log(format('Analyse - time: %f', [24.0 * 3600.0 * (now - t0)]));
end;

procedure AddMissingWords(cn: TSqlConnection);
var sql : string;
  rows : integer;
  t0 : double;
begin
  t0 := now;
  sql := format('select t.sentence_id,t.word_index,t.bare_word from %s t' + #13
      + ' left join %s k' + #13
      + ' on t.sentence_id=k.sentence_id and t.word_index=k.word_index' + #13
      + ' where k.word_id is null',
      [KTableNameTemporarySentenceWords, KTableNameLnkSentencesWords]);

  //log_query(sql);

  rows := ExecuteSql(cn, 'insert into %s(sentence_id, word_index, bare_word) %s',
     [KTableNameMissingWords, sql]);
  writeln('Missing words: ', rows);
  log(format('Missing words - time: %f', [24.0 * 3600.0 * (now - t0)]));

  //LbData.Commit(DbIdLanguage);
end;

procedure UpdateLinkSentencesWords(cn: TSqlConnection; sourceTypeId : integer; keepTemps : boolean);
var
  t0 : double;
begin
  t0 := now;
  log(format('BEGIN UPDATE LINK - source_type_id=%d', [sourceTypeId]));

  RecreateTable(cn, KTableNameTemporarySentenceWords, 'tmp_id',
    [TableField('bare_word', ColumnString),
    TableField('sentence_id', ColumnInteger),
    TableField('word_index', ColumnInteger)]);

  BareLinkSentencesWords(cn, sourceTypeId);
  LinkSentencesWords(cn, sourceTypeId);
  AnalyseSentencesWords(cn, sourceTypeId);
  AddMissingWords(cn);
  log(format('END UPDATE LINK - source_type_id=%d time: %f', [sourceTypeId, 24.0 * 3600.0 * (now - t0)]));

  // delete the temp table
  if not KeepTemps then
  begin
    ExecuteSql(cn, 'drop table %s', [KTableNameTemporarySentenceWords]);
  end;

  // (re)create the index(es)
  RecreateLinkSentenceWordIndexes(cn);
end;

end.

