unit lb_import_sentence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function InsertSentence(cn : TSqlConnection;
     sourceTypeId, sourceId : integer;
     const sentence : string;
     sentenceIndex : integer = -1;
     rating : integer = -1) : integer;
procedure InsertTranslation(cn : TSqlConnection;
    sentenceId : integer; languageId : byte; sourceId : integer; const translation : string);
function InsertSound(cn : TSqlConnection; sourceTypeId : integer;
    const filename : string; isHuman : boolean = true) : integer;
function InsertSoundLink(cn : TSqlConnection; sentenceId, soundId : integer;
    timeBegin, timeEnd : double) : integer;

procedure DeleteSentencesFromSource(cn : TSqlConnection; sourceTypeId : integer;
     doDeleteSourceType : boolean = false);


implementation

uses lb_datamodel, lb_lib_string, lb_sql_dml_insert;

function InsertSentence(cn : TSqlConnection; sourceTypeId, sourceId : integer;
     const sentence : string;
     sentenceIndex, rating : integer) : integer;
begin
  result := SqlInsert(cn, 'sentences',
      [cv('source_type_id', sourceTypeId),
       cv('source_id', sourceId),
       cv('sentence', sentence),
       cv('bare_sentence', BareString(sentence)),
       cvif('sentence_index', sentenceIndex >= 0, sentenceIndex),
       cvif('rating', rating >= 0, rating)
       ]);
end;

procedure InsertTranslation(cn : TSqlConnection;
    sentenceId : integer;
    languageId : byte;
    sourceId : integer; const translation : string);
begin
  SqlInsert(cn, 'translations',
     [cv('sentence_id', sentenceId),
      cv('language_id', languageId),
      cv('source_id', sourceId),
      cv('sentence', translation)]);
end;

function InsertSound(cn : TSqlConnection; sourceTypeId : integer;
    const filename : string; isHuman : boolean) : integer;
begin
  result := -1;
  if filename <> '' then
  begin
    result := SqlInsert(cn, 'sounds',
      [cv('source_type_id', sourceTypeId),
       cv('sound_file', filename),
       cv('is_human', ord(isHuman))]);
  end;
end;

function InsertSoundLink(cn : TSqlConnection; sentenceId, soundId : integer;
     timeBegin, timeEnd : double) : integer;
var hasTime : boolean;
begin
  hasTime := (timeBegin >= 0) and (timeEnd >= 0);
  result := SqlInsert(cn, 'lnk_sentences_sounds',
      [cv('sentence_id', sentenceId),
       cv('sound_id', soundId),
       cvif('time_begin', hasTime, timeBegin),
       cvif('time_end', hasTime, timeEnd)]);
end;

procedure DeleteSentencesFromSource(cn : TSqlConnection; sourceTypeId : integer;
       doDeleteSourceType : boolean);
var condition : string;
begin
  condition := ' where source_type_id = ' + IntToStr(sourceTypeId);
  cn.ExecuteDirect('delete from lnk_sentences_sounds'
    + ' where sound_id in (select sound_id from sounds' + condition + ')');
  cn.ExecuteDirect('delete from sentences' + condition);
  cn.ExecuteDirect('delete from translations'
    + ' where sentence_id in (select sentence_id from sentences' + condition + ')');
  cn.ExecuteDirect('delete from sounds' + condition);

  if doDeleteSourceType then
  begin
    cn.ExecuteDirect('delete from source_types' + condition);
  end;
end;

end.

