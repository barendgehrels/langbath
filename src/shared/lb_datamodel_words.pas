// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Shared functionality to create tables for words and word forms

unit lb_datamodel_words;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

procedure RecreateWordTables(cn : TSqlConnection);
procedure RecreateWordIndices(cn : TSqlConnection);

implementation

uses lb_sql_ddl, lb_datamodel;

procedure RecreateWordTables(cn: TSqlConnection);
begin
  RecreateTable(cn, KTableNameWords, 'word_id',
    [TableField('word', ColumnString),
    TableField('bare_word', ColumnString),
    TableField('source_type_id', ColumnInteger),
    TableField('source_id', ColumnInteger),
    TableField('permanent_id', ColumnInteger),
    TableField('type_id', ColumnInteger),
    TableField('rank', ColumnInteger),
    TableField('level', ColumnString)]);

  RecreateTable(cn, KTableNameForms, 'form_id',
    [TableField('word_id', ColumnInteger),
    TableField('source_type_id', ColumnInteger),
    TableField('word', ColumnString),
    TableField('bare_word', ColumnString),
    TableField('type_id', ColumnInteger),
    TableField('person_id', ColumnInteger),
    TableField('number_id', ColumnInteger),
    TableField('tense_id', ColumnInteger),
    TableField('case_id', ColumnInteger),
    TableField('gender_id', ColumnInteger),
    TableField('mood_id', ColumnInteger),
    TableField('aspect_id', ColumnInteger),
    TableField('voice_id', ColumnInteger),
    TableField('animacy_id', ColumnInteger),
    TableField('specific_id', ColumnInteger),
    TableField('subrank', ColumnInteger)]);
end;

procedure RecreateWordIndices(cn: TSqlConnection);
begin
  RecreateIndex(cn, KTableNameWords, 'bare_word');
  RecreateIndex(cn, KTableNameWords, 'source_id');
  RecreateIndex(cn, KTableNameWords, 'source_type_id');
  RecreateIndex(cn, KTableNameWords, 'type_id');
  RecreateIndex(cn, KTableNameWords, 'level');
  RecreateIndex(cn, KTableNameWords, 'rank');

  RecreateIndex(cn, KTableNameWords, ['type_id', 'rank']);

  // Foreign key
  RecreateIndex(cn, KTableNameForms, 'word_id');

  RecreateIndex(cn, KTableNameForms, 'bare_word');
  RecreateIndex(cn, KTableNameForms, 'source_type_id');
  RecreateIndex(cn, KTableNameForms, 'type_id');
  RecreateIndex(cn, KTableNameForms, 'person_id');
  RecreateIndex(cn, KTableNameForms, 'case_id');
  RecreateIndex(cn, KTableNameForms, 'number_id');

  RecreateIndex(cn, KTableNameForms, ['word_id', 'case_id', 'number_id']);
end;

end.

