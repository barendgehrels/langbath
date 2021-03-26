unit lb_datamodel_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

procedure RecreateSentenceTables(cn : TSqlConnection);
procedure RecreateLinkSentenceWordIndexes(cn : TSqlConnection);

implementation

uses lb_sql_ddl, lb_sql_dml_insert, lb_datamodel, lb_const;

procedure RecreateLinkSentenceWordTables(cn : TSqlConnection);
begin
  RecreateTable(cn, KTableNameLnkSentencesWords, 'lnk_sentence_word_id',
    [TableField('sentence_id', ColumnInteger),
    TableField('word_id', ColumnInteger),
    TableField('source_type_id', ColumnInteger), // we might delete it later
    TableField('word_index', ColumnInteger)]);

  RecreateTable(cn, KTableNameLnkSentencesForms, 'lnk_sentence_form_id',
    [TableField('sentence_id', ColumnInteger),
    TableField('form_id', ColumnInteger),
    TableField('source_type_id', ColumnInteger), // might be omitted later
    TableField('word_index', ColumnInteger)]);

  RecreateTable(cn, KTableNameSentencesProperties, 'sentence_property_id',
    [TableField('sentence_id', ColumnInteger),
    TableField('letter_count', ColumnInteger),
    TableField('word_count', ColumnInteger),
    TableField('avg_rank', ColumnFloat),
    TableField('avg_level', ColumnFloat),
    TableField('suitability', ColumnFloat)]);

  RecreateTable(cn, KTableNameMissingWords, 'missing_word_id',
    [TableField('sentence_id', ColumnInteger),
    TableField('word_index', ColumnInteger),
    TableField('bare_word', ColumnString)]);
end;

procedure RecreateLinkSentenceWordIndexes(cn : TSqlConnection);
begin
  RecreateIndex(cn, KTableNameLnkSentencesWords, 'sentence_id');
  RecreateIndex(cn, KTableNameLnkSentencesWords, 'word_id');
  RecreateIndex(cn, KTableNameLnkSentencesWords, ['sentence_id', 'word_index']);
  RecreateIndex(cn, KTableNameLnkSentencesWords, ['sentence_id', 'word_id']);

  //RecreateIndex(cn, KTableNameLnkSentencesWords, ['word_id', 'sentence_id', 'word_index']);
  //RecreateIndex(cn, KTableNameLnkSentencesWords, ['source_type_id', 'sentence_id', 'word_index']);

  RecreateIndex(cn, KTableNameLnkSentencesForms, 'sentence_id');
  RecreateIndex(cn, KTableNameLnkSentencesForms, 'form_id');
  RecreateIndex(cn, KTableNameLnkSentencesForms, ['sentence_id', 'word_index']);

  //RecreateIndex(cn, KTableNameLnkSentencesForms, ['source_type_id', 'sentence_id', 'word_index']);
  //RecreateIndex(cn, KTableNameLnkSentencesWords, ['source_type_id', 'word_id', 'sentence_id']);
  //RecreateIndex(cn, KTableNameLnkSentencesWords, ['sentence_id', 'source_type_id']);

  RecreateIndex(cn, KTableNameSentencesProperties, 'sentence_id');
  RecreateIndex(cn, KTableNameSentencesProperties, 'suitability');
  RecreateIndex(cn, KTableNameSentencesProperties, ['sentence_id', 'suitability', 'letter_count']);

  RecreateIndex(cn, KTableNameMissingWords, 'bare_word');
  RecreateIndex(cn, KTableNameMissingWords, ['sentence_id', 'word_index']);
end;

procedure RecreateSourceTypeTable(cn : TSqlConnection);
begin
  // Shared with words / but needed for Book Sentences, the rest is fixed.
  RecreateTable(cn, 'source_types', 'source_type_id',
    [TableField('title', ColumnString),
    TableField('author', ColumnString),
    TableField('book_id', ColumnString)]);

  SqlInsert(cn, 'source_types',
    [cv('source_type_id', SourceTypeIdTatoeba),
     cv('title', 'Tatoeba')]);

  SqlInsert(cn, 'source_types',
    [cv('source_type_id', SourceTypeIdOpenRussian),
     cv('title', 'Open Russian')]);
end;

procedure RecreateSentenceTables(cn : TSqlConnection);
begin
  RecreateTable(cn, 'sentences', 'sentence_id',
    [TableField('bare_sentence', ColumnString),
    TableField('sentence', ColumnString),
    TableField('permanent_id', ColumnInteger), // preserved when importing
    TableField('source_type_id', ColumnInteger),
    TableField('source_id', ColumnInteger), // tatoeba/open russian id
    TableField('chapter_index', ColumnInteger), // in books, sequential numbering of chapters
    TableField('sentence_index', ColumnInteger), // in books, sequential numbering of sentences
    TableField('rating', ColumnInteger)]);

  RecreateTable(cn, 'translations', 'translation_id',
    [TableField('sentence_id ', ColumnInteger),
    TableField('language_id', ColumnInteger),
    TableField('sentence', ColumnString),
    TableField('distance', ColumnInteger),
    TableField('source_id', ColumnInteger)]);

  RecreateTable(cn, 'sounds', 'sound_id',
    [TableField('source_type_id', ColumnInteger),
    TableField('sound_file', ColumnString),
    TableField('is_human', ColumnInteger)]);

  RecreateTable(cn, 'lnk_sentences_sounds', 'lnk_sentence_sound_id',
    [TableField('sentence_id', ColumnInteger),
    TableField('sound_id', ColumnInteger),
    TableField('time_begin', ColumnFloat),
    TableField('time_end', ColumnFloat)]);

  RecreateIndex(cn, 'sentences', 'source_type_id');
  RecreateIndex(cn, 'sentences', 'permanent_id');
  RecreateIndex(cn, 'sentences', 'rating');
  RecreateIndex(cn, 'sentences', ['sentence_id', 'source_type_id']);

  RecreateIndex(cn, 'translations', 'sentence_id');
  RecreateIndex(cn, 'translations', 'language_id');
  RecreateIndex(cn, 'translations', ['sentence_id', 'language_id']);

  RecreateIndex(cn, 'sounds', ['sound_id', 'is_human']);

  RecreateIndex(cn, 'lnk_sentences_sounds', 'sentence_id');
  RecreateIndex(cn, 'lnk_sentences_sounds', 'sound_id');
  RecreateIndex(cn, 'lnk_sentences_sounds', ['sentence_id', 'sound_id']);

  RecreateLinkSentenceWordTables(cn);
  RecreateSourceTypeTable(cn);
end;

end.

