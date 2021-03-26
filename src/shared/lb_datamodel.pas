// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Shared functionality defining table names

unit lb_datamodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  KTableNameWords : string = 'words';
  KTableNameForms : string = 'forms';

  KTableNameLnkSentencesWords : string = 'lnk_sentences_words';
  KTableNameLnkSentencesForms : string = 'lnk_sentences_forms';
  KTableNameSentencesProperties : string = 'sentences_properties';

  // Words, encountered in sentences, but not in the words database
  // (these might also be names (for now))
  KTableNameMissingWords : string = 'missing_words';

  KTableNameVisualWords : string = 'visual_words';
  KTableNameVisualWordAlternatives : string = 'visual_word_alts';


  KTableNameTemporarySentenceWords : string = 'tmp_sentence_words'; // during import only

  KTableNameSentenceHashesBefore : string = 'tmp_sentence_hashes_before'; // during import only
  KTableNameSentenceHashesAfter : string = 'tmp_sentence_hashes_after'; // during import only


  // Datamodel:
  // TABLE forms: word forms
  //   word_id -> words
  //   person_id -> 1,2,3 (I, you, he)
  //   number_id -> 1,2 (sing,plural)
  //   mood_id -> indicative, subjunctive
  //   aspect_id -> perfect, imperfect
  //   voice_id -> active, passive
  //   tense_id -> 1,2,3 (past, present, future)
  //   case_id -> 1,2,3,4,5,6 (depends on language)
  //   gender_id (male, female)
  //
  //   word: the form itself (RENAME TO "form")
  //   bare_word: without accents (RENAME TO "bare_form")
  //   type_id -> 1=verb
  // TABLE words: words
  //   source_id -> id in OpenRussian
  //   source_type_id -> OpenRussian=2
  //   type_id -> 1,2 (verb,noun) or 0 (other) or null
  //   rank -> from OpenRussian
  //   level -> idem


implementation

end.

