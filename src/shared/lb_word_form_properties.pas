unit lb_word_form_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // A word, its form, and some properties
  // Fields can be left empty
  TWordFormProperties = record
    wordId : integer; // primary key (word_id) in database table "words"
    formId : integer; // primary key (form_id) in database table "forms"

    typeId : integer;      // noun,verb,..., from table "words"
    permanentId : integer; // of word, from table "words"
    caseId : integer;      // for nouns, from table "forms"
    numberId : integer;    // singular, plural, for nouns and verbs, from table "forms"
    tenseId : integer;     // for verbs, from table "forms"
    personId : integer;    // for verbs, from table "forms"
    // mood, gender, aspect

    wordForm : string; // the word itself, from table "forms" or (if not found) from "words"

    correct : boolean; // for convenience, for UI applications needing it
  end;

  TArrayOfWordFormProperties = array of TWordFormProperties;


implementation

end.

