// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// This unit defines some const values used in various other units

unit lb_const;

{$mode objfpc}{$H+}

interface

const
  DbIdLanguage = 1;
  DbIdUser = 2;


  // Constants for values in the table "forms"
  // type_id (verb, noun, adverb, etc)
  // person_id (first, second and third person for verbs)
  // number_id (singular, plural)
  // tense_id (past, present, future)
  // aspect_id (Spanish/Russian, imperfective, perfective)
  // voice_id (active, passive)
  // mood_id (imperative, indicative, subjunctive)
  CurrentUserId : byte = 1; // TODO: get from database

  // Defined from
  //   https://en.wikipedia.org/wiki/Part_of_speech#Western_tradition
  // and partly used in OpenRussian
  // we have noun/verb reversed (CAN STILL BE FIXED)
  //
  // noun
  // verb
  // adjective
  // adverb
  // pronoun
  // preposition
  // conjunction
  // interjection
  // article or (more recently) determiner

  TypeIdUnknown = 0;
  TypeIdVerb = 1;
  TypeIdNoun = 2;
  TypeIdAdjective = 3;
  TypeIdAdverb = 4;
  TypeIdPronoun = 5;
  TypeIdPreposition = 6;
  TypeIdName = 10;
  TypeIdExpression = 11; // for OpenRussian, it's a combination of words and could be omitted
  TypeIdOther = 12; // some from OpenRussian are reassigned

  GroupTypeIdVerbs : integer = 1;
  GroupTypeIdNouns : integer = 2;
  GroupTypeIdVisual : integer = 3;
  GroupTypeIdAudio : integer = 4;

  CaseIdNone : integer = -1;

  PersonId1 : integer = 1;
  PersonId2 : integer = 2;
  PersonId3 : integer = 3;

  NumberIdNone : integer = -1;
  NumberIdSingular : integer = 1;
  NumberIdPlural : integer = 2;

  TenseIdPast : integer = 1;
  TenseIdPresent : integer = 2;
  TenseIdFuture : integer = 3;

  AspectIdImperfective : integer = 1;
  AspectIdPerfective : integer = 2;

  MoodIdIndicative : integer = 1;
  MoodIdImperative : integer = 2;
  MoodIdSubjunctive : integer = 3;

  GenderIdNone = -1;
  GenderIdIrrelevant = 0;
  GenderIdMale = 1;
  GenderIdFemale = 2;
  GenderIdNeutral = 3;

  AnimacyIdInanimate : integer = 0;
  AnimacyIdAnimate : integer = 1;

  TaxonomicIdSynonym : integer = 1; // synonym
  TaxonomicIdHyperonym : integer = 2; // it's broader, e.g. monkey = hyperonym(chimp)
  TaxonomicIdHyponym : integer = 3; // it's smaller, e.g. chimp=hyponym(monkey)
  TaxonomicIdGender : integer = 4; // it's the same animal but the other gender (cow/bull)

  SpecificIdComparative : integer = 1;
  SpecificIdSuperlative : integer = 2;
  SpecificIdShortForm : integer = 3;

  SourceTypeIdTatoeba = 1;
  SourceTypeIdOpenRussian = 2;
  SourceTypeIdWiktionary = 3;
  SourceTypeIdLangBath = 9; // langbath itself for additional words
  SourceTypeIdMaxFixed = 10;

  // Numeric values are taken over from
  // http://devlib.symbian.slions.net/s3/GUID-31C133DE-F245-5992-9A41-20A99291E72A.html
  // but in range [1..255]
  LanguageIdEnglish : byte = 1;
  LanguageIdFrench : byte = 2;
  LanguageIdGerman : byte = 3;
  LanguageIdSpanish : byte = 4;
  LanguageIdItalian : byte = 5;
  LanguageIdSwedish : byte = 6;
  LanguageIdDanish : byte = 7;
  LanguageIdNorwegian : byte = 8;
  LanguageIdPortuguese : byte = 13;
  LanguageIdRussian : byte = 16;
  LanguageIdDutch : byte = 18;
  LanguageIdCzech : byte = 25;
  LanguageIdSlovenian : byte = 28;
  LanguageIdCatalan : byte = 44;
  LanguageIdSwahili : byte = 84;
  LanguageIdUkrainian : byte = 93;

  LanguageIdLatin : byte = 200; // not in that list

function NumberLabel(numberId : integer): string;

implementation

function NumberLabel(numberId : integer): string;
begin
  if numberId = NumberIdSingular then result := 'sg'
  else if numberID = NumberIdPlural then result := 'pl'
  else result := '';
end;

end.

