unit lb_import_sentences_from_books;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

procedure ImportSentencesFromIni(cn : TSqlConnection; const iniFileName : string);


implementation

uses lb_datamodel,
  lb_import_sentence, lb_sql_dml_insert,
  lb_book_settings, lb_const, lb_lib;

procedure ImportSentencesFromBook(cn : TSqlConnection; sourceTypeId : integer; const settings : TBookSettings);
var list: TStringList;
  translations, timings : TStringList;
  k, p : integer;
  sentenceId, soundId : integer;
  sentence : string;
  hasTiming : boolean;
  timeBegin, timeEnd : double;
  rating : integer;
begin
  assert(FileExists(settings.iFilenameTarget));

  list := TStringList.Create;
  translations := TStringList.Create;
  timings := TStringList.Create;
  try
    list.LoadFromFile(settings.iFilenameTarget);

    if list.count = 0 then exit;

    soundId := InsertSound(cn, sourceTypeId, settings.iFilenameSound);

    if (settings.iFilenameTranslation <> '') and FileExists(settings.iFilenameTranslation) then translations.LoadFromFile(settings.iFilenameTranslation);
    if (settings.iFilenameTimings <> '') and FileExists(settings.iFilenameTimings) then timings.LoadFromFile(settings.iFilenameTimings);

    if ((translations.count > 0) and (list.count <> translations.count))
    or ((timings.count > 0) and (list.count <> timings.count))
    then
    begin
      // They should be in sync.
      exit;
    end;

    for k := 0 to list.count - 1 do
    begin
      sentence := trim(list[k]);

      // Skip commented lines (TODO: add chapters from them if applicable)
      p := pos('#', sentence);
      if p <> 1 then
      begin
        rating := 0;
        hasTiming :=
          (k < timings.Count)
          and SplitTimings(timeBegin, timeEnd, rating, timings[k])
          and (timeEnd > timeBegin);

        // FOR NOW: only with timing - sentences are otherwise useless and sampling is infinite (bug)
        if hasTiming then
        begin
          sentenceId := InsertSentence(cn, sourceTypeId, k + 1, sentence, k + 1, rating);
          if k < translations.Count then
          begin
            InsertTranslation(cn, sentenceId,
              LanguageIdEnglish, // TODO
              k + 1, translations[k]);
          end;

          InsertSoundLink(cn, sentenceId, soundId, timeBegin, timeEnd);
        end;
      end;
    end;

  finally
    list.Free;
    translations.Free;
    timings.Free;
  end;

end;

procedure ImportSentencesFromIni(cn : TSqlConnection; const iniFileName : string);
var entries : TStringList;
  i : integer;
  settings : TBookSettings;
begin
  entries := ReadBookEntries(iniFileName);
  try
    for i := 0 to entries.count - 1 do
    begin
      settings := ReadBookSettings(iniFileName, entries[i]);

      if (settings.iBookId <> '')
      and (settings.iFilenameTarget <> '')
      and FileExists(settings.iFilenameTarget)
      then
      begin
        writeln('Import book ', settings.iBookId, ' into ', settings.iSourceTypeId);
        log('BEGIN IMPORT ' + settings.iBookId);

        // Delete earlier sentences, if any, sounds, etc, and its source_type_id
        DeleteSentencesFromSource(cn, settings.iSourceTypeId, true);

        // Insert a new source_type_id and all sentences
        SqlInsert(cn, 'source_types',
          [cv('source_type_id', settings.iSourceTypeId),
           cv('book_id', settings.iBookId),
           cv('title', settings.iTitle),
           cv('author', settings.iAuthor)]);

        ImportSentencesFromBook(cn, settings.iSourceTypeId, settings);

        log('END IMPORT ' + settings.iBookId);
      end;
    end;
  finally
    entries.free;
  end;
end;


end.

