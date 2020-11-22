// Language Bath - Common
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This unit a structure keeping book settings, and functionality to read
// and write them from/to ini files (Lazarus ini-files are not dependent
// on Windows)

unit lb_book_settings;

{$mode objfpc}{$H+}

interface

uses Classes;

type
  TBookSettings = record

    iCurrentSentenceIndex : integer;

    // ID (to save it and recognize it later, it should not change)
    iBookId : string;

    // Local filenames
    iFilenameTarget, iFilenameTranslation, iFilenameTimings, iFilenameSound : string;

    // Just documentation
    iAuthor, iTitle, iLanguage : string;

    // Source (usually a URL - it's just documentation)
    iSrcTarget, iSrcTranslation, iSrcSound : string;
  end;

function DefaultBookSettings : TBookSettings;
function ReadBookSettings(const ProjectId : string = '') : TBookSettings;
procedure SaveBookSettings(const settings : TBookSettings; itemIndex : integer);
procedure SaveEditedBookSettings(const settings : TBookSettings);
function ReadBookEntries : TStringList;
function ReadSettingsToStringList(const ProjectId : string; items : TStrings) : integer;

operator = (const left, right: TBookSettings) : boolean;

implementation

uses IniFiles, SysUtils;

const
  KIniFile : string = 'langbath.ini';

  // General settings
  KSection : string = 'assign_timings';
  KEntryCurrentBookId : string = 'current.id'; // TODO maybe make numerical, titles in combo

  // Settings per book (ID is in section name)
  KEntryAuthor : string = 'author';
  KEntryTitle : string = 'title';
  KEntryLanguageTarget : string = 'language.target';

  KEntryTarget : string = 'filename.target';
  KEntryTranslation : string = 'filename.translation';
  KEntryTimings : string = 'filename.timings';
  KEntrySound : string = 'filename.sound';

  KEntrySrcTarget : string = 'src.target';
  KEntrySrcTranslation : string = 'src.translation';
  KEntrySrcSound : string = 'src.sound';

  KEntryCurrentSentence : string = 'sentence.current';

function IniFileName : string;
begin
  result := GetAppConfigDir(false) + KIniFile;
  if not FileExists(result) then
  begin
    result := KIniFile;
  end;
end;

function ReadBookSettings(const ProjectId : string) : TBookSettings;
var ini : TIniFile;
  section, current : string;
begin

  ini := TIniFile.Create(IniFileName);
  try
    if ProjectId = '' then
    begin
      result.iBookId := ini.ReadString(KSection, KEntryCurrentBookId, '');
    end
    else
    begin
      result.iBookId := ProjectId;
    end;

    if result.iBookId <> '' then
    begin
      section := KSection + '.' + result.iBookId;
      current := ini.ReadString(section, KEntryCurrentSentence, '');
      TryStrToInt(current, result.iCurrentSentenceIndex);

      result.iAuthor := ini.ReadString(section, KEntryAuthor, '');
      result.iTitle := ini.ReadString(section, KEntryTitle, '');

      result.iFilenameTarget := ini.ReadString(section, KEntryTarget, '');
      result.iFilenameTranslation := ini.ReadString(section, KEntryTranslation, '');
      result.iFilenameTimings := ini.ReadString(section, KEntryTimings, '');
      result.iFilenameSound  := ini.ReadString(section, KEntrySound, '');

      result.iSrcTarget := ini.ReadString(section, KEntrySrcTarget, '');
      result.iSrcTranslation := ini.ReadString(section, KEntrySrcTranslation, '');
      result.iSrcSound  := ini.ReadString(section, KEntrySrcSound, '');
    end;

  finally
    ini.free;
  end;
end;

procedure SaveEditedBookSettings(const settings : TBookSettings);
var ini : TIniFile;
  section : string;

  procedure SaveSetting(const key, value : string);
  begin
    if value = '' then ini.DeleteKey(section, key)
    else ini.WriteString(section, key, value);
  end;

begin
  ini := TIniFile.Create(IniFileName);
  try
    section := KSection + '.' + settings.iBookId;

    SaveSetting(KEntryAuthor, settings.iAuthor);
    SaveSetting(KEntryTitle, settings.iTitle);
    SaveSetting(KEntryLanguageTarget, settings.iLanguage);

    SaveSetting(KEntryTarget, settings.iFilenameTarget);
    SaveSetting(KEntryTranslation, settings.iFilenameTranslation);
    SaveSetting(KEntryTimings, settings.iFilenameTimings);
    SaveSetting(KEntrySound, settings.iFilenameSound);

    SaveSetting(KEntrySrcTarget, settings.iSrcTarget);
    SaveSetting(KEntrySrcTranslation, settings.iSrcTranslation);
    SaveSetting(KEntrySrcSound, settings.iSrcSound);
  finally
    ini.free;
  end;
end;

procedure SaveBookSettings(const settings : TBookSettings; itemIndex : integer);
var ini : TIniFile;
  section : string;
begin
  ini := TIniFile.Create(IniFileName);
  try
    // Write settings about this book
    if settings.iBookId <> '' then
    begin
      section := KSection + '.' + settings.iBookId;
      if itemIndex >= 0 then
      begin
        ini.WriteInteger(section, KEntryCurrentSentence, itemIndex);
      end
      else
      begin
        ini.DeleteKey(section, KEntryCurrentSentence);
      end;
    end;

    // Write general settings
    ini.WriteString(KSection, KEntryCurrentBookId, settings.iBookId);
  finally
    ini.free;
  end;
end;

function ReadBookEntries : TStringList;
var ini : TIniFile;
  sections : TStringList;
  i : integer;
  s : string;
begin
  result := TStringList.Create;
  sections := TStringList.Create;
  ini := TIniFile.Create(IniFileName);
  try
    ini.ReadSections(sections);

    for i := 0 to sections.count - 1 do
    begin
      s := trim(StringReplace(sections[i], KSection + '.', '', []));
      if s <> '' then
      begin
        result.Append(s);
      end;
    end;
  finally
    sections.free;
    ini.free;
  end;
end;

function ReadSettingsToStringList(const ProjectId : string; items : TStrings) : integer;

  function IsSection(var s : string; const start : string) : boolean;
  var len : integer;
  begin
    len := length(start);
    result := copy(s, 1, len) = start;
    if result then
    begin
      delete(s, 1, len);
    end;
  end;

var ini : TIniFile;
  sections : TStringList;
  i : integer;
  s : string;
begin
  result := -1;

  items.Clear;

  ini := TIniFile.Create(IniFileName);
  try
    sections := TStringList.Create;
    try
      ini.ReadSections(sections);
      for i := 0 to sections.count - 1 do
      begin
        s := sections[i];
        if IsSection(s, KSection + '.') then
        begin
          items.Add(s);
          if s = ProjectId then
          begin
            result := items.Count - 1;
          end;
        end;
      end;
    finally
      sections.free;
    end;
  finally
    ini.free;
  end;
end;


operator = (const left, right: TBookSettings) : boolean;
begin
  result := (left.iBookId = right.iBookId)
    and (left.iAuthor = right.iAuthor)
    and (left.iTitle = right.iTitle)
    and (left.iLanguage = right.iLanguage)
    and (left.iFilenameTarget = right.iFilenameTarget)
    and (left.iFilenameTranslation = right.iFilenameTranslation)
    and (left.iFilenameTimings = right.iFilenameTimings)
    and (left.iFilenameSound = right.iFilenameSound)
    and (left.iSrcTarget = right.iSrcTarget)
    and (left.iSrcTranslation = right.iSrcTranslation)
    and (left.iSrcSound = right.iSrcSound);
end;

function DefaultBookSettings : TBookSettings;
begin
  // All of this is initialized automatically
  result.iBookId := '';
end;

end.

