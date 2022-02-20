unit lb_ini_dictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDictionarySettings = record
    databaseLanguage : string;
    databaseTrain : string;
    folderSoundCache : string;
    UrlOpenRussian : string;
    UrlTatoeba : string;
  end;

function GetDictionarySettings : TDictionarySettings;

implementation

uses IniFiles, lb_config;
const
  KIniFilename = 'dictionary.ini';

  KSection = 'dictionary';
  KEntryDatabaseLanguage = 'database.language';
  KEntryDatabaseTrain = 'database.train';
  KEntryFolderSoundCache = 'folder.soundcache';
  KEntryUrlTatoeba = 'url.tatoeba';
  KEntryUrlOpenRussian = 'url.openrussian';


function GetDictionarySettings: TDictionarySettings;
var ini : TIniFile;
  filename : string;
begin
  filename := format('%s/%s', [ConfigDir, KIniFilename]);
  ini := TIniFile.Create(filename);
  try
    result.databaseLanguage := ini.ReadString(KSection, KEntryDatabaseLanguage, '');
    result.databaseTrain := ini.ReadString(KSection, KEntryDatabaseTrain, '');
    result.folderSoundCache := ini.ReadString(KSection, KEntryFolderSoundCache, '');
    result.UrlOpenRussian := ini.ReadString(KSection, KEntryUrlOpenRussian, '');
    result.UrlTatoeba  := ini.ReadString(KSection, KEntryUrlTatoeba, '');
  finally
    ini.free;
  end;
end;

end.

