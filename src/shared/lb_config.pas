unit lb_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IniFileName : string;
function ConfigDir : string;
function AssureConfigFolder : boolean;

implementation

function IniFileName : string;
begin
  result := ConfigDir + 'langbath.ini';
end;

function ConfigDir : string;
begin
  // Put all configuration of all programs in the suite
  // in just one place: (windows /users/xxx/AppData/Local/langbath/)
  result := StringReplace(GetAppConfigDir(false), ApplicationName, 'langbath',
         [rfReplaceAll, rfIgnoreCase])
end;

function AssureConfigFolder : boolean;
var folder : string;
begin
  folder := ConfigDir;
  result := DirectoryExists(folder);
  if not result then result := CreateDir (folder);
end;


end.

