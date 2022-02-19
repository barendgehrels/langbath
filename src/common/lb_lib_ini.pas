unit lb_lib_ini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Inifiles;

function IniFileName : string;
function IniReadInteger(ini : TIniFile; const section, entry : string; def : integer) : integer;
function IniReadFloat(ini : TIniFile; const section, entry : string; def : double) : double;
function IsSection(var s : string; const start : string) : boolean;

implementation

uses lb_lib;

function IniFileName : string;
begin
  result := ConfigDir + 'langbath.ini';
end;

function IniReadInteger(ini : TIniFile; const section, entry : string; def : integer) : integer;
var s : string;
begin
  result := def;
  s := ini.ReadString(section, entry, IntToStr(def));
  TryStrToInt(s, result);
end;

function IniReadFloat(ini : TIniFile; const section, entry : string; def : double) : double;
var s : string;
begin
  result := def;
  s := ini.ReadString(section, entry, FloatToStr(def));
  TryStrToFloat(s, result);
end;

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


end.

