// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Common functionality to parse parameters (at this moment just a bit) like Python ArgumentParser
// It's an alternative for TCustomApplication (GetOption). I don't use it 
// because it adds a lot of unnecessary overhead to the main program.
// It's an alternative for getopts. I don't use it because its usage is unclear
// and it requires various consts and global variables.
// Using code from this unit gives readable code, and it's still powerful
// despite the fact that its implementation is relatively concise.

unit lb_argument_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TProgramArgument = record
    required : boolean; // change to option
    isDefault : boolean;
    asChar : string;
    asWord : string;
    explanation : string;
    // TODO: a set of options: [paRequired, paInteger, paFloat, paBool,
    //   paExistingFile, paExistingFolder, paNewFile]
  end;

  TArrayOfProgramArgument = array of TProgramArgument;
  TArrayOfVariant = array of variant;

// Allows writing arguments in concise arrays, for example:
// options := [Option('h', 'help'), Option('i', 'input', true, 'Input filename')];
function Option(const c, w : string; required : boolean = false;
  const e : string = ''; isDefault : boolean = false) : TProgramArgument;

// Writes all options on the console
procedure PrintHelp(const a : array of TProgramArgument; const header : string = 'Usage');

// Debug functionality, write all specified arguments
procedure PrintValues(const a: array of TProgramArgument; const v : TArrayOfVariant);

// Returns true if all arguments marked as required are specified.
// If not, a line (per argument) is writen
function HasAllRequiredOptions(const a : array of TProgramArgument; const v : TArrayOfVariant) : boolean;

// Parses the parameters (1..paramcount) and sets the result into a value array
// Returns false if there is input not as specified in the arguments
function ParseArguments(const a : array of TProgramArgument; out v : TArrayOfVariant) : boolean;

function VariantAsBoolean(v : variant) : boolean;
function VariantAsInteger(v : variant; defaultValue : integer = -1) : integer;

implementation

uses Variants;

function Option(const c, w : string; required : boolean; const e : string;
   isDefault : boolean) : TProgramArgument;
begin
  assert(length(c) = 1);
  assert(length(w) > 1);

  result.required := required;
  result.isDefault := isDefault;
  result.asChar := c;
  result.asWord := w;
  result.explanation := e;
end;

procedure PrintHelp(const a : array of TProgramArgument; const header : string);
var i : integer;
  s : string;
begin
  s := '';
  for i := low(a) to high(a) do
  begin
    s := s + ' ';
    if not a[i].required then s := s + '{';
    s := s + '-' + a[i].asChar + ' ...';
    if not a[i].required then s := s + '}';
  end;
  writeln(Header, ': ', ExtractFileName(paramstr(0)), s);
  for i := low(a) to high(a) do
  begin
    writeln('  -', a[i].asChar, ' (--', a[i].asWord, ') : ', a[i].explanation);
  end;
end;

function ArgIndex(const a: array of TProgramArgument; const s : string) : integer;
var i : integer;
  arg : string;
  isWord : boolean;
begin
  arg := '';
  result := -1;
  isWord := false;
  if not s.StartsWith('-') then exit;
  if s.StartsWith('--') then
  begin
    arg := copy(s, 3, length(s));
    isWord := true;
  end
  else if length(s) = 2 then
  begin
    arg := copy(s, 2, 1);
  end
  else
  begin
    exit;
  end;

  for i := low(a) to high(a) do
  begin
    if (isWord and (arg = a[i].AsWord))
    or (not isWord and (arg = a[i].asChar))
    then
    begin
      result := i;
      exit;
    end;
  end;
end;

function ParseArguments(const a: array of TProgramArgument; out v : TArrayOfVariant): boolean;
var p, i : integer;
begin
  result := true;

  v := [];
  SetLength(v, length(a));
  for i := low(v) to high(v) do v[i] := unassigned;

  if ParamCount = 1 then
  begin
    for i := low(a) to high(a) do
    begin
      if a[i].isDefault then
      begin
        v[i] := ParamStr(1);
        exit;
      end;
    end;
  end;

  p := 1;
  while p <= ParamCount do
  begin
    i := ArgIndex(a, paramstr(p));
    if i < 0 then result := false
    else
    begin
      if (p + 1 <= paramcount) and (ArgIndex(a, paramstr(p + 1)) = -1) then
      begin
        v[i] := paramstr(p + 1);
        inc(p);
      end
      else
      begin
        // It's the last option, or the next token is also an option. Set to true.
        v[i] := true;
      end;
    end;
    inc(p);
  end;
end;

function VariantAsBoolean(v: variant): boolean;
begin
  if varType(v) = varBoolean then result := v else result := false;
end;

function VariantAsInteger(v: variant; defaultValue: integer): integer;
begin
  if not TryStrToInt(v, result) then result := defaultValue;
end;

procedure PrintValues(const a: array of TProgramArgument; const v : TArrayOfVariant);
var i : integer;
begin
  for i := low(a) to high(a) do
  begin
    if varType(v[i]) <> varEmpty then
    begin
      writeln('  Option --', a[i].asWord, ' : ', v[i]);
    end;
  end;
end;

function HasAllRequiredOptions(const a: array of TProgramArgument;
  const v: TArrayOfVariant): boolean;
var i : integer;
  s : string;
begin
  result := false;
  if length(a) <> length(v) then exit;

  result := true;
  for i := low(a) to high(a) do
  begin
    if a[i].required then
    begin
      if varType(v[i]) <> varEmpty then s := v[i] else s := '';
      if s = '' then
      begin
        Writeln('Option -', a[i].AsChar , ' --', a[i].AsWord, ' (', a[i].Explanation, ')  is missing');
        result := false;
      end;
    end;
  end;
end;

end.
