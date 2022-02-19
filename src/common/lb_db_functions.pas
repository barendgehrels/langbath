unit lb_db_functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ExecuteSql(cn : TSqlConnection; const sql : string) : TRowsCount;
function ExecuteSql(cn : TSqlConnection; const sql : string;
  const args : array of const) : TRowsCount;
function QueryAsVariant(cn : TSqlConnection; const sql : string;
  const args : array of const) : variant;
function QueryAsInteger(cn : TSqlConnection; const sql : string;
  const args : array of const; defaultResult : integer = -1) : integer;
function QueryAsString(cn : TSqlConnection; const sql : string;
  const args : array of const; defaultResult : string = '') : string;

implementation

uses variants, LazLoggerBase;

// Function to execute an SQL. It's similar to cn.ExecuteDirect, but it returns the number of
// rows affected.
function ExecuteSql(cn : TSqlConnection; const sql : string) : TRowsCount;
var q : TSQLQuery;
begin
  result := 0;
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := sql;
    q.ExecSql;
    result := q.RowsAffected;
  finally
    q.free;
  end;
end;

function ExecuteSql(cn : TSqlConnection; const sql : string;
  const args : array of const) : TRowsCount;
begin
  result := ExecuteSql(cn, format(sql, args));
end;

function QueryAsVariant(cn : TSqlConnection; const sql : string;
  const args : array of const) : variant;
var q : TSQLQuery;
begin
  result := varnull;
  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := format(sql, args);
    q.Open;

    if not q.eof then
    begin
      result := q.Fields[0].value;
    end;
  finally
    q.free;
  end;
end;

function QueryAsInteger(cn: TSqlConnection; const sql: string;
  const args: array of const; defaultResult : integer): integer;
var v : variant;
begin
  result := defaultResult;
  try
    v := QueryAsVariant(cn, sql, args);
    if v <> varnull then
    begin
      if not TryStrToInt(v, result) then
      begin
        DebugLn('NOT SUCCESSFUL');
        result := defaultResult;
      end;
    end
    else
    begin
      DebugLn('IT IS NULL sql: ' + format(sql, args));
    end;
  except
    on e : Exception do DebugLn(e.message);
    // Ignore any exception (null->string->integer for example)
  end;
end;

function QueryAsString(cn: TSqlConnection; const sql: string;
  const args: array of const; defaultResult: string): string;
var v : variant;
begin
  result := defaultResult;
  v := QueryAsVariant(cn, sql, args);
  if varType(v) = varString then
  begin
    result := v;
  end;
end;

end.

