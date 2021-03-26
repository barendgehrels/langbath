unit lb_sql_dml_insert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

type

  { TColumnValue }

  TColumnValue  = record
    column : string;
    value : variant;
    isBlob : boolean;
  end;

  TArrayOfColumnValue = array of TColumnValue;

  { TSqlInserter }

  TSqlInserter = class
    public
      query : TSqlQuery;

      procedure AddColumn(var partColumns, partValues : string; const column : string);
      function CreateSql(const table, partColumns, partValues : string) : string;

    public

      constructor Create(cn : TSQLConnection);
      destructor Destroy; override;

      procedure Prepare(const table: string; const columnNames: array of string);
      procedure Bind(const columnValues: array of variant);
      procedure Bind(const columnValues: array of string);

      procedure Prepare(const table : string; const columnValues : array of TColumnValue);
      procedure Bind(const columnValues: array of TColumnValue);

      procedure Execute;
  end;


// Main function: uses behind the scenes the TSqlInserter and prepares, fills and executes
// a query using specified table name and column names / values
// It returns (if it is SQLite) the last inserted ID (or 0)
function SqlInsert(cn : TSQLConnection; const table : string;
    const columnValues : array of TColumnValue) : int64;

// Creates a TColumnValue record with a column and any type of value
function cv(const col : string; const val : variant): TColumnValue;

// Creates a TColumnValue record with a column and either the value or a null
function cvif(const col : string; condition : boolean; val : variant) : TColumnValue;

// Creates a TColumnValue record with a column and a filename from which blob will be stored
function cblob(const col: string; const path: string): TColumnValue;


// TEMP
function SqlCreateMaxQuery(const table, id : string) : string;

implementation

uses Db, SqLite3Conn;

function SqlCreateMaxQuery(const table, id : string) : string;
begin
  result := format('select max(%s) from %s', [id, table]);
end;

{ TSqlInserter }

constructor TSqlInserter.Create(cn: TSQLConnection);
begin
  query := TSqlQuery.Create(nil);
  query.Database := cn;
end;

destructor TSqlInserter.Destroy;
begin
  query.free;
  inherited destroy;
end;

procedure TSqlInserter.AddColumn(var partColumns, partValues: string; const column: string);
begin
  if partColumns = '' then
  begin
    partColumns := column;
    partValues := ':' + column;
  end
  else
  begin
    partColumns := partColumns + ', ' + column;
    partValues := partValues + ', :' + column;
  end;
end;

function TSqlInserter.CreateSql(const table, partColumns, partValues: string): string;
begin
  result := format('insert into %s(%s) values(%s)', [table, partColumns, partValues]);
end;

procedure TSqlInserter.Prepare(const table: string; const columnNames: array of string);
var i : integer;
  partColumns : string;
  partValues : string;
begin
  partColumns := '';
  partValues := '';
  for i := low(columnNames) to high(columnNames) do
  begin
    AddColumn(partColumns, partValues, columnNames[i]);
  end;

  query.Sql.Text := CreateSql(table, partColumns, partValues);
  query.Prepare;
end;

procedure TSqlInserter.Prepare(const table: string; const columnValues: array of TColumnValue);
var i : integer;
  partColumns : string;
  partValues : string;
begin
  partColumns := '';
  partValues := '';
  for i := low(columnValues) to high(columnValues) do
  begin
    AddColumn(partColumns, partValues, columnValues[i].column);
  end;

  query.Sql.Text := CreateSql(table, partColumns, partValues);
  query.Prepare;
end;

procedure TSqlInserter.Bind(const columnValues: array of TColumnValue);
var i : integer;
begin
  for i := low(columnValues) to high(columnValues) do
  begin
    if columnValues[i].isBlob then
    begin
      query.Params.Items[i].LoadFromFile(columnValues[i].value, ftBlob)
    end
    else
    begin
      query.Params.Items[i].value := columnValues[i].value;
    end;
  end;
end;

procedure TSqlInserter.Bind(const columnValues: array of variant);
var i : integer;
begin
  for i := low(columnValues) to high(columnValues) do
  begin
    query.Params.Items[i].value := columnValues[i];
  end;
end;

procedure TSqlInserter.Bind(const columnValues: array of string);
var i : integer;
begin
  for i := low(columnValues) to high(columnValues) do
  begin
    query.Params.Items[i].value := columnValues[i];
  end;
end;

procedure TSqlInserter.Execute;
begin
  query.ExecSql;
end;

function SqlInsert(cn: TSQLConnection; const table: string;
  const columnValues: array of TColumnValue) : int64;
var inserter : TSqlInserter;
begin
  inserter := TSqlInserter.Create(cn);
  try
    inserter.Prepare(table, columnValues);
    inserter.Bind(columnValues);
    inserter.Execute;
  finally
    inserter.Free;
  end;
  if (cn is TSqlite3Connection) then result := (cn as TSQLite3Connection).GetInsertID
  else result := -1;
end;

function cv(const col: string; const val: variant): TColumnValue;
begin
  result.column := col;
  result.value := val;
  result.isBlob := false;
end;

function cvif(const col: string; condition: boolean; val: variant): TColumnValue;
begin
  if condition then result := cv(col, val) else result := cv(col, null);
end;

function cblob(const col: string; const path: string): TColumnValue;
begin
  result.column := col;
  result.value := path;
  result.isBlob := true;
end;

end.

