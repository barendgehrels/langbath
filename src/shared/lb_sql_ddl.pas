// Language Bath - Common
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// This unit provides functionality for SQL Data Definition Language (create, drop)

unit lb_sql_ddl;

{$mode objfpc}{$H+}

interface

uses
  SqlDb;

type
  TColumnType = (ColumnInteger, ColumnFloat, ColumnString, ColumnBlob);

procedure RecreateTable(cn : TSqlConnection; const tablename, idname : string;
    const columns : array of string);
procedure RecreateIndex(cn : TSqlConnection; const tablename : string;
    const columns : array of string);

procedure RecreateIndex(cn : TSqlConnection; const tablename, column : string);

procedure DropIndex(cn : TSqlConnection; const indexname : string);

function TableField(const columnName : string; columnType : TColumnType): string;


implementation

uses SysUtils;

procedure RecreateTable(cn : TSqlConnection; const tablename, idname : string;
  const columns : array of string);
const pk : string = 'INTEGER PRIMARY KEY AUTOINCREMENT';
var c : string;
  i : integer;
begin
  cn.ExecuteDirect('drop table if exists ' + tablename);

  // Create a comma separated string
  c := '';
  for i := low(columns) to high(columns) do
  begin
    c := c + ', ' + columns[i];
  end;

  cn.ExecuteDirect(format('create table %s (%s %s %s)' , [tablename, idname, pk, c]));
end;


procedure DropIndex(cn : TSqlConnection; const indexname : string);
begin
  cn.ExecuteDirect(format('drop index if exists %s', [indexname]));
end;

procedure RecreateIndex(cn : TSqlConnection; const tablename, indexname, columns : string);
begin
  cn.ExecuteDirect(format('drop index if exists %s', [indexname]));
  cn.ExecuteDirect(format('create index %s on %s (%s)',
    [indexname, tablename, columns]));
end;

procedure RecreateIndex(cn : TSqlConnection; const tablename: string;
      const columns: array of string);
var indexName, columnNames : string;
  i : integer;
begin
  columnNames := '';
  indexName := 'idx_' + tablename + '_';
  for i := low(columns) to high(columns) do
  begin
    if i > low(columns) then
    begin
      columnNames := columnNames + ',';
      indexName := indexName + '_plus_';
    end;
    columnNames := columnNames + columns[i];
    indexName := indexName + columns[i];
  end;
  RecreateIndex(cn, tableName, indexName, columnNames);
end;

procedure RecreateIndex(cn : TSqlConnection; const tablename, column : string);
var indexname : string;
begin
  indexname := format('idx_%s_%s', [tablename, column]);
  RecreateIndex(cn, tablename, indexname, column);
end;


function ColumnTypeAsString(columnType : TColumnType): string;
begin
  result := '';
  case columnType of
    ColumnInteger : result := 'INTEGER';
    ColumnString : result := 'TEXT';
    ColumnFloat  : result := 'REAL';
    ColumnBlob : result := 'BLOB';
  end;
end;

function TableField(const columnName : string; columnType : TColumnType): string;
begin
  result := format('%s %s', [columnName, ColumnTypeAsString(columnType)]);
end;

end.

