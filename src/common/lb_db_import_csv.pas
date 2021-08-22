// Language Bath - Common
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// This unit provides functionality to import a CSV file into a database.
// The first row can contain the header, or column names can be specified.

unit lb_db_import_csv;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, SqlDb, lb_sql_ddl;

function ColumnList(const columnNames : array of string; const ColumnTypes : array of TColumnType) : string;

function ImportCsvFile(cn: TSQLConnection; const CsvFilename, tableName : string;
   separator : char = #9;
   const IdSuffix : string = '';
   maxLines : integer = -1) : boolean;

function ImportCsvFile(cn: TSQLConnection;
   const CsvFilename, tableName : string;
   const ColumnNames : array of string;
   const ColumnTypes : array of TColumnType;
   const ColumnIndices : array of boolean;
   separator : char = #9;
   maxLines : integer = -1;
   ignoreRest : boolean = false) : boolean;


implementation

uses LazUtf8, lb_lib, lb_sql_dml_insert;

function ColumnList(const columnNames : array of string; const ColumnTypes : array of TColumnType) : string;
var i : integer;
begin
  result := '';
  for i := low(columnNames) to high(columnNames) do
  begin
    if i > low(columnNames) then result := result + ',';
    result := result + TableField(columnNames[i], ColumnTypes[i]);
  end;
end;

function ColumnList(const columnNames : array of string; const IdSuffix: string) : string;
var i : integer;
  columnTypes : array of TColumnType;
begin
  columnTypes := [];
  SetLength(columnTypes, length(columnNames));
  for i := low(columnNames) to high(columnNames) do
  begin
    if (IdSuffix <> '') and columnNames[i].EndsWith(IdSuffix, true)
    then columnTypes[i] := ColumnInteger else columnTypes[i] := ColumnString;
  end;
  result := ColumnList(columnNames, columnTypes);
end;

function ImportCsvFile(cn: TSQLConnection; const CsvFilename,
  tableName: string; separator: char; const IdSuffix: string;
  maxLines : integer) : boolean;
var txt : TextFile;
  s, sql, indexname : string;
  ar : array of string;
  columnNames : array of string;
  i, n : integer;
  inserter : TSqlInserter;
begin
  result := false;
  assign(txt, CsvFilename);
  {$I-}
  reset(txt);
  {$I+}
  if IOResult <> 0 then exit;

  inserter := TSqlInserter.Create(cn);

  try
    columnNames := [];
    n := 0;
    while not eof(txt) and ((maxLines < 0) or (n < maxLines)) do
    begin
      inc(n);
      Readln(txt, s);
      ar := SplitString(s, separator);

      if n = 1 then
      begin
        sql := format('create table %s(%s)', [tablename, ColumnList(ar, IdSuffix)]);
        cn.ExecuteDirect(sql);
        SetLength(columnNames, length(ar));
        for i := low(ar) to high(ar) do columnNames[i] := ar[i];
        inserter.Prepare(tablename, columnNames);
      end
      else if length(ar) <> length(columnNames) then
      begin
        log(format('%s: inconsistent line: %d', [CsvFileName, n]));
      end
      else
      begin
        //SqlInsert(cn, tablename, CreateColumns(columnNames, ar));
        inserter.Bind(ar);
        inserter.Execute;
      end;
    end;

  finally
    CloseFile(txt);
    inserter.free;
  end;

  // Create indices for all numerical columnNames
  if IdSuffix <> '' then
  begin
    for i := low(columnNames) to high(columnNames) do
    begin
      if columnNames[i].endswith(IdSuffix) then
      begin
        indexname := format('idx_%s_%s', [tablename, columnNames[i]]);
        cn.ExecuteDirect(format('create index %s on %s (%s)',
          [indexname, tablename, columnNames[i]]));
      end;
    end;
  end;

  result := true;
end;

function ImportCsvFile(cn: TSQLConnection;
   const CsvFilename, tableName : string;
   const ColumnNames : array of string;
   const ColumnTypes : array of TColumnType;
   const ColumnIndices : array of boolean;
   separator : char = #9;
   maxLines : integer = -1;
   ignoreRest : boolean = false) : boolean;
var txt : TextFile;
  s, sql, indexname : string;
  ar : array of string;
  i, n : integer;
  inserter : TSqlInserter;
begin
  sql := format('create table %s(%s)', [tablename, ColumnList(ColumnNames, ColumnTypes)]);
  cn.ExecuteDirect(sql);

  result := false;
  assign(txt, CsvFilename);
  {$I-}
  reset(txt);
  {$I+}
  if IOResult <> 0 then exit;

  // Create and prepare the query only once, to achieve blazing inserts
  inserter := TSqlInserter.Create(cn);
  inserter.Prepare(tablename, columnNames);

  try
    n := 0;
    while not eof(txt) and ((maxLines < 0) or (n < maxLines)) do
    begin
      inc(n);
      Readln(txt, s);
      ar := SplitString(s, separator);

      if (length(ar) > length(columnNames)) and ignoreRest then
      begin
        SetLength(ar, length(ColumnNames));
      end;

      if length(ar) <> length(columnNames) then
      begin
        log(format('%s: inconsistent line: %d', [CsvFileName, n]));
      end
      else
      begin
        inserter.Bind(ar);
        inserter.Execute;
      end;
    end;

    if n mod 10000 = 0 then cn.transaction.commit;

  finally
    CloseFile(txt);
    inserter.free;
  end;

  // Create indices
  for i := low(columnNames) to high(columnNames) do
  begin
    if columnIndices[i] then
    begin
      indexname := format('idx_%s_%s', [tablename, columnNames[i]]);
      cn.ExecuteDirect(format('create index %s on %s (%s)',
        [indexname, tablename, columnNames[i]]));
    end;
  end;

  result := true;
end;

end.

