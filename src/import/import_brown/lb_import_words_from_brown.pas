// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Imports words from Nicholas Brown:
//   "Russian Learners' Dictionary: 10,000 Russian Words in Frequency Order"
//   https://www.amazon.com/Russian-Learners-Dictionary-Words-Frequency/dp/0415137926
// It is available as TSV from here:
//   https://www.reddit.com/r/russian/comments/289wba/10000_most_common_russian_words_in_spreadsheet

unit lb_import_words_from_brown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function ImportWordsFromBrown(cn : TSqlConnection; ta : TSqlTransaction;
    const filename : string; keepTemps : boolean;
    maxCsvRows : integer) : boolean;

implementation

uses LazUtf8, LazLoggerBase,
  lb_lib, lb_lib_string, lb_types, lb_db_import_csv, lb_sql_ddl, lb_sql_dml_insert;

const

  brownTables : array of string = ('brown_words_by_frequency', 'brown_words_cleaned');

  // Specify definitions
  brownColumns : array of string = ('brown_word', 'translation','explanation');
  brownTypes : array of TColumnType = (ColumnString, ColumnString, ColumnString);
  brownIndices : array of boolean = (true, false, false);


procedure CleanBrownWords(cn: TSqlConnection);

  procedure ExtractAnnotation(const word : string; out processed, annotations : string);
  var p1, p2 : integer;
  begin
    annotations := '';
    // Some words have, somehow, double quotes. Replace them, this algorithm doesn't work
    // correctly for them, nested brackets are not supported.
    processed := UTF8LowerCase(StringReplace(StringReplace(word, '))', ')', [rfReplaceAll]), '((', '(', [rfReplaceAll]));

    // Fix some erroneous lines
    processed := StringReplace(processed, ' adj', ' (adj)', [rfReplaceAll]);
    processed := StringReplace(processed, ' or ', '/', [rfReplaceAll]);
    p1 := pos('(', processed);
    while p1 > 0 do
    begin
      p2 := pos(')', processed, p1);
      if p2 = 0 then exit;

      annotations := annotations + ' ' + copy(processed, p1, p2 - p1 + 1);
      delete(processed, p1, p2 - p1 + 1);
      processed := trim(processed);

      // For next round
      p1 := pos('(', processed);
    end;
  end;

  // Splits
  // 1: "сшибать/сшибить" into "сшибать" and "сшибить"
  // 2: "щупать/по-" into "щупать" and "пощупать"
  // 3: "мучить/за-/из-" -> "мучить" and "замучить"
  function SplitVariants(const word : string) : TArrayOfString;
  var i, p : integer;
    parts : TArrayOfString;
  begin
    parts := SplitString(word, '/');
    if length(parts) <= 1 then
    begin
      result := [word];
      exit;
    end;

    SetLength(result, length(parts));
    for i := low(parts) to high(parts) do
    begin
      p := pos('-', parts[i]);
      if p > 0 then result[i] := copy(parts[i], 1, p - 1) + parts[0] else result[i] := parts[i]
    end;
  end;

var q : TSqlQuery;
  word, processed, annotations : string;
  values, variants : array of string;
  inserter : TSqlInserter;

  i : integer;
begin
  RecreateTable(cn, brownTables[1], 'brown_id',
    [TableField('brown_word', ColumnString),
    TableField('bare_word', ColumnString),
    TableField('annotation', ColumnString),
    TableField('translation', ColumnString),
    TableField('explanation', ColumnString)]);

  inserter := TSqlInserter.Create(cn);
  inserter.Prepare(brownTables[1], ['brown_word', 'bare_word', 'annotation', 'translation', 'explanation']);

  q := TSQLQuery.Create(nil);
  try
    q.Database := cn;
    q.SQL.Text := format('select * from %s', [brownTables[0]]);
    q.Open;

    while not q.eof do
    begin
      word := trim(q.FieldByName('brown_word').AsString);
      ExtractAnnotation(word, processed, annotations);
      variants := SplitVariants(processed);

      for i := low(variants) to high(variants) do
      begin
        values := [variants[i], DisplayString(variants[i]), annotations,
          q.FieldByName('translation').AsString,
          q.FieldByName('explanation').AsString];
        inserter.Bind(values);
        inserter.Execute;
      end;

      q.next;
    end;
  finally
    q.free;
    inserter.free;
  end;

  RecreateIndex(cn, brownTables[1], 'bare_word');
  cn.transaction.Commit;
end;


procedure ImportCsvTable(cn : TSqlConnection; const filename : string; maxCsvRows : integer);
begin
  cn.transaction.StartTransaction;
  ImportCsvFile(cn, filename, brownTables[0],
    brownColumns, brownTypes, brownIndices, #9, maxCsvRows, true);
  cn.transaction.Commit;
end;

procedure DeleteTemporaryBrownTable(cn : TSqlConnection);
begin
  // Delete only the uncleaned table,
  // because the cleaned table is necessary for export.
  cn.executedirect(format('drop table if exists %s', [brownTables[0]]));
  cn.transaction.Commit;
end;

function ImportWordsFromBrown(cn : TSqlConnection; ta : TSqlTransaction;
    const filename : string; keepTemps : boolean;
    maxCsvRows : integer) : boolean;
begin
  result := false;
  if not fileexists(filename) then exit;

  DeleteTemporaryBrownTable(cn);

  DebugLn('Importing CSV...');
  ImportCsvTable(cn, filename, maxCsvRows);

  DebugLn('Cleaning words...');
  CleanBrownWords(cn);

  if not keepTemps then
  begin
    DebugLn('Deleting temporary table...');
    DeleteTemporaryBrownTable(cn);
  end;
  result := true;
end;

end.

