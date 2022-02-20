program import_brown;

uses
  Classes, SysUtils, Variants, LazLogger,
  lb_const, lb_db_manager, lb_types, lb_argument_parser,
  lb_db_functions,
  lb_import_words_from_brown;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    filename : string;
    keep : boolean;
    maxCsvRows : integer;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'filename', true, 'Filename of Brown''s Learners Dictionary CSV file'),
     Option('d', 'database', true, 'Filename of the SQLite database'),
     Option('k', 'keep', false, 'Keep temporary tables'),
     Option('r', 'max_csv_rows', false, 'Maximum rows of CSV (default: all)')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  result.help := VariantAsBoolean(values[0]);
  result.filename := values[1];
  result.db := values[2];
  result.keep := VariantAsBoolean(values[3]);
  result.maxCsvRows := VariantAsInteger(values[4]);
end;

procedure Import(const opts : TProgramOptions);
var db : TDbManager;
  t0 : double;
begin
  DebugLnEnter(format('Import Brown: start %s, rows=%d, keep=%d',
     [opts.filename, opts.maxCsvRows, ord(opts.keep)]));
  t0 := now;
  db := TDbManager.Create(opts.db);
  try

    ImportWordsFromBrown(db.Connection, db.Transaction,
          opts.filename, opts.keep, opts.maxCsvRows);


  finally
    db.free;
  end;

  DebugLnExit(format('Import Brown: elapsed: %.3f s', [(now - t0) * 24 * 3600]));
end;

var args : array of TProgramArgument;
  values : array of variant;
  opts : TProgramOptions;
  error : boolean;
begin
  args := GetPossibleArguments;
  if not ParseArguments(args, values) then
  begin
    WriteLn('Wrong argument(s)');
    PrintValues(args, values);
    PrintHelp(args);
    exit;
  end;

  opts := ArgumentsToRecord(values);
  if opts.help then
  begin
    PrintHelp(args);
    exit;
  end;

  if HasAllRequiredOptions(args, values) then
  begin
    error := false;
    if not FileExists(opts.filename) then
    begin
      WriteLn('File does not exist: ', opts.filename);
      error := true;
    end;
    if not FileExists(opts.db) then
    begin
      WriteLn('Database does not exist: ', opts.db);
      error := true;
    end;

    if not error then
    begin
      Import(opts);
    end;
  end;
end.


