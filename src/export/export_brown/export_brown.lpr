program export_brown;

uses
  Classes, SysUtils, Variants, LazLogger,
  lb_const, lb_db_manager, lb_types, lb_argument_parser,
  lb_db_functions,
  lb_export_words_from_brown;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    filename : string;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'filename', true, 'Filename to write Brown list'),
     Option('d', 'database', true, 'Filename of the SQLite database')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  result.help := VariantAsBoolean(values[0]);
  result.filename := values[1];
  result.db := values[2];
end;

procedure ExportBrown(const opts : TProgramOptions);
var db : TDbManager;
  t0 : double;
begin
  DebugLnEnter(format('Exporting to %s', [opts.filename]));
  t0 := now;
  db := TDbManager.Create(opts.db);
  try
    ExportWordsFromBrown(db.Connection, opts.filename);
  finally
    db.free;
  end;

  DebugLnExit(format('Elapsed: %.3f s', [(now - t0) * 24 * 3600]));
end;

var args : array of TProgramArgument;
  values : array of variant;
  opts : TProgramOptions;
  error : boolean;
begin
  args := GetPossibleArguments;
  if not ParseArguments(args, values) then
  begin
    writeln('Wrong argument(s)');
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
    if  FileExists(opts.filename) then
    begin
      writeln('File already exists: ', opts.filename);
      error := true;
    end;
    if not FileExists(opts.db) then
    begin
      writeln('Database does not exist: ', opts.db);
      error := true;
    end;

    if not error then
    begin
      ExportBrown(opts);
    end;
  end;
end.


