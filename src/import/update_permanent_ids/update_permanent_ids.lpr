program update_permanent_ids;

uses
  Classes, SysUtils, Variants,
  lb_const, lb_db_manager, lb_types, lb_argument_parser,
  lb_datamodel_sentences,
  lb_import_sentence,
  lb_datamodel,
  lb_db_functions, lb_update_permanent_ids;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    csvFilename : string;
    targetTable : string;
    keep : boolean;
    lower, upper : cardinal
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'filename', true, 'CSV filename containing permanent ids'),
     Option('d', 'database', true, 'Filename of the SQLite database'),
     Option('t', 'target', true, 'Target table ("words" or "sentences")'),
     Option('l', 'lower', true, 'Lower boundary'),
     Option('u', 'upper', true, 'Upper boundary'),
     Option('k', 'keep', false, 'Keep temporary tables')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  if varType(values[0]) = varBoolean then result.help := values[0] else result.help := false;
  result.csvFilename := values[1];
  result.db := values[2];
  result.targetTable := values[3];
  if not TryStrToDWord(values[4], result.lower) then result.lower := 0;
  if not TryStrToDWord(values[5], result.upper) then result.upper := 0;
  if varType(values[6]) = varBoolean then result.keep := values[6] else result.keep := false;
end;

procedure Import(const opts : TProgramOptions);
var db : TDbManager;
  t0 : double;
begin
  t0 := now;
  db := TDbManager.Create(opts.db);
  try
    if db.IsNew or not db.HasTable(opts.targetTable) then
    begin
      writeln('Table ', opts.targetTable, ' does not exist in the database');
      exit;
    end;

    UpdatePermanentIds(db.Connection, opts.csvFilename,
      opts.targetTable, opts.lower, opts.upper, opts.keep);
    db.Transaction.Commit;

  finally
    db.free;
  end;

  writeln(format('Elapsed: %.3f s', [(now - t0) * 24 * 3600]));
end;

var args : array of TProgramArgument;
  values : array of variant;
  opts : TProgramOptions;
  error : boolean;
begin
  args := GetPossibleArguments;
  if not ParseArguments(args, values) then
  begin
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
    if not FileExists(opts.csvFilename) then
    begin
      writeln('File does not exist: ', opts.csvFilename);
      error := true;
    end;
    if not FileExists(opts.db) then
    begin
      writeln('Database does not exist: ', opts.db);
      error := true;
    end;
    if opts.lower >= opts.upper then
    begin
      writeln('Boundaries invalid ', opts.lower, ' ', opts.upper);
      error := true;
    end;

    if not error then
    begin
      Import(opts);
    end;
  end;
end.

