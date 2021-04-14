program import_book;

uses
  Classes, SysUtils, LazUtf8, Variants,
  lb_argument_parser,
  lb_datamodel_sentences,
  lb_db_manager,
  lb_db_functions,
  lb_import_sentences_from_books;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    inifile : string;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('i', 'ini', true, 'Name of the INI file containing book settings'),
     Option('d', 'database', true, 'Filename of the SQLite database')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  if varType(values[0]) = varBoolean then result.help := values[0] else result.help := false;
  result.inifile := values[1];
  result.db := values[2];
end;

procedure Import(const opts : TProgramOptions);
var db : TDbManager;
  t0 : double;
begin
  t0 := now;
  db := TDbManager.Create(opts.db);
  try

    if db.IsNew or not db.HasTable('sentences') then
    begin
      RecreateSentenceTables(db.Connection);
      db.Transaction.commit;
    end;

    ImportSentencesFromIni(db.Connection, opts.inifile);
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
  path : string;
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
    if not FileExists(opts.inifile) then
    begin
      writeln('File does not exist: ', opts.inifile);
      error := true;
    end;
    path := ExtractFilePath(opts.db);
    if (path <> '') and not DirectoryExists(path) then
    begin
      writeln('Folder for database does not exist: ', ExtractFilePath(path));
      error := true;
    end;

    if not error then
    begin
      Import(opts);
    end;
  end;
end.

