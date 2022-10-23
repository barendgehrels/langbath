program import_tatoeba;

uses
  Classes, SysUtils, Variants,
  lb_const, lb_db_manager, lb_types, lb_argument_parser,
  lb_datamodel_sentences,
  lb_import_sentence,
  lb_import_tatoeba_sentences, lb_db_functions;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    folder : string;
    keep : boolean;
    maxCsvRows : integer;
    maxLevel : integer;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'folder', true, 'Folder containing Tatoeba CSV files'),
     Option('d', 'database', true, 'Filename of the SQLite database'),
     Option('k', 'keep', false, 'Keep temporary tables'),
     Option('l', 'maxlevel', false, 'Maximum levels of (in)direct translations (default: all)'),
     Option('r', 'max_csv_rows', false, 'Maximum rows of each CSV (default: all)')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  result.help := VariantAsBoolean(values[0]);
  result.folder := values[1];
  result.db := values[2];
  result.keep := VariantAsBoolean(values[3]);
  result.maxLevel := VariantAsInteger(values[4]);
  result.maxCsvRows := VariantAsInteger(values[5]);
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

    ImportTatoebaSentences(db.Connection, db.Transaction,
      opts.folder, db.IsNew, opts.keep, opts.maxCsvRows, opts.maxLevel);

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
    if not DirectoryExists(opts.folder) then
    begin
      writeln('Folder does not exist: ', opts.folder);
      error := true;
    end;
    if not FileExists(opts.folder + DirectorySeparator + 'sentences.csv') then
    begin
      writeln('File does not exist: ', opts.folder + DirectorySeparator + 'sentences.csv');
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


