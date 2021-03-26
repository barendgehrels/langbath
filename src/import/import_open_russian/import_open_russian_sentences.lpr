program import_open_russian_sentences;

uses
  Classes, SysUtils, Variants,
  lb_db_manager, lb_types, lb_argument_parser,
  lb_import_or_sentences, lb_datamodel_sentences,
  lb_import_sentence;

type
  TProgramOptions = record
    help : boolean;
    db : string;
    folder : string;
    keep : boolean;
    maxCsvRows : integer;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('f', 'folder', true, 'Folder containing OpenRussian CSV files'),
     Option('d', 'database', true, 'Filename of the SQLite database'),
     Option('k', 'keep', false, 'Keep temporary tables'),
     Option('r', 'max_csv_rows', false, 'Maximum rows of each CSV (default: all)')];
end;

function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  result.help := VariantAsBoolean(values[0]);
  result.folder := values[1];
  result.db := values[2];
  result.keep := VariantAsBoolean(values[3]);
  result.maxCsvRows := VariantAsInteger(values[4]);
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

    ImportOpenRussianSentences(db.Connection, opts.folder, opts.keep, opts.maxCsvRows);
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
    if not DirectoryExists(opts.folder) then
    begin
      writeln('Folder does not exist: ', opts.folder);
      error := true;
    end;
    if not FileExists(opts.folder + '\sentences.csv') then
    begin
      writeln('File does not exist: ', opts.folder + '\sentences.csv');
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

