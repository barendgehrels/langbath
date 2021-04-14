program link_words_and_sentences;
uses
  Classes, SysUtils, Variants,
  lb_const, lb_db_manager, lb_types, lb_argument_parser,
  lb_datamodel_sentences,
  lb_import_sentence,
  lb_db_functions, lb_link_words_and_sentences;

type
  TProgramOptions = record
    help : boolean;
    sourceTypeId : integer;
    db : string;
    keep : boolean;
  end;

function GetPossibleArguments : TArrayOfProgramArgument;
begin
  result :=
    [Option('h', 'help'),
     Option('s', 'source_type_id', false, 'Source type id'),
     Option('d', 'database', true, 'Filename of the SQLite database'),
     Option('k', 'keep', false, 'Keep temporary tables')];
end;

// Changes the values of the possible arguments as set above,
// into an easier to read structure
function ArgumentsToRecord(const values : array of variant) : TProgramOptions;
begin
  if varType(values[0]) = varBoolean then result.help := values[0] else result.help := false;
  if not TryStrToInt(values[1], result.sourceTypeId) then
  begin
    result.sourceTypeId := -1;
  end;
  result.db := values[2];
  if varType(values[3]) = varBoolean then result.keep := values[6] else result.keep := false;
end;

procedure Import(const opts : TProgramOptions);
var db : TDbManager;
  t0 : double;
begin
  t0 := now;
  db := TDbManager.Create(opts.db);
  try

    if db.IsNew
    or not db.HasTable('sentences')
    or not db.HasTable('words')
    then
    begin
      writeln('Words and sentences should first be imported into the database');
      exit;
    end;

    UpdateLinkSentencesWords(db.Connection, opts.sourceTypeId, opts.keep);
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

