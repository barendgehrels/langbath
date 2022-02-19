// Language Bath - Common
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// This unit provides a small class to connect to a SQLite3 database

unit lb_db_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb, SQLite3Conn;

type

  // Adapted connection to set some pragmas,
  // for example to enhance performance for inserting bulk data
  TPragmaticSqliteConnection = class(TSQLite3Connection)
  protected
    procedure DoInternalConnect; override;
  end;

  // Lightweight class containing the two necessary components
  // of a connection to SQLite3: the connection and the transation
  TDbManager = class
    private
      fIsNew : boolean;
      fConnection : TPragmaticSqliteConnection;
      fTransaction : TSQLTransaction;

    public
      constructor Create(const filename : string);
      destructor Destroy; override;

      // Returns true if a tablename (case-insensitive) is in the database
      // Note: it's not cached or fast, don't use it often
      function HasTable(const tablename : string) : boolean;

    published
      // Returns true if the database is brand new
      property IsNew : boolean read fIsNew;

      // Returns the connection
      property Connection : TPragmaticSqliteConnection read fConnection;

      // Returns the transaction
      property Transaction : TSqlTransaction read fTransaction;
  end;


implementation

procedure TPragmaticSqliteConnection.DoInternalConnect;
begin
  inherited DoInternalConnect;
  // The super-duper performance mode!
  // The settings below result in the fastest inserts for our application.
  // It is now 75 seconds (for linking words to sentences in OR)
  // instead of the 286 seconds before, so ~4 times faster.
  // Removing the indexes (temporarily) was another big step -> 34 seconds

  //execsql('PRAGMA journal_mode = WAL'); // not much better, though some sites say so
  //execsql('PRAGMA locking_mode = EXCLUSIVE'); // the same

  execsql('PRAGMA synchronous = OFF'); // much much better
  execsql('PRAGMA cache_size = -100000'); // the higher the better
end;

constructor TDbManager.Create(const filename: string);
begin
  fTransaction := TSQLTransaction.Create(nil);
  fConnection := TPragmaticSqliteConnection.Create(nil);

  // Link them
  fConnection.Transaction := fTransaction;
  fTransaction.Database := fConnection;

  fConnection.Options:= [];

  fIsNew := not FileExists(filename);
  fConnection.databasename := filename;

  // Open it or create it
  fConnection.Open;
end;

destructor TDbManager.Destroy;
begin
  fTransaction.free;
  fConnection.free;
  inherited Destroy;
end;

function TDbManager.HasTable(const tablename: string): boolean;
var list : TStringList;
  k : integer;
begin
  result := false;
  list := TStringList.Create;
  try
    fConnection.GetTableNames(list, false);
    for k := 0 to list.count - 1 do
    begin
      if lowercase(list[k]) = tablename then
      begin
        result := true;
        exit;
      end;
    end;
  finally
    list.free;
  end;
end;

end.

