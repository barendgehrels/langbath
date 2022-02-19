unit lb_lib_storage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStorage }

  TStorage = class
    public
      function AddPicture(const topic, filename : string) : integer;
  end;

implementation

uses SqlDb, lb_lib, lb_sql_ddl, lb_db_manager, lb_config;

{ TStorage }

procedure CreateDatamodel(cn : TSqlConnection);
begin
  RecreateTable(cn, 'images', 'image_id',
    [TableField('topic', ColumnString),
    TableField('filename', ColumnString)]);
end;

function TStorage.AddPicture(const topic, filename: string): integer;
var db : TDbManager;
  cn : TPragmaticSqliteConnection;
  sql : string;
begin
  db := TDbManager.Create(ConfigDir + 'describe_picture.db');
  if db.IsNew then
  begin
    CreateDatamodel(db.Connection);
    db.Transaction.commit;
  end;

  sql := format('insert into images(topic, filename) values(''%s'', ''%s'')', [topic, filename]);

  cn := db.Connection;
  cn.ExecuteDirect(sql);
  result := cn.GetInsertID;
  db.Transaction.commit;
end;

end.

