unit lb_db_train;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

procedure CreateTrainTables(cn : TSqlConnection);

implementation

uses lb_sql_ddl;

procedure CreateTrainTables(cn: TSqlConnection);
begin
  RecreateTable(cn, 'train', 'train_id',
    [TableField('permanent_word_id', ColumnInteger),
     TableField('permanent_sentence_id', ColumnInteger),
     TableField('language_id', ColumnInteger),
     TableField('timestamp', ColumnFloat),
     TableField('interval_age', ColumnFloat),
     TableField('interval_update', ColumnFloat)]);

  RecreateTable(cn, 'scores', 'score_id',
    [TableField('train_id', ColumnInteger),
     TableField('permanent_sentence_id', ColumnInteger),
     TableField('timestamp', ColumnFloat),
     TableField('entered_text', ColumnString),
     TableField('interval_at_enter', ColumnFloat)]);
end;

end.

