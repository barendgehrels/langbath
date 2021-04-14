unit lb_update_permanent_ids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

procedure UpdatePermanentIds(cn : TSqlConnection;
  const permanentIdFilename, targetTable : string;
  lowId, highId : cardinal; keepTemps : boolean);

implementation

uses lb_lib, lb_db_import_csv, lb_db_functions;

procedure UpdateNewPermanentIds(cn : TSqlConnection;
  const targetTable, idsTable, newIdsTable : string; lowId, highId : cardinal);
const
  UpdateSql = 'update %s set permanent_id = %d + abs(random()) %% (%d - %d)';
  SubQuery1Sql = 'select permanent_id from %s';
  SubQuery2Sql = 'select permanent_id from (select permanent_id,count(1) as cnt from %s group by permanent_id having cnt>1)';
  Count1Sql = 'select count(1) from %s where permanent_id in (' + SubQuery1Sql + ')';
  Count2Sql = 'select count(1) from (select permanent_id from %s where permanent_id in (' + SubQuery2Sql + '))';
var cnt0, cnt, cnt1, cnt2, iteration : integer;
  column, sql : string;
begin
  cnt := QueryAsInteger(cn, 'select count(1) from %s', [targetTable]);
  cnt0 := QueryAsInteger(cn, 'select count(1) from %s where permanent_id is null', [targetTable]);

  if cnt0 = 0 then
  begin
    writeln('No update necessary: ', cnt0);
    exit;
  end;

  writeln('Table ', targetTable, ' : ', cnt, ' rows, without id : ', cnt0);

  if cnt0 > (highId - lowId) then
  begin
    log(format('Cannot generate new permanent ids in range [%d..%d] for %d rows',
      [lowId, highId, cnt0]));
    exit;
  end;

  // sentences -> "bare_sentence"
  column := 'bare_' + copy(targetTable, 1, length(targetTable) - 1);

  // Extract new rows
  log(format('Adding new permanent ids for %d rows', [cnt0]));
  cn.ExecuteDirect(format('drop table if exists %s', [newIdsTable]));
  cn.ExecuteDirect(format('create table %s as'
    + ' select %s,source_type_id,source_id,permanent_id from %s'
    + ' where permanent_id is null', [newIdsTable, column, targetTable]));

  cn.ExecuteDirect(format('create index idx_%s_%s on %s (%s)',
    [newIdsTable, 'source_id', newIdsTable, 'source_id']));
  cn.ExecuteDirect(format('create index idx_%s_%s on %s (%s)',
    [newIdsTable, 'permanent_id', newIdsTable, 'permanent_id']));

  // Generate new IDS in this temporary table
  cnt := ExecuteSql(cn, UpdateSql, [newIdsTable, LowId, HighId, LowId]);

  // Counts should be the same
  log(format('Updated %d new permanent ids for %d rows', [cnt, cnt0]));
  writeln(format('Updated %d new permanent ids for %d rows', [cnt, cnt0]));

  iteration := 0;
  cnt1 := QueryAsInteger(cn, Count1Sql, [newIdsTable, idsTable]);
  cnt2 := QueryAsInteger(cn, Count2Sql, [newIdsTable, newIdsTable]);
  while cnt1 + cnt2 > 0 do
  begin
    // Keep generating new IDS for the rows which are not yet unique.
    // The counts should decrease, also depending on the space (for "small" it doesn't decrease quickly)
    // TODO: it should be done more efficiently by changing only the duplicate ids
    sql := format(UpdateSql
      + ' where permanent_id in (%s) or permanent_id in (%s)',
      [newIdsTable, LowId, HighId, LowId,
        format(SubQuery1Sql, [idsTable]),
        format(SubQuery2Sql, [newIdsTable])]);
    cnt := ExecuteSql(cn, sql);

    writeln(format('Updated %d new permanent ids to avoid duplicate ids in [%d / %d]',
       [cnt, cnt1, cnt2]));
    log(format('Updated %d new permanent ids to avoid duplicate ids in [%d / %d]',
       [cnt, cnt1, cnt2]));

    cnt1 := QueryAsInteger(cn, Count1Sql, [newIdsTable, idsTable]);
    cnt2 := QueryAsInteger(cn, Count2Sql, [newIdsTable, newIdsTable]);

    inc(iteration);
    if iteration > 100 * (highId - lowid) then
    begin
      writeln('Impossible to assign new permanent ids in this range');
      exit;
    end;
  end;

  // on FAKE database it goes like:
  // Adding new permanent ids for 14130 rows
  // Updated 14130 new permanent ids for 14130 rows
  // Updated 2555 new permanent ids to avoid duplicate ids in [805 / 1871]
  // Updated 803 new permanent ids to avoid duplicate ids in [152 / 658]
  // Updated 290 new permanent ids to avoid duplicate ids in [52 / 238]
  // Updated 92 new permanent ids to avoid duplicate ids in [22 / 70]
  // Updated 22 new permanent ids to avoid duplicate ids in [4 / 18]
  // Updated 9 new permanent ids to avoid duplicate ids in [1 / 8]
  // Updated 7 new permanent ids to avoid duplicate ids in [1 / 6]
  // Updated 2 new permanent ids to avoid duplicate ids in [0 / 2]
  // Updated 2 new permanent ids to avoid duplicate ids in [0 / 2]
  // On REAL the range is higher, the missing IDS are lower, there should be only a few steps, if any

  // Update the newIdsTable with the new permanent ids
  cn.ExecuteDirect(format('update %s set permanent_id='
    + ' (select permanent_id from %s t where t.source_id=%s.source_id)'
    + ' where permanent_id is null', [targetTable, newIdsTable, targetTable]));
end;


procedure UpdatePermanentIds(cn : TSqlConnection;
  const permanentIdFilename, targetTable : string; lowId, highId : cardinal; keepTemps : boolean);
var newIdsTable, idsTable : string;
begin
  idsTable := format('existing_%s_permanent_ids', [targetTable]);
  newIdsTable := format('new_%s_permanent_ids', [targetTable]);

  cn.ExecuteDirect(format('drop table if exists %s', [idsTable]));

  if not ImportCsvFile(cn, permanentIdFilename, idsTable, #9, 'id') then
  begin
    exit;
  end;

  // Set the ids (will be most of them) using a join on source-type and the id of the source
//TODO: this MUST join on the bare sentence/word too because ID's (in Books) change!
  cn.ExecuteDirect(format('update %s set permanent_id = '
    + ' (select permanent_id from %s p where p.source_id=%s.source_id'
    + ' and p.source_type_id=%s.source_type_id)',
    [targetTable, idsTable, targetTable, targetTable]));
  cn.Transaction.Commit;

  UpdateNewPermanentIds(cn, targetTable, idsTable, newIdsTable, lowId, highId);
  if not KeepTemps then
  begin
    cn.executedirect(format('drop table if exists %s', [newIdsTable]));
    cn.executedirect(format('drop table if exists %s', [idsTable]));
  end;
end;

end.

// The first time, they can be updated in SQLiteStudio by calling this repetitively.
// update X set permanent_id = 1000000 + abs(random()) % (9000000 - 1000000)
// where permanent_id is null
// or permanent_id in (select permanent_id from sentences group by permanent_id having count(1) > 1)

