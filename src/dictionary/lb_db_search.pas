unit lb_db_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SqlDb;

function SearchWordId(cn : TSqlConnection; const term : string) : integer;

implementation

uses lb_db_functions;

function SearchWordId(cn: TSqlConnection; const term: string): integer;
var rank : integer;
begin
  if (copy(term, 1, 1) = '#')
  and TryStrToInt(copy(term, 2, length(term) - 1), rank) then
  begin
    result := QueryAsInteger(cn, 'select word_id from words where rank = %d', [rank], -1);
    if result > 0 then exit;
  end;

  // Look if it is a word
  result := QueryAsInteger(cn,
    'select word_id from words where bare_word=''%s''',
    [term], -1);

  if result = -1 then
  begin
    // Look if it is a form
    result := QueryAsInteger(cn,
      'select word_id from forms where bare_word=''%s''',
      [term], -1);
  end;

  if result = -1 then
  begin
    // Fuzzy search: query with first/last letter, length around the entered length,
    // and smallest edit distance
  end;
end;

end.

