unit lb_ui_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

procedure AddMinimal(item : TListItem; count : integer);


implementation

procedure AddMinimal(item : TListItem; count : integer);
var i : integer;
begin
  for i := item.SubItems.Count to count do
  begin
    item.SubItems.Add('');
  end;
end;


end.

