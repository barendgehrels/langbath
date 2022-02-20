unit lb_frame_db_image;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, DBCtrls, ExtCtrls;

type

  { TFrameDbImage }

  TFrameDbImage = class(TFrame)
    DBImage: TDBImage;
    BottomPanel: TPanel;
  private

  public

    procedure Clear;

  end;

implementation

{$R *.lfm}

{ TFrameDbImage }

procedure TFrameDbImage.Clear;
begin
  DbImage.Picture.Clear;
end;

end.

