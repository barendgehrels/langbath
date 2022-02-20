unit lb_frame_search_words;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DBCtrls, ExtCtrls, Buttons,
  SqlDb, lb_word_form_properties, lb_frame_db_image;

type

  { TFrameSearchWords }

  TWordIdEvent = procedure(wordId : integer) of object;

  TFrameSearchWords = class(TFrame)
    ButtonSearch: TSpeedButton;
    Edit1: TEdit;
    EditPermanentId: TEdit;
    EditSourceId: TEdit;
    EditRank: TEdit;
    EditWordId: TEdit;
    EditWordType: TEdit;
    EditGender: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelWordType: TLabel;
    LabelWordType1: TLabel;
    PaintBox: TPaintBox;
    LeftPanel: TPanel;
    ImagePanel: TPanel;
    SearchPanel: TPanel;
    Splitter1: TSplitter;
    TimerToRaiseEvent: TTimer;
    procedure Edit1Enter(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ImagePanelResize(Sender: TObject);
    procedure TimerToRaiseEventTimer(Sender: TObject);
  private
    iConnection : TSqlConnection;
    iForms : TArrayOfWordFormProperties;

    iFrameSingular, iFramePlural : TFrameDbImage;

    iWordId : integer;
    iOnSearch : TWordIdEvent;

    procedure Clear;
    procedure FillWordProperties(wordId : integer);

  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Connection : TSqlConnection write iConnection;
    property OnSearch : TWordIdEvent write iOnSearch;
  end;

implementation

{$R *.lfm}

uses Db, Graphics,
  lb_db_functions, lb_db_search, lb_draw_word_forms, lb_db_get_word_form_properties;

procedure BlobToImage(q : TSqlQuery; const columnName : string; db : TDbImage);
var stream : TStream;
begin
  stream := q.CreateBlobStream(q.FieldByName(columnName), bmRead);
  try
    Db.Picture.LoadFromStream(stream)
  finally
    stream.Free;
  end;
end;

procedure TFrameSearchWords.SpeedButtonClick(Sender: TObject);
var permanentId : integer;
begin
  Clear;

  iWordId := SearchWordId(iConnection, edit1.text);

  if iWordId > 0 then
  begin
    FillWordProperties(iWordId);

    permanentId := QueryAsInteger(iConnection, 'select permanent_id from words where word_id = %d',
      [iWordId], -1);
    iForms := GetWordFormProperties(iConnection, permanentId);
  end;

  PaintBox.Invalidate;
  TimerToRaiseEvent.Enabled := true;
end;

procedure TFrameSearchWords.Edit1Enter(Sender: TObject);
begin
  SpeedButtonClick(nil);
end;

procedure TFrameSearchWords.PaintBoxPaint(Sender: TObject);
const
  KLineHeight = 36;
var sel : TWordFormProperties;
  bitmap : TBitmap;
  maxW : integer;
begin
  if length(iForms) = 0 then exit;

  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(PaintBox.Width, PaintBox.Height);
    bitmap.Canvas.Brush.Color := $00B3FFFF;
    bitmap.Canvas.FillRect(0, 0, bitmap.Width, bitmap.Height);

    bitmap.Canvas.Font.Name := 'Segoe UI';
    bitmap.Canvas.Font.Height := KLineHeight;

    maxW := GetMaxWidth(bitmap.Canvas, iForms);

    Initialize(sel);
    sel.TypeId := iForms[0].typeId;
    DrawWordForms(bitmap, iForms, sel, 10, 10, maxw, KLineHeight);

    PaintBox.Canvas.Draw(0, 0, bitmap);
  finally
    bitmap.Free;
  end;
end;

procedure TFrameSearchWords.ImagePanelResize(Sender: TObject);
begin
  iFrameSingular.Height := ImagePanel.Height div 2;
end;

procedure TFrameSearchWords.TimerToRaiseEventTimer(Sender: TObject);
begin
  TimerToRaiseEvent.Enabled := false;
  if Assigned(iOnSearch) and (iWordId > 0) then
  begin
    iOnSearch(iWordId);
  end;
end;

procedure TFrameSearchWords.Clear;
begin
  iForms := [];
  EditWordId.Text := '';
  EditPermanentId.Text := '';
  EditSourceId.Text := '';
  EditRank.Text := '';
  EditWordType.Text := '';
  EditGender.Text := '';

  iFrameSingular.clear;
  iFramePlural.clear;
end;

procedure TFrameSearchWords.FillWordProperties(wordId: integer);
var q : TSQLQuery;
  i : integer;
begin
  q := TSQLQuery.Create(nil);
  try
    q.Database := iConnection;
    q.SQL.Text := format('select w.*,v.visual_word_id,v.image'
      + ' , (select min(gender_id) from forms f where f.word_id=w.word_id) as gender_id'
      + ' from words w'
      + ' left join visual_words v on v.word_id = w.word_id'
      + ' where w.word_id = %d', [wordId]);
    q.Open;

    i := 0;
    while not q.eof do
    begin
      if i = 0 then
      begin
        EditWordId.Text := q.FieldByName('word_id').AsString;
        EditWordType.Text := q.FieldByName('type_id').AsString;
        EditGender.Text := q.FieldByName('gender_id').AsString;
        EditPermanentId.Text := q.FieldByName('permanent_id').AsString;
        EditSourceId.Text := q.FieldByName('source_id').AsString;
        EditRank.Text := q.FieldByName('rank').AsString;
      end;

      if q.FieldByName('visual_word_id').AsInteger > 0 then
      begin
        if i = 0 then BlobToImage(q, 'image', iFrameSingular.DBImage)
        else if i = 1 then BlobToImage(q, 'image', iFramePlural.DBImage)
      end;

      inc(i);
      q.next;

    end;
  finally
    q.free;
  end;
end;

constructor TFrameSearchWords.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  iFrameSingular := TFrameDbImage.Create(self);
  iFrameSingular.Parent := ImagePanel;
  iFrameSingular.Align := alTop;
  iFrameSingular.Name := 'frameSingular';

  iFramePlural := TFrameDbImage.Create(self);
  iFramePlural.Parent := ImagePanel;
  iFramePlural.Align := alClient;
  iFramePlural.Name := 'framePlural';

  Clear;
end;

end.

