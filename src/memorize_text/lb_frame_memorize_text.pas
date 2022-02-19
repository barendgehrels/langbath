// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Main frame

unit lb_frame_memorize_text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics;

type

  { TFrameMemorizeText }

  TFrameMemorizeText = class(TFrame)
    Button1: TButton;
    ButtonCompare: TButton;
    ComboBoxTexts: TComboBox;
    Edit1: TEdit;
    GroupBoxImage: TGroupBox;
    GroupBoxEnter: TGroupBox;
    ImageUnsplash: TImage;
    Memo1: TMemo;
    MemoInput: TMemo;
    PaintBox: TPaintBox;
    Panel2: TPanel;
    PanelCenter: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCompareClick(Sender: TObject);
    procedure ComboBoxTextsChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MemoInputChange(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);

    function GetFilename : string;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddEntry(const s : string);
  private
    alignment1, alignment2 : string;
  end;

implementation

{$R *.lfm}

uses LazUTF8, LCLIntf, Dialogs,
  lb_needleman_wunsch, lb_align_lengths, lb_draw_text,
  lb_lib, lb_lib_string, lb_config;

function ReadStringsFromFile(const f : string; hint : boolean) : TStringList;
var list : TStringList;
  i : integer;
  s : string;
  isHint : boolean;
begin
  result := TStringList.Create;

  list := TStringList.Create;
  try
    list.LoadFromFile(f);
    for i := 0 to list.count - 1 do
    begin
      s := list[i];
      isHint := s.StartsWith('#');
      if (hint and ishint) or (not hint and not isHint) then
      begin
        result.add(list[i]);
      end;
    end;
  finally
    list.free;
  end;

end;

constructor TFrameMemorizeText.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  PanelCenter.Caption := '';
end;

destructor TFrameMemorizeText.Destroy;
begin
  inherited Destroy;
end;

procedure TFrameMemorizeText.AddEntry(const s: string);
begin
  ComboboxTexts.Items.Add(s);
end;

procedure TFrameMemorizeText.FrameResize(Sender: TObject);
var w : integer;
begin
  w := self.width - PanelCenter.width;
  GroupBoxImage.width := w div 2;
end;

procedure TFrameMemorizeText.MemoInputChange(Sender: TObject);
begin
  alignment1 := '';
  alignment2 := '';
  PaintBox.Invalidate;
end;

procedure TFrameMemorizeText.ButtonCompareClick(Sender: TObject);
var list : TStringList;
  filename, correction, entered : string;
  d : integer;
begin
  filename := GetFilename;

  if filename = '' then
  begin
    exit;
  end;

  alignment1 := '';
  alignment2 := '';

  list := ReadStringsFromFile(filename, false);

  try

    // Use a clean string
    entered := ReplaceAccents(memoinput.text);
    correction := AlignLengths(ReplaceAccents(list.text), entered);

    if correction <> '' then
    begin
      AlignWithNeedlemanWunsch(correction, entered, '*', -5, alignment1, alignment2);

      d := LevenshteinDistance(CleanString(correction), CleanString(entered));
      edit1.text := Inttostr(d);
    end;


  finally
    list.free;
  end;
  PaintBox.Invalidate;
end;

procedure TFrameMemorizeText.Button1Click(Sender: TObject);
var list : TStringList;
  filename : string;
begin
  filename := GetFilename;
  if filename <> '' then
  begin
    list := ReadStringsFromFile(filename, true);
    memo1.Text:= list.text;
    list.free;
  end;
end;

procedure TFrameMemorizeText.ComboBoxTextsChange(Sender: TObject);
begin

end;

procedure TFrameMemorizeText.PaintBoxPaint(Sender: TObject);

  function FontSize : integer;
  const factor = 0.975;
  var pixelsPerLine : double;
  begin
    result := PaintBox.Font.Height;
    pixelsPerLine := factor * PaintBox.Height / (MemoInput.Lines.count + 1);
    if pixelsPerLine < result then
    begin
      result := round(pixelsPerLine);
    end;
  end;

var
  bitmap : TBitmap;
begin

  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(PaintBox.Width, PaintBox.Height);

    // Fill with a light yellow background
    bitmap.Canvas.Brush.Color := $00B3FFFF;
    bitmap.Canvas.FillRect(0, 0, bitmap.Width, bitmap.Height);

    if alignment1 <> '' then
    begin
      // For Russian:
      // Take "Segoe" which displays L (л) more distinctively from P (п)
      // then other fonts do
      bitmap.Canvas.Font.Name := 'Segoe UI';
      bitmap.Canvas.Font.Height := Fontsize;

      bitmap.Canvas.MoveTo(3, 3);

      DrawTextConsideringAlignment(bitmap.Canvas, alignment1, alignment2);
    end;

    PaintBox.Canvas.Draw(0, 0, bitmap);
  finally
    bitmap.Free;
  end;
end;

function TFrameMemorizeText.GetFilename: string;
begin
  result := '';
  if ComboBoxTexts.ItemIndex < 0 then
  begin
    exit;
  end;

  with ComboBoxTexts do
  begin
    result := ConfigDir + Items[ItemIndex] + '.txt'
  end;

  if not FileExists(result) then
  begin
    showmessage('File does not exist: ' + result);
    result := '';
  end;

end;

end.

