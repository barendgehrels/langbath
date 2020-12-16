// Language Bath - Wave Form Timing Editor
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This form samples a wave form, draws it, and lets the user define begin and end time.

unit lb_form_wave_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, lb_bass, lb_time_optimizer;

type

  { TFormWaveForm }

  TFormWaveForm = class(TForm)
    ButtonOptimize: TButton;
    ButtonHelp: TButton;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonPlayFirst: TSpeedButton;
    ButtonPlaySecond: TSpeedButton;
    CheckBoxRescale: TCheckBox;
    LabelRight: TLabel;
    LabelLeft: TLabel;
    LabelCount: TLabel;
    PaintBoxWaveForm: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelOkCancel: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    TimerPlayExample: TTimer;
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonOptimizeClick(Sender: TObject);
    procedure ButtonPlayFirstClick(Sender: TObject);
    procedure ButtonPlaySecondClick(Sender: TObject);
    procedure CheckBoxRescaleChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PaintBoxWaveFormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxWaveFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxWaveFormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxWaveFormPaint(Sender: TObject);
    procedure PaintBoxWaveFormResize(Sender: TObject);
    procedure TimerPlayExampleTimer(Sender: TObject);
  private

    iCurrentPosition1 : double;
    iCurrentPosition2 : double;

    iBass : TLbBass;
    iLevels : TArrayOfLevel;
    iPositionIndications : array of double;
    iDragging1 : boolean;
    iDragging2 : boolean;
    iPosScale : double;
    iLevelScale : double;
    iPosMin, iPosMax : double;
    iLevelMin, iLevelMax : double;
    iDetails : TDetailedLevelInfo;

    procedure Draw(bitmap : TBitmap);

    procedure GetExtremes;
    procedure CalculateScale;

    function GetX(pos : double) : integer;
    function GetY(level : double) : integer;
    function GetInverseX(x : integer) : double;
    function IsMouseAt(x : integer; pos : double) : boolean;
    function IsOneSplit : boolean;
  public

    property GetPosition1 : double read iCurrentPosition1;
    property GetPosition2 : double read iCurrentPosition2;
    procedure Initialize(bass : TLbBass; pos1, pos2 : double);
    procedure AddPositionIndication(pos : double);
  end;

var
  FormWaveForm: TFormWaveForm;

implementation

{$R *.lfm}

uses Math;

{ TFormWaveForm }

const KMargin : integer = 5;

function TFormWaveForm.GetX(pos : double) : integer;
begin
  result := KMargin + round((pos - iPosMin) * iPosScale);
end;

function TFormWaveForm.GetY(level : double) : integer;
begin
  result := PaintBoxWaveForm.height - (KMargin + round((level - iLevelMin) * iLevelScale));
end;

function TFormWaveForm.GetInverseX(x : integer) : double;
begin
  if iPosScale = 0 then result := 0
  else result := iPosMin + (x - KMargin) / iPosScale;
end;

function TFormWaveForm.IsMouseAt(x : integer; pos : double) : boolean;
begin
  result := abs(GetX(pos) - x) < 5;
end;

function TFormWaveForm.IsOneSplit : boolean;
begin
  result := abs(GetX(iCurrentPosition1) - GetX(iCurrentPosition2)) <= 1;
end;

procedure TFormWaveForm.Initialize(bass : TLbBass; pos1, pos2 : double);
const length : double = 1.5;
var posMin, posMax : double;
begin
  iPositionIndications := [];
  iDetails.baseLevels := [];
  iBass := bass;

  // Assign positions
  iCurrentPosition1 := pos1;
  iCurrentPosition2 := pos2;
  LabelLeft.Caption := format('%.3f', [iCurrentPosition1]);
  LabelRight.Caption := format('%.3f', [iCurrentPosition2]);

  // Note that pos1 might be right of pos2
  posMin := min(pos1, pos2);
  posMax := max(pos1, pos2);

  iPosMin := max(posMin - length, 0);
  iPosMax := posMax + length;

  iLevelMin := 0;
  iLevelMax := 1;

  iLevels := iBass.GetSamples(iPosMin, iPosMax);
  GetExtremes;
  CalculateScale;
  PaintBoxWaveFormPaint(nil);
end;

procedure TFormWaveForm.AddPositionIndication(pos: double);
var n : integer;
begin
  n := length(iPositionIndications);
  SetLength(iPositionIndications, n + 1);
  iPositionIndications[n] := pos;
end;

procedure TFormWaveForm.GetExtremes;
var i : integer;
begin
  if CheckBoxRescale.Checked and (length(iLevels) > 0) then
  begin
    iPosScale := 0;
    iLevelScale := 0;
    iPosMin := 0;
    iPosMax := 0;
    iLevelMin := 0;

    i := low(iLevels);
    iPosMin := iLevels[i].positionSeconds;
    iPosMax := iLevels[i].positionSeconds;
    iLevelMin := iLevels[i].level;
    iLevelMax := iLevels[i].level;
    inc(i);

    while i <= high(iLevels) do
    begin
      if iLevels[i].positionSeconds < iPosMin then iPosMin := iLevels[i].positionSeconds;
      if iLevels[i].positionSeconds > iPosMax then iPosMax := iLevels[i].positionSeconds;
      if iLevels[i].level < iLevelMin then iLevelMin := iLevels[i].level;
      if iLevels[i].level > iLevelMax then iLevelMax := iLevels[i].level;
      inc(i);
    end;
  end
end;

procedure TFormWaveForm.CalculateScale;
var posRange, levelRange : double;
begin
  iPosScale := 0;
  iLevelScale := 0;

  posRange := iPosMax - iPosMin;
  levelRange := iLevelMax - iLevelMin;
  if (posRange > 0) and (levelRange > 0) then
  begin
    iPosScale := double(PaintBoxWaveForm.Width - 2 * KMargin) / posRange;
    iLevelScale := double(PaintBoxWaveForm.Height - 2 * KMargin) / levelRange;
  end;
end;

procedure TFormWaveForm.Draw(bitmap : TBitmap);

  procedure DrawVertical(pos : double; color : TColor);
  var x : integer;
  begin
    x := GetX(pos);
    if (x > 0) and (x < bitmap.Width) then
    begin
      bitmap.Canvas.Pen.Color := color;
      bitmap.Canvas.Pen.Width := 3;
      bitmap.Canvas.Line(x, KMargin div 2, x, bitmap.Height - KMargin div 2);
    end;
  end;
  procedure DrawHorizontal(level : double; color : TColor);
  var y : integer;
  begin
    y := GetY(level);
    if (y > 0) and (y < bitmap.Height) then
    begin
      bitmap.Canvas.Pen.Color := color;
      bitmap.Canvas.Pen.Width := 3;
      bitmap.Canvas.Line(KMargin div 2, y, bitmap.Width - KMargin div 2, y);
    end;
  end;

var i, x1, y1, x2, y2 : integer;
begin
  bitmap.Canvas.Brush.Color := clCream;
  bitmap.Canvas.FillRect(0, 0, bitmap.Width, bitmap.Height);

  bitmap.Canvas.Pen.Color := clGray;
  bitmap.Canvas.Pen.Width := 1;
  for i := low(iLevels) + 1 to high(iLevels) do
  begin
    x1 := GetX(iLevels[i - 1].positionSeconds);
    y1 := GetY(iLevels[i - 1].level);
    x2 := GetX(iLevels[i].positionSeconds);
    y2 := GetY(iLevels[i].level);
    bitmap.Canvas.Line(x1, y1, x2, y2);
  end;

  for i := low(iPositionIndications) to high(iPositionIndications) do
  begin
    DrawVertical(iPositionIndications[i], clSilver);
  end;

  if length(iDetails.baseLevels) > 0 then
  begin
    // This is basically for debugging only - though it might be useful for some end users.
    DrawVertical(iDetails.posZeroBegin, clYellow);
    DrawVertical(iDetails.posZeroEnd, TColor($0088FF));
    DrawVertical((iDetails.posZeroBegin + iDetails.posZeroEnd) / 2.0, clAqua);
    for i := low(iDetails.baseLevels) to high(iDetails.baseLevels) do
    begin
      DrawHorizontal(iDetails.baseLevels[i], clFuchsia);
    end;
  end;

  if IsOneSplit then
  begin
    DrawVertical(iCurrentPosition1, clTeal);
  end
  else
  begin
    DrawVertical(iCurrentPosition1, clGreen);
    DrawVertical(iCurrentPosition2, clBlue);
  end;

  if iBass.Active then
  begin
    DrawVertical(iBass.GetPositionSeconds, clRed);
  end;
end;

procedure TFormWaveForm.PaintBoxWaveFormPaint(Sender: TObject);
var bitmap : TBitmap;
begin
  // Draw indiretly, using a bitmap, to avoid flickering.
  // We might make it a class member and draw only when things change.
  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(PaintBoxWaveForm.Width, PaintBoxWaveForm.Height);
    Draw(bitmap);
    PaintBoxWaveForm.Canvas.Draw(0, 0, bitmap);
  finally
    bitmap.Free;
  end;
end;

procedure TFormWaveForm.PaintBoxWaveFormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  iDragging1 := IsMouseAt(x, iCurrentPosition1);
  iDragging2 := IsMouseAt(x, iCurrentPosition2);
  if iDragging1 and iDragging2 and not (ssCtrl in shift) then iDragging2 := false;

  if iDragging1 or iDragging2 then
  begin
    PaintBoxWaveForm.cursor := crHSplit;
  end;
end;

procedure TFormWaveForm.ButtonPlaySecondClick(Sender: TObject);
var length : double;
begin
  TimerPlayExample.Enabled := true;
  length := iPosMax - iCurrentPosition2;
  if length < 1.0 then length := 1;
  iBass.PlaySelection(iCurrentPosition2, iCurrentPosition2 + length);
end;

procedure TFormWaveForm.CheckBoxRescaleChange(Sender: TObject);
begin
  if CheckBoxRescale.Checked then
  begin
    GetExtremes;
  end
  else
  begin
    iLevelMin := 0;
    iLevelMax := 1;
  end;
  CalculateScale;
  PaintBoxWaveFormPaint(nil);
end;

procedure TFormWaveForm.FormHide(Sender: TObject);
begin
  iBass.Stop;
end;

procedure TFormWaveForm.ButtonPlayFirstClick(Sender: TObject);
var length : double;
begin
  TimerPlayExample.Enabled := true;
  length := iCurrentPosition1 - iPosMin;
  if length < 1.0 then length := 1;
  iBass.PlaySelection(max(iCurrentPosition1 - length, 0), iCurrentPosition1);
end;

procedure TFormWaveForm.ButtonHelpClick(Sender: TObject);
begin
  DefaultMessageBox('Drag the green line to define the end of the first sentence.' + #13
    + 'Drag the blue line to define the begin of the second sentence.' + #13 + #13
    + 'The first sentence might end either before or after the second sentence begins.' + #13 + #13
    + 'Hold CTRL and then drag to define both begin and end together.',
    'Help', 0);
end;

procedure TFormWaveForm.ButtonOptimizeClick(Sender: TObject);
begin
  GetOptimalTime(iDetails, iLevels, iCurrentPosition1);
  PaintBoxWaveFormPaint(nil);
end;

procedure TFormWaveForm.PaintBoxWaveFormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);

  function GetCurrentLevel : double;
  var i : integer;
  begin
    result := -1;
    for i := low(iLevels) to high(iLevels) do
    begin
      if iLevels[i].PositionSeconds > iCurrentPosition1 then // TODO
      begin
        result := iLevels[i].level;
        exit;
      end;
    end;
  end;

var isHovering : boolean;
begin
  if iDragging1 or iDragging2 then
  begin
    if iDragging1 then
    begin
      iCurrentPosition1 := GetInverseX(x);
      LabelLeft.Caption := format('%.3f', [iCurrentPosition1]);
    end;
    if iDragging2 then
    begin
      iCurrentPosition2 := GetInverseX(x);
      LabelRight.Caption := format('%.3f', [iCurrentPosition2]);
    end;
    PaintBoxWaveFormPaint(nil);
  end
  else
  begin
    isHovering := IsMouseAt(x, iCurrentPosition1) or IsMouseAt(x, iCurrentPosition2);
    PaintBoxWaveForm.cursor := specialize IfThen<TCursor>(isHovering, crHSplit, crDefault);
  end;
end;

procedure TFormWaveForm.PaintBoxWaveFormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  iDragging1 := false;
  iDragging2 := false;
  PaintBoxWaveForm.cursor := crDefault;
end;

procedure TFormWaveForm.PaintBoxWaveFormResize(Sender: TObject);
begin
  GetExtremes;
  CalculateScale;
  PaintBoxWaveFormPaint(nil);
end;

procedure TFormWaveForm.TimerPlayExampleTimer(Sender: TObject);
begin
  PaintBoxWaveFormPaint(nil);
  if not iBass.Active then
  begin
    TimerPlayExample.Enabled := false;
  end;
end;

end.

