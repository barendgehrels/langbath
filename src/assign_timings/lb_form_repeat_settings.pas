unit lb_form_repeat_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  TRepeatSettings = record
    showTarget : boolean;
    showTranslation : boolean;
    playAudio : boolean;
  end;

  TArrayOfRepeatSettings = array of TRepeatSettings;

  TControlPanel = record
    panel : tpanel;
    checkBoxShowTarget, checkBoxShowTranslation, checkBoxPlayAudio : TChecKBox;
  end;

  { TFormRepeatSettings }

  TFormRepeatSettings = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    PanelOkCancel: TPanel;
    sb: TScrollBox;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

    iControlPanels : array of TControlPanel;

    procedure FreeChildControls(myControl : TWinControl; doFree : boolean);
    procedure SetChecks(const ar : TArrayOfRepeatSettings);
    procedure CreatePanels(count : integer);

  public

    procedure Init(const ar : TArrayOfRepeatSettings; c : integer);
    procedure Evaluate(var ar : TArrayOfRepeatSettings);
  end;

var
  FormRepeatSettings: TFormRepeatSettings;

implementation

{$R *.lfm}

uses Math;

procedure TFormRepeatSettings.FormResize(Sender: TObject);
begin
end;

procedure TFormRepeatSettings.FreeChildControls(myControl : TWinControl; DoFree : boolean);
var
  i : integer;
  Item : TControl;
begin
  if Assigned(myControl) then
  begin
    for i := myControl.ControlCount - 1 downto 0 do
    begin
      Item := myControl.controls[i];
      if assigned(item) and (item is TWinControl) then FreeChildControls(item as TWinControl, true);
    end;

    if doFree then FreeAndNil(myControl);
  end;
end;

procedure TFormRepeatSettings.CreatePanels(count : integer);
const separation : integer = 100;
var i : integer;
  p : tpanel;
begin

  //FreeChildControls(sb, false);
  for i := low(iControlPanels) to high(iControlPanels) do
  begin
    iControlPanels[i].checkBoxPlayAudio.Free;
    iControlPanels[i].checkBoxShowTranslation.Free;
    iControlPanels[i].checkBoxShowTarget.Free;
    iControlPanels[i].panel.Free;
  end;


  SetLength(iControlPanels, count);

  for i := low(iControlPanels) to high(iControlPanels) do
  begin
    p := tpanel.create(sb);
    p.parent := sb;
    p.top := i * 20;
    p.Height := 20;
    p.left := 1;
    p.Width := 4 * separation;

    iControlPanels[i].checkBoxShowTarget := TCheckBox.Create(p);
    iControlPanels[i].checkBoxShowTarget.parent := p;
    iControlPanels[i].checkBoxShowTarget.Caption:= 'Target';
    iControlPanels[i].checkBoxShowTarget.left := 10;
    iControlPanels[i].checkBoxShowTarget.tag := i;
    iControlPanels[i].checkBoxShowTarget.OnChange := @CheckBox1Change;

    iControlPanels[i].checkBoxShowTranslation := TCheckBox.Create(p);
    iControlPanels[i].checkBoxShowTranslation.parent := p;
    iControlPanels[i].checkBoxShowTranslation.Caption:= 'Translation';
    iControlPanels[i].checkBoxShowTranslation.left := 10 + separation;
    iControlPanels[i].checkBoxShowTranslation.tag := i;
    iControlPanels[i].checkBoxShowTranslation.OnChange := @CheckBox2Change;

    iControlPanels[i].checkBoxPlayAudio := TCheckBox.Create(p);
    iControlPanels[i].checkBoxPlayAudio.parent := p;
    iControlPanels[i].checkBoxPlayAudio.Caption:= 'Play sound';
    iControlPanels[i].checkBoxPlayAudio.left := 10 + 2 * separation;
    iControlPanels[i].checkBoxPlayAudio.tag := i;
    iControlPanels[i].checkBoxPlayAudio.OnChange := @CheckBox3Change;

    iControlPanels[i].panel := p;
  end;
end;

procedure TFormRepeatSettings.SetChecks(const ar : TArrayOfRepeatSettings);
var i : integer;
begin
  for i := low(ar) to high(ar) do
  begin
    if i <= high(iControlPanels) then
    begin
      iControlPanels[i].checkBoxShowTarget.Checked := ar[i].showTarget;
      iControlPanels[i].checkBoxShowTranslation.Checked := ar[i].showTranslation;
      iControlPanels[i].checkBoxPlayAudio.Checked := ar[i].playAudio;
    end;
  end;
  for i := high(ar) + 1 to high(iControlPanels) do
  begin
    iControlPanels[i].checkBoxShowTarget.Checked := true;
    iControlPanels[i].checkBoxShowTranslation.Checked := true;
    iControlPanels[i].checkBoxPlayAudio.Checked := true;
  end;
end;



procedure TFormRepeatSettings.CheckBox1Change(Sender: TObject);
begin

end;

procedure TFormRepeatSettings.CheckBox2Change(Sender: TObject);
begin

end;

procedure TFormRepeatSettings.CheckBox3Change(Sender: TObject);
begin

end;

procedure TFormRepeatSettings.Init(const ar : TArrayOfRepeatSettings; c : integer);
begin
  // Use a maximum of, say, 50 controls
  CreatePanels(min(c, 50));
  SetChecks(ar);
end;

procedure TFormRepeatSettings.Evaluate(var ar : TArrayOfRepeatSettings);
var i : integer;
begin
  if length(iControlPanels) > length(ar) then
  begin
    SetLength(ar, length(iControlPanels));
  end;
  for i := low(iControlPanels) to high(iControlPanels) do
  begin
    ar[i].showTarget := iControlPanels[i].checkBoxShowTarget.Checked;
    ar[i].showTranslation := iControlPanels[i].checkBoxShowTranslation.Checked;
    ar[i].playAudio := iControlPanels[i].checkBoxPlayAudio.Checked;
  end;
end;

end.

