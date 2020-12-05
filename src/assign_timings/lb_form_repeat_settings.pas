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


  { TFormRepeatSettings }

  TFormRepeatSettings = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    PanelOkCancel: TPanel;
    procedure FormResize(Sender: TObject);
  private

  public

    procedure Init(ar : TArrayOfRepeatSettings; c : integer);
    procedure Evaluate(var ar : TArrayOfRepeatSettings);
  end;

var
  FormRepeatSettings: TFormRepeatSettings;

implementation

{$R *.lfm}

uses Math;

procedure TFormRepeatSettings.FormResize(Sender: TObject);
begin
  CheckGroup1.Width := self.Width div 2;
end;

procedure TFormRepeatSettings.Init(ar : TArrayOfRepeatSettings; c : integer);
var i : integer;
begin
  // More than 50 parameters is not supported in settings.
  c := min(c, 50);

  CheckGroup1.Items.Clear;
  CheckGroup2.Items.Clear;

  CheckGroup1.Columns := ((c - 1) div 10) + 1;
  CheckGroup2.Columns := ((c - 1) div 10) + 1;

  for i := 0 to c - 1 do
  begin
    CheckGroup1.Items.Add(format('%d', [i + 1]));
    CheckGroup2.Items.Add(format('%d', [i + 1]));
    if i <= high(ar) then
    begin
      CheckGroup1.Checked[i] := ar[i].showTarget;
      CheckGroup2.Checked[i] := ar[i].showTranslation;
    end
    else
    begin
      CheckGroup1.Checked[i] := true;
      CheckGroup2.Checked[i] := true;
    end;
  end;
end;

procedure TFormRepeatSettings.Evaluate(var ar : TArrayOfRepeatSettings);
var i : integer;
begin
  assert(CheckGroup1.Items.Count = CheckGroup2.Items.Count);
  if CheckGroup1.Items.Count > length(ar) then
  begin
    SetLength(ar, CheckGroup1.Items.Count);
  end;
  for i := 0 to CheckGroup1.Items.Count - 1 do
  begin
    ar[i].showTarget := CheckGroup1.Checked[i];
    ar[i].showTranslation := CheckGroup2.Checked[i];
  end;
end;

end.

