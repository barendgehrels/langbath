unit lb_form_paste;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormPaste }

  TFormPaste = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    MemoTarget: TMemo;
    MemoNew: TMemo;
    MemoCurrent: TMemo;
    PanelClient: TPanel;
    PanelGlue1: TPanel;
    PanelGlue2: TPanel;
    PanelOkCancel: TPanel;
    procedure PanelClientResize(Sender: TObject);
  private

  public
    function ClipboardLineCount : integer;
    procedure SetCurrentTranslation(list : TStrings);
    procedure Paste;
    function GetNewTranslation : TStrings;

  end;

var
  FormPaste: TFormPaste;

implementation

{$R *.lfm}

uses Clipbrd, lb_lib;

procedure TFormPaste.PanelClientResize(Sender: TObject);
var h : integer;
begin
  PanelGlue1.Height := PanelClient.BorderWidth;
  PanelGlue2.Height := PanelClient.BorderWidth;
  h := (PanelClient.Height - 4 * PanelClient.BorderWidth) div 3;
  MemoTarget.Height := h;
  MemoCurrent.Height := h;
end;

function TFormPaste.ClipboardLineCount: integer;
var list : TStringList;
begin
  result := 0;
  if Clipboard.HasFormatName('text/plain') then
  begin
    list := TStringList.Create;
    try
      list.Text := Clipboard.AsText;
      result := list.count;
    finally
      list.free;
    end;
  end;
end;

procedure TFormPaste.SetCurrentTranslation(list : TStrings);
begin
//TODO: set target
  MemoCurrent.Text := list.Text;
end;

procedure TFormPaste.Paste;
var list : TStringList;
begin
  memoNew.clear;

  if Clipboard.HasFormatName('text/plain') then
  begin
    list := TStringList.Create;
    try
      list.Text := Clipboard.AsText;
      MemoNew.Text := list.Text;
      //MemoNew.Append(inttostr(list.count));
    finally
      list.free;
    end;
  end;
end;

function TFormPaste.GetNewTranslation: TStrings;
begin
  result := MemoNew.Lines;
end;

end.

