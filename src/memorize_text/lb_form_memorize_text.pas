// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Main form

unit lb_form_memorize_text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, lb_frame_memorize_text;

type


  { TFormMemorizeText }

  TFormMemorizeText = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

    iFrame : TFrameMemorizeText;

  public

  end;

var
  FormMemorizeText: TFormMemorizeText;

implementation

{$R *.lfm}

uses Controls, Dialogs, Inifiles, lb_config;

{ TFormMemorizeText }


procedure TFormMemorizeText.FormCreate(Sender: TObject);
var ini : TIniFile;
  list : TStringList;
  i : integer;
begin
  WindowState := wsMaximized;

  iFrame := TFrameMemorizeText.Create(self);
  iFrame.Parent := self;
  iFrame.Align := alClient;

  ini := TIniFile.Create(IniFileName);
  list := TStringList.Create;
  try
    ini.ReadSection('memorize_text', list);
    for i := 0 to list.count - 1 do
    begin
      iFrame.AddEntry(list[i]);
    end;
  finally
    ini.free;
    list.free;
  end;
end;

procedure TFormMemorizeText.FormDestroy(Sender: TObject);
begin
  FreeAndNil(iFrame);
end;


end.

