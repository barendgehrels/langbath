// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This unit contains a form to edit (or add) settings for a book:
// - meta information (title, author, language)
// - file information (target, translation, timings, and sound)
// - source information (usually URL's)

unit lb_form_edit_book_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, lb_book_settings;

type

  { TFormEditBookSettings }

  TFormEditBookSettings = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonSelectSound: TButton;
    ButtonSelectTarget: TButton;
    ButtonSelectTimings: TButton;
    ButtonSelectTranslation: TButton;
    EditAuthor: TEdit;
    EditSrcSound: TEdit;
    EditSrcTarget: TEdit;
    EditTitle: TEdit;
    EditTarget: TEdit;
    EditTimings: TEdit;
    EditTranslation: TEdit;
    EditSound: TEdit;
    EditId: TEdit;
    EditSrcTranslation: TEdit;
    LabelSrcSound: TLabel;
    LabelSrcTarget: TLabel;
    LabelTitle: TLabel;
    LabelSound: TLabel;
    LabelId: TLabel;
    LabelAuthor: TLabel;
    LabelTranslation: TLabel;
    LabelTimings: TLabel;
    LabelTarget: TLabel;
    LabelSrcTranslation: TLabel;
    OpenDialog: TOpenDialog;
    PanelMain: TPanel;
    PanelOkCancel: TPanel;
    procedure ButtonSelectSoundClick(Sender: TObject);
    procedure ButtonSelectTargetClick(Sender: TObject);
    procedure ButtonSelectTimingsClick(Sender: TObject);
    procedure ButtonSelectTranslationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelMainResize(Sender: TObject);
  private
    iSettings : TBookSettings;
    procedure Select(edit : TEdit; isText, mustExist : boolean);
  public
    procedure SetSettings(const settings : TBookSettings);
    function GetSettings : TBookSettings;
  end;

var
  FormEditBookSettings: TFormEditBookSettings;

implementation

{$R *.lfm}

{ TFormEditBookSettings }

procedure TFormEditBookSettings.Select(edit : TEdit; isText, mustExist : boolean);

  function TakeOver(otherEdit : TEdit) : boolean;
  begin
    result := FileExists(otherEdit.Text);
    if result then
    begin
      OpenDialog.InitialDir := ExtractFilePath(otherEdit.text);
    end;
  end;

begin
  if isText
  then OpenDialog.Filter := 'Text Files|*.txt;*.TXT;'
  else OpenDialog.Filter := 'Music Files|*.mp3;*.MP3;';

  if mustExist
  then OpenDialog.Options := OpenDialog.Options - [ofCreatePrompt] + [ofFileMustExist]
  else OpenDialog.Options := OpenDialog.Options + [ofCreatePrompt] - [ofFileMustExist];

  if FileExists(edit.text) then
  begin
    OpenDialog.InitialDir := ExtractFilePath(edit.text);
  end
  else
  begin
    // Take over the folder from one of the other files
    if not (TakeOver(EditTarget)
            or TakeOver(EditTranslation)
            or TakeOver(EditTimings)
            or TakeOver(EditSound))
    then
    begin
      // Nothing exists... Leave as default, or use previous choice
    end;
  end;

  OpenDialog.Filename := edit.text;

  if OpenDialog.Execute then
  begin
    edit.text := OpenDialog.FileName;
  end;
end;

procedure TFormEditBookSettings.ButtonSelectTargetClick(Sender: TObject);
begin
  Select(EditTarget, true, true);
end;

procedure TFormEditBookSettings.ButtonSelectSoundClick(Sender: TObject);
begin
  Select(EditSound, false, true);
end;

procedure TFormEditBookSettings.ButtonSelectTimingsClick(Sender: TObject);
begin
  Select(EditTimings, true, false);
end;

procedure TFormEditBookSettings.ButtonSelectTranslationClick(Sender: TObject);
begin
  Select(EditTranslation, true, false);
end;

procedure TFormEditBookSettings.FormShow(Sender: TObject);
begin
  EditId.ReadOnly := EditId.Text <> '';
end;

// Take care of widths and precise alignments
procedure TFormEditBookSettings.PanelMainResize(Sender: TObject);

  procedure SetWidth(margin : integer; edit : TEdit; extra : integer = 0);
  begin
    edit.Width := PanelMain.Width - (3 * margin + ButtonSelectTarget.Width) - extra;
  end;

  procedure SetWidthShort(margin : integer; edit : TEdit);
  begin
    edit.Width := (PanelMain.Width - 3 * margin) div 2;
  end;

  procedure AlignControls(margin : integer; edit : TEdit; lab : TLabel; button : TButton);
  begin
    edit.Left := margin;

    lab.Left := margin;
    lab.Top := edit.Top - (lab.Height + 1);

    if button <> nil then
    begin
      button.Top := edit.Top - (button.Height - edit.Height);
      button.Left := EditTarget.Width + 2 * margin;
    end;
  end;

  procedure AlignTop(var current : integer; edit : TEdit; height : integer);
  begin
    current := current + height;
    edit.top := current;
  end;

  procedure AlignTops(var current : integer; edit, editSrc : TEdit; height : integer);
  begin
    current := current + height;
    edit.top := current;
    current := current + height;
    editSrc.top := current - 5;
  end;


var margin, rowHeight, current : integer;
begin
  PanelMain.Caption := '';

  // EditAuthor acts as a model, its left margin is also used as right margin
  // and as spacing between edit and Select buttons
  margin := EditAuthor.Left;

  SetWidthShort(margin, EditAuthor);
  SetWidthShort(margin, EditId);
  SetWidth(margin, EditTitle);
  SetWidth(margin, EditTarget);
  SetWidth(margin, EditTranslation);
  SetWidth(margin, EditTimings);
  SetWidth(margin, EditSound);

  SetWidth(margin, EditSrcTarget, margin * 2);
  SetWidth(margin, EditSrcTranslation, margin * 2);
  SetWidth(margin, EditSrcSound, margin * 2);

  // Vertical alignments (spaced equally)
  rowHeight := PanelMain.Height div 9;
  current := EditAuthor.Top;
  AlignTop(current, EditTitle, rowHeight);
  AlignTops(current, EditTarget, EditSrcTarget, rowHeight);
  AlignTops(current, EditTranslation, EditSrcTranslation, rowHeight);
  AlignTops(current, EditSound, EditSrcSound, rowHeight);
  AlignTop(current, EditTimings, rowHeight);

  // Horizontal alignments and alignment of labels
  AlignControls(margin, EditAuthor, LabelAuthor, nil);
  AlignControls(margin, EditTitle, LabelTitle, nil);
  AlignControls(margin, EditTarget, LabelTarget, ButtonSelectTarget);
  AlignControls(margin, EditTranslation, LabelTranslation, ButtonSelectTranslation);
  AlignControls(margin, EditTimings, LabelTimings, ButtonSelectTimings);
  AlignControls(margin, EditSound, LabelSound, ButtonSelectSound);

  // Edits containing sources are indented
  AlignControls(margin * 3, EditSrcTarget, LabelSrcTarget, nil);
  AlignControls(margin * 3, EditSrcTranslation, LabelSrcTranslation, nil);
  AlignControls(margin * 3, EditSrcSound, LabelSrcSound, nil);

  // The ID, right from author, gets another margin
  AlignControls(margin * 2 + EditAuthor.Width, EditId, LabelId, nil);
end;

procedure TFormEditBookSettings.SetSettings(const settings : TBookSettings);
begin
  iSettings := settings;

  EditId.Text := settings.iBookId;
  EditAuthor.Text := settings.iAuthor;
  EditTitle.Text := settings.iTitle;

  EditTarget.Text := settings.iFilenameTarget;
  EditTranslation.Text := settings.iFilenameTranslation;
  EditTimings.Text := settings.iFilenameTimings;
  EditSound.Text := settings.iFilenameSound;

  EditSrcTarget.Text := settings.iSrcTarget;
  EditSrcTranslation.Text := settings.iSrcTranslation;
  EditSrcSound.Text := settings.iSrcSound;
end;

function TFormEditBookSettings.GetSettings : TBookSettings;
begin
  result := iSettings;

  result.iBookId := EditId.Text;
  result.iAuthor  := EditAuthor.Text;
  result.iTitle  := EditTitle.Text;
  result.iFilenameTarget := EditTarget.Text;
  result.iFilenameTranslation := EditTranslation.Text;
  result.iFilenameTimings := EditTimings.Text;
  result.iFilenameSound := EditSound.Text;

  result.iSrcTarget := EditSrcTarget.Text;
  result.iSrcTranslation := EditSrcTranslation.Text;
  result.iSrcSound := EditSrcSound.Text;
end;

end.

