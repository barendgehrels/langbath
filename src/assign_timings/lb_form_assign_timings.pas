// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This form contains the frame with all sentences, and some controls for book management
// (selection, edit settings, add book)

unit lb_form_assign_timings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lb_frame_book_sentences, lb_book_settings;

type

  { TFormAssignTimes }

  TFormAssignTimes = class(TForm)
    ButtonEdit: TButton;
    ButtonAdd: TButton;
    CheckBoxSaveOnExit: TCheckBox;
    ComboBoxProject: TComboBox;
    Panel1: TPanel;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ComboBoxProjectChange(Sender: TObject);
    procedure ComboBoxProjectSelect(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    iFrameReadSentences : TFrameReadSentences;

    iSettings : TBookSettings;

    procedure ShowCurrent;

    procedure ReadSettings(const bookId : string = '');
    procedure ReadContents;
    procedure CallRead;

    procedure CallSave;
    procedure SaveSettings;
    procedure SaveContents(force : boolean = false);
  public

  end;

var
  FormAssignTimes: TFormAssignTimes;

implementation

{$R *.lfm}

{ TFormAssignTimes }

uses LCLIntf, LCLType, ComCtrls, lb_read_timings, lb_write_timings,
  lb_form_edit_book_settings, lb_bass,
  lb_lib;

procedure TFormAssignTimes.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  InitBass(handle);
  {$ELSE}
  InitBass(nil);
  {$ENDIF}

  // Create all frames at runtime,
  // to avoid leaking properties into their containing form, with loads of
  // runtime errors.
  iFrameReadSentences := TFrameReadSentences.Create(self);
  iFrameReadSentences.Parent := self;
  iFrameReadSentences.Align := alClient;

  CallRead;
end;

procedure TFormAssignTimes.ButtonAddClick(Sender: TObject);
var newSettings : TBookSettings;
  list : TStringList;
begin
  newSettings := DefaultBookSettings;

  FormEditBookSettings.SetSettings(newSettings);

  // Take care that the same ID is never used again
  list := TStringList.Create;
  try
    ReadSettingsToStringList('', list);
    FormEditBookSettings.SetExistingIds(list);
  finally
    list.free;
  end;

  FormEditBookSettings.ShowModal;
  if FormEditBookSettings.ModalResult = mrOK then
  begin
    newSettings := FormEditBookSettings.GetSettings;
    SaveEditedBookSettings(newSettings);
    // FOR NOW - it will be read next time

  end;
end;

procedure TFormAssignTimes.ButtonEditClick(Sender: TObject);
var updatedSettings : TBookSettings;
begin
  log('Before update ' + inttostr(isettings.iCurrentSentenceIndex));
  FormEditBookSettings.SetSettings(iSettings);
  FormEditBookSettings.ShowModal;
  if FormEditBookSettings.ModalResult = mrOK then
  begin
    updatedSettings := FormEditBookSettings.GetSettings;

    if updatedSettings <> iSettings then
    begin
      iSettings := updatedSettings;
      log('After update ' + inttostr(isettings.iCurrentSentenceIndex));

      // Save current work (because the files might be changed)
      SaveContents(true);

      // Update (will be done automatically anyway, on exit)
      SaveEditedBookSettings(iSettings);

      // If soundfile is updated, or anything else, the contents should be reread
      // but it forces loss of focus
      ReadContents;
      ShowCurrent;
    end
    else
    begin
      log('Settings didn''t change');
    end;
  end;
end;

procedure TFormAssignTimes.ComboBoxProjectChange(Sender: TObject);
begin

end;


procedure TFormAssignTimes.ComboBoxProjectSelect(Sender: TObject);
var id : string;
begin
  if ComboBoxProject.ItemIndex >= 0 then
  begin
    id := ComboBoxProject.Items[ComboBoxProject.ItemIndex];
  end;

  if (id <> '') and (id <> iSettings.iBookId) then
  begin
    CallSave;
    ReadSettings(id);
    ReadContents;

    ShowCurrent;
  end;
end;

procedure TFormAssignTimes.FormDestroy(Sender: TObject);
begin
  CallSave;
  iFrameReadSentences.Free;
  EndBass;
end;

procedure TFormAssignTimes.ReadSettings(const bookId : string);
var index : integer;
begin
  iSettings := ReadBookSettings(bookId);
  index := ReadSettingsToStringList(iSettings.iBookId, ComboBoxProject.Items);
  ComboBoxProject.ItemIndex := index;
end;

procedure TFormAssignTimes.ReadContents;
begin
  ReadTargetIntoListView(iFrameReadSentences.ListViewSentences, iSettings.iFilenameTarget);
  ReadTimingsIntoListView(iFrameReadSentences.ListViewSentences, iSettings.iFilenameTimings);
  ReadTranslationIntoListView(iFrameReadSentences.ListViewSentences, iSettings.iFilenameTranslation);
  iFrameReadSentences.ReadSound(iSettings.iFilenameSound);
  iFrameReadSentences.TimesDirty := false;
  iFrameReadSentences.SentencesDirty := false;
end;

procedure TFormAssignTimes.CallRead;
begin
  ReadSettings;
  ReadContents;
end;

procedure TFormAssignTimes.SaveContents(force : boolean);
begin
  if force or iFrameReadSentences.TimesDirty then
  begin
    WriteTimingsFromListView(iFrameReadSentences.ListViewSentences, iSettings.iFilenameTimings);
    iFrameReadSentences.TimesDirty := false;
  end;
  if force or iFrameReadSentences.SentencesDirty then
  begin
    WriteColumnFromListView(iFrameReadSentences.ListViewSentences, 1, iSettings.iFilenameTarget);
    WriteColumnFromListView(iFrameReadSentences.ListViewSentences, 2, iSettings.iFilenameTranslation);
    iFrameReadSentences.SentencesDirty := false;
  end;
end;

procedure TFormAssignTimes.SaveSettings;
var item : TListItem;
begin
  item := iFrameReadSentences.ListViewSentences.Selected;
  if item <> nil then
  begin
    SaveBookSettings(iSettings, item.Index);
  end
  else
  begin
    SaveBookSettings(iSettings, -1);
  end;
end;

procedure TFormAssignTimes.CallSave;
begin
  if CheckBoxSaveOnExit.Checked then
  begin
    SaveContents;
    SaveSettings;
  end;
end;

procedure TFormAssignTimes.ShowCurrent;
var item : TListItem;
  sentenceIndex : integer;
begin
  sentenceIndex := iSettings.iCurrentSentenceIndex;
  if sentenceIndex < 0 then sentenceIndex := 0;

  if (sentenceIndex >= 0)
  and (sentenceIndex < iFrameReadSentences.ListViewSentences.Items.Count)
  then
  begin
    item := iFrameReadSentences.ListViewSentences.Items[sentenceIndex];
    item.Selected := true;
    item.Focused := true;
    item.MakeVisible(false);
  end
end;

procedure TFormAssignTimes.FormShow(Sender: TObject);
begin
  WindowState := wsMaximized;
  ShowCurrent;
end;

end.

