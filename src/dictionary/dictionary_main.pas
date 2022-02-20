unit dictionary_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lb_frame_search_words, lb_frame_show_sentences, lb_db_manager, lb_ini_dictionary;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    iFrameSearchWords : TFrameSearchWords;
    iFrameShowSentences : TFrameShowSentences;
    iDbLanguage : TDbManager;
    iDbTrain : TDbManager;
    iSettings : TDictionarySettings;

    procedure OnWordSearch(wordId : integer);
    procedure CreateDatamodel;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses lb_bass, lb_form_train, lb_db_train;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitBass(handle);

  iSettings := GetDictionarySettings;

  if not FileExists(iSettings.databaseLanguage) then
  begin
    ShowMessage(format('SQLite database not found: %s', [iSettings.databaseLanguage]));
    exit;
  end;

  WindowState := wsMaximized;
  iDbLanguage := TDbManager.Create(iSettings.databaseLanguage);
  if iSettings.databaseTrain <> '' then
  begin
    iDbTrain := TDbManager.Create(iSettings.databaseTrain);
    CreateDatamodel;
  end;

  iFrameSearchWords := TFrameSearchWords.Create(self);
  iFrameSearchWords.Parent := Panel1;
  iFrameSearchWords.Align := alClient;
  iFrameSearchWords.Connection := iDbLanguage.Connection;
  iFrameSearchWords.OnSearch := @OnWordSearch;

  iFrameShowSentences := TFrameShowSentences.Create(self);
  iFrameShowSentences.Parent := Panel2;
  iFrameShowSentences.Align := alClient;
  iFrameShowSentences.Connection := iDbLanguage.Connection;
  iFrameShowSentences.FolderSoundCache := iSettings.folderSoundCache;
  iFrameShowSentences.UrlOpenRussian := iSettings.UrlOpenRussian;
  iFrameShowSentences.UrlTatoeba := iSettings.UrlTatoeba;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  iDbLanguage.Free;
  iDbTrain.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FormTrain.ConnectionLanguage := iDbLanguage.Connection;
  formTrain.ConnectionTrain := iDbTrain.Connection;
end;

procedure TMainForm.OnWordSearch(wordId: integer);
begin
  iFrameShowSentences.ShowSentencesOfWord(wordId);
end;

procedure TMainForm.CreateDatamodel;
begin
  if iDbTrain.IsNew
  or not iDbTrain.HasTable('train')
  or not iDbTrain.HasTable('scores')
  then
  begin
    CreateTrainTables(iDbTrain.Connection);
    iDbTrain.Transaction.commit;
  end;
end;

end.

