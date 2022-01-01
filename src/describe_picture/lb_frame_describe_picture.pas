// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Main frame

unit lb_frame_describe_picture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  lb_language_tool_types, lb_describe_picture_settings,
  lb_deepl_functionality;

type

  TFrameDescribe = class(TFrame)
    ButtonDeepL: TButton;
    ButtonLanguageTool: TButton;
    ButtonGetRandomPicture: TButton;
    EditTopic: TEdit;
    GroupBox4: TGroupBox;
    Image2: TImage;
    Label1: TLabel;
    LabelDetectedLanguage1: TLabel;
    LabelDetectedLanguage2: TLabel;
    LabelDetectedLanguage3: TLabel;
    MemoResult: TMemo;
    MemoInput: TMemo;
    PaintBox: TPaintBox;
    Panel1: TPanel;
    PanelCenter: TPanel;
    RadioButtonUnsplash: TRadioButton;
    FormalityRadioGroup: TRadioGroup;
    procedure ButtonDeepLClick(Sender: TObject);
    procedure ButtonLanguageToolClick(Sender: TObject);
    procedure ButtonGetRandomPictureClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);

  public
    constructor Create(AOwner : TComponent; const settings : TDescribePictureSettings);
    destructor Destroy; override;
  private
    iLtHints : TLanguageToolCorrection;
    iDlCorrection : string;
    iLtCorrection : string;
    iSentenceAndCorrections : array of TSentenceAndCorrection;
    iEntered : string;
    iSettings : TDescribePictureSettings;

    procedure CompareAndAssign(list : TStringList);
  end;

implementation

{$R *.lfm}

uses LazUTF8, FpJson, JsonParser, Math,
  lb_detect_language_errors, lb_random_picture,
  lb_needleman_wunsch, lb_draw_text,
  lb_lib, lb_split_string_into_sentences;

constructor TFrameDescribe.Create(AOwner : TComponent; const settings : TDescribePictureSettings);
begin
  inherited Create(AOwner);
  iSettings := settings;
  FormalityRadioGroup.ItemIndex := 0;
  LabelDetectedLanguage1.Caption := '';
  LabelDetectedLanguage2.Caption := '';
  LabelDetectedLanguage3.Caption := '';
  PanelCenter.Caption := '';
end;

destructor TFrameDescribe.Destroy;
begin
  inherited Destroy;
end;

procedure TFrameDescribe.ButtonGetRandomPictureClick(Sender: TObject);
begin
  GetPictureFromJsonAnswer(RequestUnsplash(iSettings.iUnsplashApiUrl, iSettings.iUnsplashApiKey,
      Image2.Width > Image2.Height, EditTopic.text), Image2.Picture);
end;

procedure TFrameDescribe.FrameResize(Sender: TObject);
begin
  Image2.Height := self.Height div 2;
  Panel1.Width := (self.Width - PanelCenter.Width) div 2;
end;


procedure TFrameDescribe.PaintBoxPaint(Sender: TObject);
const
  KLineHeight = 36;
var
  bitmap : TBitmap;
  s1, s2 : string;
begin

  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(PaintBox.Width, PaintBox.Height);
    bitmap.Canvas.Brush.Color := $00B3FFFF;
    bitmap.Canvas.FillRect(0, 0, bitmap.Width, bitmap.Height);

    bitmap.Canvas.Font.Name := 'Segoe UI';
    bitmap.Canvas.Font.Height := KLineHeight;

    bitmap.Canvas.MoveTo(3, 3);

    if iDlCorrection <> '' then
    begin
      AlignWithNeedlemanWunsch(iDlCorrection, iEntered, '*', -10, s1, s2);
      DrawTextConsideringAlignment(bitmap.Canvas, s1, s2);
    end
    else if iLtCorrection <> '' then
    begin
      AlignWithNeedlemanWunsch(iLtCorrection, iEntered, '*', -10, s1, s2);
      DrawTextConsideringAlignment(bitmap.Canvas, s1, s2);
    end;

    PaintBox.Canvas.Draw(0, 0, bitmap);
  finally
    bitmap.Free;
  end;
end;

procedure TFrameDescribe.ButtonLanguageToolClick(Sender: TObject);
var
  ltHint : TLanguageToolHint;
  json : string;
  i, j : integer;
  s : string;
begin
  if UTF8Length(MemoInput.Lines.Text) < 1 then
  begin
    exit;
  end;

  Initialize(iLtHints);
  iDlCorrection := '';

  json := CallLanguageToolDotOrgAPI(iSettings.iTargetLanguage, MemoInput.Lines.Text);

  iLtHints := GetCorrectionsFromLanguageTool(MemoInput.Lines.Text, json);

  // Replace the hints into the entered text.
  // Walk backwards to be able to keep original index values
  iEntered := MemoInput.Lines.Text;
  iLtCorrection := iEntered;
  for i := high(iLtHints.hints) downto low(iLtHints.hints) do
  begin
    ltHint := iLtHints.hints[i];
    Utf8Delete(iLtCorrection, ltHint.offset, ltHint.Length);
    if length(ltHint.replacements) > 0 then
    begin
      Utf8Insert(ltHint.replacements[0], iLtCorrection, ltHint.offset);
    end;
  end;

  // Give information
  LabelDetectedLanguage1.Caption := iLtHints.detectedLanguageCode;
  LabelDetectedLanguage2.Caption := iLtHints.detectedLanguage;
  LabelDetectedLanguage3.Caption := format('%d %%', [round(100.0 * iLtHints.detectedLanguageConfidence)]);

  MemoResult.lines.Clear;
  for i := low(iLtHints.hints) to high(iLtHints.hints) do
  begin
    ltHint := iLtHints.hints[i];
    s := format('%s (%s %s) [ ', [ltHint.inputPart, ltHint.issueType, ltHint.categoryId]);
    for j := low(ltHint.replacements) to min(7, high(ltHint.replacements)) do
    begin
      if j > 0 then s := s + ' / ';
      s := s + ltHint.replacements[j];
    end;
    s := s + '] ' + ltHint.message;
    MemoResult.Lines.Add(s);
  end;

  PaintBox.Invalidate;
end;

procedure TFrameDescribe.CompareAndAssign(list: TStringList);

  function FindSentence(const s : string) : integer;
  var i : integer;
  begin
    result := -1;
    for i := low(iSentenceAndCorrections) to high(iSentenceAndCorrections) do
    begin
      if iSentenceAndCorrections[i].iSentence = s then
      begin
        result := i;
        exit;
      end;
    end;
  end;

var i, index : integer;
  sentences : array of TSentenceAndCorrection;
begin
  // Create a temporary list, extracting unchanged items from the member list
  sentences := [];
  SetLength(sentences, list.Count);

  for i := 0 to list.Count - 1 do
  begin
    index := FindSentence(list[i]);
    if index >= 0 then
    begin
      sentences[i] := iSentenceAndCorrections[index];
      sentences[i].iSkip := true;
    end
    else
    begin
      sentences[i].iSentence := list[i];
    end;
  end;

  // Reassign
  iSentenceAndCorrections := sentences;
end;

procedure TFrameDescribe.ButtonDeepLClick(Sender: TObject);


  procedure CombineResults;
  var i : integer;
  begin
    iEntered := '';
    iDlCorrection := '';
    if length(iSentenceAndCorrections) = 0 then exit;

    iEntered := iSentenceAndCorrections[0].iSentence;
    iDlCorrection := iSentenceAndCorrections[0].iTranslatedTwice;

    for i := low(iSentenceAndCorrections) + 1 to high(iSentenceAndCorrections) do
    begin
      iEntered := iEntered + ' ' + iSentenceAndCorrections[i].iSentence;
      iDlCorrection := iDlCorrection + ' ' + iSentenceAndCorrections[i].iTranslatedTwice;
    end;
  end;

var i  : integer;
  list : TStringList;
  formality, debugTag : string;
  sac : TSentenceAndCorrection;
begin

  list := SplitStringIntoSentences(MemoInput.Text);
  try
    CompareAndAssign(list);
  finally
    list.free;
  end;

  MemoResult.Lines.clear;

  formality := '';
  if (FormalityRadioGroup.ItemIndex >= 0)
  and (iSettings.iTargetLanguage = 'RU') // TODO: "hasFormality" in ini-file
  then
  begin
    formality := FormalityRadioGroup.Items[FormalityRadioGroup.ItemIndex];
  end;

  for i := low(iSentenceAndCorrections) to high(iSentenceAndCorrections) do
  begin
    if iSentenceAndCorrections[i].iSkip then
    begin
      debugTag := 'SKIPPED because it was unaltered';
    end
    else
    begin
      iSentenceAndCorrections[i] := TranslateWithDeepL(iSettings, formality, iSentenceAndCorrections[i].iSentence);
      debugTag := 'TRANSLATED via ' + iSentenceAndCorrections[i].iViaLanguage;
    end;
    sac := iSentenceAndCorrections[i];

    // To check the meaning of the entered sentence, also provide it in the native language
    MemoResult.Lines.Add(format('%s->%s: %s (%s)', [iSettings.iTargetLanguage,
        iSettings.iCheckLanguage, sac.iCheckTranslation1, debugTag]));

    if sac.iCheckTranslation1 <> sac.iCheckTranslation2 then
    begin
      MemoResult.Lines.Add(format('%s->%s->%s->%s : %s (%s)',
        [iSettings.iTargetLanguage, sac.iViaLanguage, iSettings.iTargetlanguage,
        iSettings.iCheckLanguage,
        sac.iCheckTranslation2, debugTag]));
    end;
  end;
  CombineResults;

  PaintBox.Invalidate;
end;

end.

