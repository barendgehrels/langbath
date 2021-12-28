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
  lb_language_tool_types, lb_describe_picture_settings;

type

  { TFrameDescribe }

  TSentenceAndCorrection = record
    iSentence : string;
    iTranslatedTwice : string;
    iViaTranslation : string;
    iNativeTranslation1 : string;
    iNativeTranslation2 : string;
    iErrorMessage : string;
    iViaLanguage : string;
    iKeep : boolean;
    iLevenshteinDistance : integer;
  end;

  TFrameDescribe = class(TFrame)
    ButtonDeepL: TButton;
    ButtonLanguageTool: TButton;
    ButtonGetRandomPicture: TButton;
    EditTopic: TEdit;
    EditLanguageToCheck: TEdit;
    GroupBox4: TGroupBox;
    Image2: TImage;
    Label1: TLabel;
    ListBoxLt: TListBox;
    MemoNative: TMemo;
    MemoInput: TMemo;
    MemoOutput: TMemo;
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
    iDebugDeepL : boolean;

    procedure CompareAndAssign(list : TStringList);
  end;

implementation

{$R *.lfm}

uses LazUTF8, FpJson, JsonParser, Math,
  lb_detect_language_errors, lb_random_picture,
  lb_needleman_wunsch, lb_draw_text,
  lb_lib_string,
  lb_split_string_into_sentences;

constructor TFrameDescribe.Create(AOwner : TComponent; const settings : TDescribePictureSettings);
begin
  inherited Create(AOwner);
  iSettings := settings;
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

  json := RequestLanguageTool(EditLanguageToCheck.Text, MemoInput.Lines.Text);

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

  // List information
  ListBoxLt.Items.Clear;
  ListBoxLt.Items.Add(format('%s %s', [iLtHints.detectedLanguageCode, iLtHints.detectedLanguage]));
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
    ListBoxLt.Items.Add(s);
  end;


  // Debug/raw information
  MemoOutput.Lines.Clear;
  MemoOutput.Lines.Add(json);

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
      sentences[i].iKeep := true;
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

  function Translate(const src, target, lines : string; out json : string) : string;
  var formality : string;
  begin
    formality := '';
    if FormalityRadioGroup.ItemIndex >= 0 then
    begin
      formality := FormalityRadioGroup.Items[FormalityRadioGroup.ItemIndex];
    end;
    json := TranslateWithDeepL(iSettings.iDeepLApiUrl, iSettings.iDeepLApiKey, src, target, formality, lines);
    result := InterpretDeepLAnswer(json);
  end;

  function ProcessSentence(const sentence : string) : TSentenceAndCorrection;
  var
    translatedTwice, via, viaTranslation, jsonAnswer, j2, j3 : string;
    bestDistance, levDistance, i : integer;
  begin
    Initialize(result);
    result.iSentence := sentence;
    bestDistance := MaxInt;
    for i := low(iSettings.iViaLanguages) to high(iSettings.iViaLanguages) do
    begin
      via := iSettings.iViaLanguages[i];
      viaTranslation := Translate(iSettings.iTargetLanguage, via, sentence, jsonAnswer);
      if viaTranslation = '' then
      begin
        result.iErrorMessage := concat(result.iErrorMessage, via, ' ', jsonAnswer);
        if iDebugDeepL then
        begin
          MemoOutput.lines.Add('');
          MemoOutput.lines.Add(via + ' ' + jsonAnswer);
        end;
      end
      else
      begin
        translatedTwice := Translate(via, iSettings.iTargetLanguage, viaTranslation, jsonAnswer);
        levDistance := LevenshteinDistance(sentence, translatedTwice);

        if iDebugDeepL then
        begin
          MemoOutput.lines.Add('');
          MemoOutput.lines.Add(via + ' ' + inttostr(levDistance) + ' ' + sentence);
          MemoOutput.lines.Add('(' + viaTranslation + ')');
          MemoOutput.lines.Add(translatedTwice);
        end;

        if levDistance < bestDistance then
        begin
          result.iTranslatedTwice := translatedTwice;
          result.iViaLanguage := via;
          result.iLevenshteinDistance := levDistance;
          result.iViaTranslation := viaTranslation;
          bestDistance := levDistance;
        end else if levDistance = bestDistance then
        begin
          result.iViaLanguage := result.iViaLanguage + '/' + via;
          result.iViaTranslation := result.iViaTranslation + '/' + viaTranslation;
        end;
      end;
    end;
    if iSettings.iCheckLanguage <> '' then
    begin
      result.iNativeTranslation1 := Translate(iSettings.iTargetLanguage,
        iSettings.iCheckLanguage, sentence, j2);
      result.iNativeTranslation2 := Translate(iSettings.iTargetLanguage,
        iSettings.iCheckLanguage, result.iTranslatedTwice, j3);
    end;
  end;

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
  s : string;
begin
  list := SplitStringIntoSentences(MemoInput.Text);
  CompareAndAssign(list);
  list.free;

  MemoNative.Lines.clear;

  for i := low(iSentenceAndCorrections) to high(iSentenceAndCorrections) do
  begin
    s := 'KEEP';
    if not iSentenceAndCorrections[i].iKeep then
    begin
      iSentenceAndCorrections[i] := ProcessSentence(iSentenceAndCorrections[i].iSentence);
      s := 'CHECK';
    end;

    MemoOutput.lines.Add(format('%d %s [%s -> %s] (%s)',
      [iSentenceAndCorrections[i].iLevenshteinDistance,
       iSentenceAndCorrections[i].iTranslatedTwice,
       iSentenceAndCorrections[i].iViaLanguage,
       iSentenceAndCorrections[i].iViaTranslation, s]));

    // Experimental - to check the meaning
    MemoNative.Lines.Add(iSentenceAndCorrections[i].iNativeTranslation1);
    MemoNative.Lines.Add(iSentenceAndCorrections[i].iNativeTranslation2);
  end;
  CombineResults;

  PaintBox.Invalidate;
end;

end.

