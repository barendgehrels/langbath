// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This frame contains a listview with sentences (usually of a book) and translations,
// and their timings, and an optional rating.
//   - it can read out loud the sentences (called: the target)
//   - it can repeat these sentences
//   - during repeated replay target language/translation can be set to visible/invisible (TODO)
//   - the user can edit timings (begin/end per sentence)
//   - the user can graphically edit timings
//   - the user can rate sentences
//   - the user can merge and split sentences (and their translations and timings)
//   - the user can search sentences (TODO)
//   - the user can copy/paste the target to facilitate translations (TODO)

unit lb_frame_book_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, lb_bass, lb_time_optimizer, lb_form_repeat_settings, LCLType;

type

  { TFrameReadSentences }

  TFrameReadSentences = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    ButtonWaveForm: TButton;
    ButtonGoToCurrent: TSpeedButton;
    ButtonMerge: TButton;
    ButtonPlay: TSpeedButton;
    ButtonSplit: TButton;
    ButtonStop: TSpeedButton;
    CheckBoxTranslation: TCheckBox;
    EditRepeating: TEdit;
    ListViewSentences: TListView;
    MemoSentence: TMemo;
    MemoTranslation: TMemo;
    PanelTopCenter: TPanel;
    PanelTranslation: TPanel;
    PanelLength: TPanel;
    Panel3: TPanel;
    PanelLevel: TPanel;
    PanelTop: TPanel;
    ProgressBarLevel: TProgressBar;
    ProgressBarSentence: TProgressBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButtonNone: TRadioButton;
    RadioGroupPlay: TRadioGroup;
    Shape1: TShape;
    Splitter1: TSplitter;
    TimerLevel: TTimer;
    TimerAllSound: TTimer;
    TimerRepeatAndNext: TTimer;
    TrackBarAllSound: TTrackBar;
    procedure Button2Click(Sender: TObject);
    procedure ButtonSplitClick(Sender: TObject);
    procedure ButtonGoToEditPlaceClick(Sender: TObject);
    procedure ButtonMergeClick(Sender: TObject);
    procedure ButtonWaveFormClick(Sender: TObject);
    procedure CheckBoxTranslationChange(Sender: TObject);
    procedure ListViewSentencesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MemoTranslationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoTranslationChange(Sender: TObject);
    procedure MemoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel3Resize(Sender: TObject);
    procedure PanelTopResize(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure RadioButtonNoneChange(Sender: TObject);
    procedure RadioButtonRatingChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerLevelTimer(Sender: TObject);
    procedure TimerAllSoundTimer(Sender: TObject);
    procedure TimerRepeatAndNextTimer(Sender: TObject);
  private

    iEditTimes : boolean;

    iRepeatingCount : integer;
    iPlaybackBeginPosition : double;
    iPlaybackEndPosition : double;

    iIsUpdating : boolean;

    iBass : TLbBass;
    iTimesDirty : boolean;
    iSentencesDirty : boolean;

    iRepeatSettings : TArrayOfRepeatSettings;

    procedure StopPlaying;
    procedure PlaySentence(item : TListItem; timer : TTimer; additionalMs : integer);
    procedure PlayFromCurrentSentence;

    function IsSplitEnabled : boolean;
    procedure SelectItem(item : TListItem; selected : Boolean);

    procedure AssignPeriod(item : TListItem; isBegin : boolean; pos : double);
    procedure SampleAssignPeriod(item : TListItem; isBegin : boolean; pos : double);
    procedure IndicateDirty(colIndex : integer; v : boolean; const title : string);
    procedure SetTimesDirty(v : boolean);
    procedure SetSentencesDirty(v : boolean);

  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReadSound(const filename : string);

    property TimesDirty : boolean read iTimesDirty write SetTimesDirty;
    property SentencesDirty : boolean read iSentencesDirty write SetSentencesDirty;
  end;

implementation

{$R *.lfm}

uses LazUtf8, Math, lb_lib, lb_ui_lib, lb_form_wave_form;

const
  KSepRepeating : integer = 0;
  KSepOne : integer = 1000;

procedure ExtraLog(const s : string);
begin
  // For debugging
end;

constructor TFrameReadSentences.Create(TheOwner: TComponent);
begin
  inherited Create(theOwner);
end;

destructor TFrameReadSentences.Destroy;
begin
  iBass.Free;
  inherited Destroy;
end;

procedure TFrameReadSentences.ReadSound(const filename : string);
begin
  StopPlaying;
  iBass.Free;
  iBass := TLbBass.Create(filename, true);
  TrackBarAllSound.max := trunc(1000.0 * iBass.LengthSeconds);
end;

procedure TFrameReadSentences.ButtonGoToEditPlaceClick(Sender: TObject);
var i : integer;
  it : TListItem;
begin
  ListViewSentences.ItemIndex := -1;
  // Deliberately skip the first one
  for i := 1 to ListViewSentences.Items.Count - 1 do
  begin
    it := ListViewSentences.Items[i];
    if (it.SubItems.Count < 2) or (it.Caption = '') or (it.SubItems[0] = '') then
    begin
      it.Selected := true;
      it.Focused := true;
      it.MakeVisible(false);
      exit;
    end;
  end;

end;

function TFrameReadSentences.IsSplitEnabled : boolean;
const KMinLength : integer = 1;
var len1, len2 : integer;
  item  : TListItem;
begin
  result := false;
  len1 := Utf8Length(MemoSentence.Text);
  len2 := Utf8Length(MemoTranslation.Text);
  item := ListViewSentences.Selected;
  if (ListViewSentences.SelCount <> 1)
  or (item = nil)
  or (MemoSentence.SelLength < KMinLength)
  or (MemoTranslation.SelLength < KMinLength)
  or (len1 - MemoSentence.SelLength < KMinLength)
  or (len2 - MemoTranslation.SelLength < KMinLength)
  then
  begin
    exit;
  end;

  // Splitting is only possible at begin (1) or end (calculate length)
  if (MemoSentence.SelStart = 1) and (MemoTranslation.SelStart <> 1) then
  begin
    exit;
  end;

  if (MemoSentence.SelStart > 1)
  and (   (MemoSentence.SelStart + MemoSentence.SelLength <> len1)
       or (MemoTranslation.SelStart + MemoTranslation.SelLength <> len2))
  then
  begin
    exit;
  end;

  result := true;
end;


procedure TFrameReadSentences.ButtonSplitClick(Sender: TObject);
var item, newItem : TListItem;
begin
  // Insert a new item, and fill it with the selection
  item := ListViewSentences.Selected;
  if IsSplitEnabled and (item <> nil) then
  begin
    newItem := ListViewSentences.Items.Insert(item.Index + 1);
    newItem.SubItems.Add(item.SubItems[0]);
    newItem.SubItems.Add(trim(MemoSentence.SelText));
    newItem.SubItems.Add(trim(MemoTranslation.SelText));

    // Edit this item
    item.Checked := false;
    item.SubItems[0] := '';
    item.SubItems[1] := trim(Utf8Copy(item.SubItems[1], 1, MemoSentence.SelStart));
    item.SubItems[2] := trim(Utf8Copy(item.SubItems[2], 1, MemoTranslation.SelStart));
    // Empty rating, if any
    if item.SubItems.Count > 3 then item.SubItems[3] := '';

    // Reselect it to show its new contents
    SelectItem(item, true);

    SetTimesDirty(true);
    SetSentencesDirty(true);
  end;
end;

procedure TFrameReadSentences.Button2Click(Sender: TObject);
var maxCount : integer;
begin
  if TryStrToInt(EditRepeating.Caption, maxCount) then
  begin
    FormRepeatSettings.Init(iRepeatSettings, maxCount);
    FormRepeatSettings.ShowModal;
    if FormRepeatSettings.ModalResult = mrOK then
    begin
      FormRepeatSettings.Evaluate(iRepeatSettings);
    end;
  end;
end;

procedure TFrameReadSentences.ButtonMergeClick(Sender: TObject);
var first, second : TListItem;
  i : integer;
begin
  first := ListViewSentences.Selected;
  if first = nil then exit;
  second := ListViewSentences.GetNextItem(first, sdBelow, [lisSelected]);
  if second = nil then exit;

  // Subitems: 0=end, 1=target, 2=translation, 3=rating if any
  // Merge times to one period
  first.SubItems[0] := second.SubItems[0];

  // Empty rating, if any (ratings are not merged)
  while first.SubItems.Count > 3 do first.SubItems.Delete(3);

  // Merge texts by concatenation
  for i := 1 to Min(first.SubItems.Count, second.SubItems.Count) - 1 do
  begin
    first.SubItems[i] := trim(first.SubItems[i]) + ' ' + trim(second.SubItems[i]);
  end;

  // Delete merged item
  ListViewSentences.Items.Delete(second.Index);

  // Reselect it to show its new contents
  SelectItem(first, true);

  SetTimesDirty(true);
  SetSentencesDirty(true);
end;

procedure TFrameReadSentences.ButtonWaveFormClick(Sender: TObject);
var first, second : TListItem;
  pos1, pos2 : double;
begin
  first := ListViewSentences.Selected;
  if first = nil then exit;
  second := ListViewSentences.GetNextItem(first, sdBelow, [lisSelected]);
  if second = nil then exit;

  if (first.Index + 1 = second.Index)
  and (first.SubItems.Count > 0)
  and TryStrToFloat(first.SubItems[0], pos1)
  and TryStrToFloat(second.Caption, pos2)
  then
  begin
    FormWaveForm.Initialize(iBass, pos1, pos2);

    if TryStrToFloat(first.Caption, pos1) then
    begin
       FormWaveForm.AddPositionIndication(pos1);
    end;
    if (second.SubItems.Count > 0)
    and TryStrToFloat(second.SubItems[0], pos2) then
    begin
      FormWaveForm.AddPositionIndication(pos2);
    end;


    TimerLevel.Enabled := false;
    TimerAllSound.Enabled := false;
    try
      FormWaveForm.ShowModal;
      if FormWaveForm.ModalResult = mrOK then
      begin
        AssignPeriod(ListViewSentences.Items[first.Index], false, FormWaveForm.GetPosition1);
        AssignPeriod(ListViewSentences.Items[second.Index], true, FormWaveForm.GetPosition2);
      end;

    finally
      TimerLevel.Enabled := true;
      TimerAllSound.Enabled := true;
    end;
  end;
end;

procedure TFrameReadSentences.CheckBoxTranslationChange(Sender: TObject);
begin
  memoTranslation.Visible := CheckBoxTranslation.Checked;
end;

procedure TFrameReadSentences.ListViewSentencesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SelectItem(item, selected);
end;

procedure TFrameReadSentences.MemoTranslationKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ButtonSplit.Enabled := IsSplitEnabled;
end;

procedure TFrameReadSentences.SelectItem(item : TListItem; selected : Boolean);

  function TwoConsecutiveRowsSelected : boolean;
  var first, second : TListItem;
  begin
    result := false;
    if ListViewSentences.SelCount = 2 then
    begin
      first := ListViewSentences.Selected;
      if first <> nil then
      begin
        second := ListViewSentences.GetNextItem(first, sdBelow, [lisSelected]);
        if second <> nil then
        begin
          result := second.Index = first.Index + 1;
        end;
      end;
    end;
  end;

var rating : integer;
  oneSelected : boolean;
begin
  oneSelected := ListViewSentences.SelCount = 1;
  ButtonMerge.Enabled := TwoConsecutiveRowsSelected and not iBass.Active;
  ButtonSplit.Enabled := IsSplitEnabled and not iBass.Active;
  ButtonWaveForm.Enabled := TwoConsecutiveRowsSelected and not iBass.Active;
  RadioButtonNone.Enabled := oneSelected;
  RadioButton1.Enabled := oneSelected;
  RadioButton2.Enabled := oneSelected;
  RadioButton3.Enabled := oneSelected;
  RadioButton4.Enabled := oneSelected;
  RadioButton5.Enabled := oneSelected;

  if item = nil then
  begin
    exit;
  end;

  iIsUpdating := true;
  try

    // Checking no unchecks all other ones:
    RadioButtonNone.Checked := true;
    if Selected then
    begin
      if item.SubItems.count > 1 then MemoSentence.text := item.SubItems[1];
      if item.SubItems.count > 2 then MemoTranslation.text := item.SubItems[2];
      if (item.SubItems.count > 3) and TryStrToInt(item.SubItems[3], rating) then
      begin
        RadioButton1.Checked := rating = 1;
        RadioButton2.Checked := rating = 2;
        RadioButton3.Checked := rating = 3;
        RadioButton4.Checked := rating = 4;
        RadioButton5.Checked := rating = 5;
      end;
    end
    else
    begin
      MemoSentence.text := '';
      MemoTranslation.text := '';
    end;

    if iEditTimes and iBass.Active and not item.Checked then
    begin
      SampleAssignPeriod(item, selected, iBass.GetPositionSeconds);
    end;

  finally
    iIsUpdating := false;
  end;
end;

procedure TFrameReadSentences.MemoTranslationChange(Sender: TObject);
var item : TListItem;
begin
  item := ListViewSentences.Selected;
  if not iIsUpdating
  and (item <> nil)
  and (item.SubItems.Count > 2)
  then
  begin
    item.SubItems[2] := MemoTranslation.Text;
    SetSentencesDirty(true);
  end;
end;

procedure TFrameReadSentences.MemoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ButtonSplit.Enabled := IsSplitEnabled;
end;

procedure TFrameReadSentences.Panel3Resize(Sender: TObject);
begin
  MemoSentence.Height := 3 * Panel3.Height div 5;
end;

procedure TFrameReadSentences.PanelTopResize(Sender: TObject);
var w : integer;
begin
  w := PanelTopCenter.Width;
  TrackBarAllSound.Width := w - TrackBarAllSound.Left - 25;
  ProgressBarSentence.Width := w - ProgressBarSentence.Left - 25;
  w := (ListViewSentences.Width - (ListViewSentences.Columns[0].Width + ListViewSentences.Columns[1].Width)) - 10;
  ListViewSentences.Columns[2].Width := w div 2;
  ListViewSentences.Columns[3].Width := w div 2;
end;

procedure TFrameReadSentences.ButtonPlayClick(Sender: TObject);
begin
  iEditTimes := false;
  case RadioGroupPlay.ItemIndex of
    0 : PlayFromCurrentSentence;
    1 : begin
          iEditTimes := true;
          PlayFromCurrentSentence;
        end;
    2 : begin
          ExtraLog('Play repeating');
          iRepeatingCount := 0;
          PlaySentence(ListViewSentences.Selected, TimerRepeatAndNext, KSepRepeating);
        end;
    3 : begin
          iRepeatingCount := 0;
          PlaySentence(ListViewSentences.Selected, TimerRepeatAndNext, KSepOne);
        end;
  end;
end;

procedure TFrameReadSentences.StopPlaying;
begin
  if iBass <> nil then iBass.Stop;
  TimerRepeatAndNext.Enabled := false;
end;

procedure TFrameReadSentences.ButtonStopClick(Sender: TObject);
begin
  StopPlaying;
end;

procedure TFrameReadSentences.RadioButtonNoneChange(Sender: TObject);
var item : TListItem;
begin
  item := ListViewSentences.Selected;
  if not iIsUpdating
  and (item <> nil)
  and (ListViewSentences.SelCount = 1)
  then
  begin
    AddMinimal(item, 4);
    item.SubItems[3] := '';
    SetTimesDirty(true);
  end;
end;

procedure TFrameReadSentences.RadioButtonRatingChange(Sender: TObject);
var item : TListItem;
begin
  item := ListViewSentences.Selected;
  if not iIsUpdating
  and (item <> nil)
  and (ListViewSentences.SelCount = 1)
  and (Sender is TRadioButton)
  then
  begin
    AddMinimal(item, 4);
    item.SubItems[3] := inttostr((sender as TRadioButton).Tag);
    SetTimesDirty(true);
  end;
end;

procedure TFrameReadSentences.Shape1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RadioButton4.Checked := true;
end;

function log10(v : double): double;
begin
  result := ln(v) / ln(10);
end;

procedure TFrameReadSentences.TimerLevelTimer(Sender: TObject);

  function ToDecibelAndScale(level : double) : double;
  var db : double;
  begin
    // Convert it to decibels
    if level > 0 then db := 20.0 * log10(level) else db := -100;
    if db < -100 then db := -100;
    // Scale between 0 (silence) and ~1.0 (loud!)
    result := (db + 100.0) / 100.0;
  end;

var level, periodSeconds : double;
begin
  level := 0;
  if not iBass.Active then
  begin
    level := 0;
  end
  else
  begin
    // Get over a period between timer interval
    periodSeconds := TimerLevel.Interval / 1000.0;
    level := iBass.CurrentLevel(periodSeconds);
  end;
  ProgressBarLevel.Position := trunc(ToDecibelAndScale(level) * ProgressBarLevel.Max);
end;

procedure TFrameReadSentences.AssignPeriod(item : TListItem; isBegin : boolean; pos : double);
var timeStr : string;
begin
  assert(item <> nil);
  timeStr := format('%.3f', [pos]);

  // Set begin-mark
  iIsUpdating := true;
  try
    if isBegin then
    begin
      item.Caption := timeStr;
    end
    else
    begin
      // Set end-mark and mark it as finished
      item.SubItems[0] := timeStr;
      item.Checked := true;
    end;
  finally
    iIsUpdating := false;
  end;
  SetTimesDirty(true);
end;

procedure TFrameReadSentences.SampleAssignPeriod(item : TListItem; isBegin : boolean; pos : double);
var levels : TArrayOfLevel;
  details : TDetailedLevelInfo;
  optimalPos : double;
begin
  // Get levels around this position (take 5 seconds because pauzes might be long)
  levels := iBass.GetSamples(pos - 2.5, pos + 2.5);
  if GetOptimalTime(details, levels, pos) then
  begin
    optimalPos := (details.posZeroBegin + details.posZeroEnd) / 2.0;

    AssignPeriod(item, isBegin, optimalPos);
    log(format('Optimum found for %.3f, assign %.3f to %d'
    + ' (%.3f - %.3f - %.3f) (#%d - level: %.2f .. %.2f)',
      [pos, optimalPos, item.index,
      details.posZeroBegin, details.posZeroEnd, details.posZeroEnd - details.posZeroBegin,
      length(levels), levels[low(levels)].level, levels[high(levels)].level]));
  end
  else
  begin
    log(format('No optimum, assign %.3f to %d (#%d)',
      [pos, item.index, length(levels)]));
    AssignPeriod(item, isBegin, pos);
  end;
end;

procedure TFrameReadSentences.TimerAllSoundTimer(Sender: TObject);
var p : double;
begin
  p := iBass.GetPositionSeconds;
  TrackBarAllSound.position := trunc(p * 1000.0);
  ProgressBarSentence.position := trunc(1000.0 * (p - iPlaybackBeginPosition));
  ExtraLog(format('Now at %.3f, %d, max=%d, active=%d', [p - iPlaybackBeginPosition,
  ProgressBarSentence.position, ProgressBarSentence.max, ord(iBass.Active)]));

  ButtonPlay.Enabled := not iBass.Active;
  ButtonGoToCurrent.Enabled := not iBass.Active;
  RadioGroupPlay.Enabled := not iBass.Active;
  ButtonStop.Enabled := iBass.Active;
end;

procedure TFrameReadSentences.TimerRepeatAndNextTimer(Sender: TObject);
var it : TListItem;
  maxCount : integer;
  add : integer;

begin
  iBass.Stop;
  TimerRepeatAndNext.Enabled := false;

  inc(iRepeatingCount);

  add := KSepOne;
  maxCount := 1;
  if RadioGroupPlay.ItemIndex = 2 then
  begin
    TryStrToInt(EditRepeating.Caption, maxCount);
    add := KSepRepeating;
    ExtraLog(format('Continue, max=%d, now=%d, add=%d', [maxCount, iRepeatingCount, add]));
  end;

  if iRepeatingCount < maxCount then
  begin
    // Play the same sentence again
    ExtraLog('play again');
    PlaySentence(ListViewSentences.Selected, TimerRepeatAndNext, add);
  end
  else
  begin
    // Play next sentence (from selection, users might change it)
    ExtraLog('play next');
    iRepeatingCount := 0;
    it := ListViewSentences.selected;
    if (it <> nil) and (it.Index + 1 < ListViewSentences.Items.Count) then
    begin
      // Unselect current one
      it.Selected := false;

      // Select next one
      it := ListViewSentences.Items[it.Index + 1];
      it.Selected := true;
      it.Focused := true;
      it.MakeVisible(false);
      PlaySentence(it, TimerRepeatAndNext, add);
    end;
  end;
end;

procedure TFrameReadSentences.PlaySentence(item : TListItem; timer : TTimer;
    additionalMs : integer);
var timeOfSentenceMs : longint;
  pos1, pos2 : double;
begin
  if iRepeatingCount <= high(iRepeatSettings) then
  begin
    MemoSentence.Visible := iRepeatSettings[iRepeatingCount].showTarget;
    MemoTranslation.Visible := iRepeatSettings[iRepeatingCount].showTranslation;
  end;


  iBass.Stop;
  timer.Enabled := false;
  iPlaybackBeginPosition := 0;
  iPlaybackEndPosition := 0;

  if item = nil then
  begin
    exit;
  end;

  ExtraLog(format('PlaySentence %d, int=%d, add=%d', [item.Index, timer.Interval, additionalMs]));

  if TryStrToFloat(item.Caption, pos1)
  and TryStrToFloat(item.SubItems[0], pos2)
  then
  begin
    iBass.PlaySelection(pos1, pos2);
    timeOfSentenceMs := trunc(1000.0 * (pos2 - pos1));
    ExtraLog(format('sel %.3f %.3f time %u', [pos1, pos2, timeOfSentenceMs]));
    if timeOfSentenceMs > 0 then
    begin
      ExtraLog(format('sel %.3f %.3f time %.3f', [pos1, pos2, timeOfSentenceMs / 1000.0]));
      iPlaybackBeginPosition := pos1;
      iPlaybackEndPosition := pos2;
      timer.Interval := timeOfSentenceMs + additionalMs;
      timer.Enabled := true;

      ProgressBarSentence.Max := timeOfSentenceMs;
      ProgressBarSentence.Position := 0;

      // This doesn't look good in long files.
      //TrackBarAllSound.SelStart := trunc(1000 * pos1);
      //TrackBarAllSound.SelEnd := trunc(1000 * pos2);
      //TrackBarAllSound.ShowSelRange := true;
    end;
  end;
end;

procedure TFrameReadSentences.PlayFromCurrentSentence;
var item : TListItem;
  p : double;
begin
  iBass.Stop;
  item := ListViewSentences.Selected;
  if (item = nil) and (ListViewSentences.Items.Count > 0) then
  begin
    item := ListViewSentences.Items[0];
  end;
  if item <> nil then
  begin
    if TryStrToFloat(item.Caption, p) then
    begin
      iBass.PlayFromPosition(p);
    end
    else
    begin
      iBass.PlayFromStart;
    end;
  end;
end;

procedure TFrameReadSentences.IndicateDirty(colIndex : integer; v : boolean; const title : string);
begin
  if v then ListViewSentences.Columns[colIndex].Caption := title + ' *'
  else ListViewSentences.Columns[colIndex].Caption := title;
end;

procedure TFrameReadSentences.SetTimesDirty(v : boolean);
begin
  iTimesDirty := v;
  IndicateDirty(0, v, 'Begin');
  IndicateDirty(1, v, 'End');
end;

procedure TFrameReadSentences.SetSentencesDirty(v : boolean);
begin
  iSentencesDirty := v;
  IndicateDirty(3, v, 'Translation');
end;

end.

// Icons by:
// https://freeicons.io/profile/3 (Icon King) [play, stop]
// https://freeicons.io/profile/3277 (Gayrat Muminov) [align middle=go to edit]

