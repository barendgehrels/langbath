// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This frame contains a listview with sentences (usually of a book) and translations,
// and their timings, and an optional rating.
//   - it can read out loud the sentences (called: the target)
//   - it can repeat these sentences
//   - during repeated replay target language/translation can be set to visible/invisible
//   - the user can edit timings (begin/end per sentence)
//   - the user can graphically edit timings
//   - the user can rate sentences
//   - the user can merge and split sentences (and their translations and timings)
//   - the user can search sentences or timings
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
    ButtonPlay1: TSpeedButton;
    ButtonSearch: TSpeedButton;
    ButtonGoToCurrent: TSpeedButton;
    ButtonPlay: TSpeedButton;
    ButtonStop: TSpeedButton;
    EditSearch: TEdit;
    EditRepeating: TEdit;
    ListViewSentences: TListView;
    MemoSentence: TMemo;
    MemoTranslation: TMemo;
    PanelSentence: TPanel;
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
    ButtonWaveForm: TSpeedButton;
    ButtonRepeatSettings: TSpeedButton;
    ButtonMerge: TSpeedButton;
    ButtonSplit: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    TimerRepeat: TTimer;
    TimerState: TTimer;
    TrackBarAllSound: TTrackBar;
    procedure ButtonRepeatSettingsClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonSplitClick(Sender: TObject);
    procedure ButtonGoToEditPlaceClick(Sender: TObject);
    procedure ButtonMergeClick(Sender: TObject);
    procedure ButtonWaveFormClick(Sender: TObject);
    procedure ListViewSentencesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MemoTranslationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoTranslationChange(Sender: TObject);
    procedure MemoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerRepeatTimer(Sender: TObject);
    procedure TimerStateOnTimer(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
    procedure PanelTopResize(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure RadioButtonNoneChange(Sender: TObject);
    procedure RadioButtonRatingChange(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

    iEditTimes : boolean;
    iIsUpdating : boolean;

    iBass : TLbBass;
    iTimesDirty : boolean;
    iSentencesDirty : boolean;

    iRepeatSettings : TArrayOfRepeatSettings;

    // Index of the sentence being repeated
    iRepeatSentenceIndex : integer;

    // The number of repetitions
    iRepeatCount : integer;

    // The current repetition
    iRepeatIndex : integer;


    iRepeatBeginSeconds : double;
    iRepeatEndSeconds : double;

    procedure StopPlaying;
    function IsRepeating : boolean;
    function GetRepeatSettings : TRepeatSettings;
    procedure ShowMemos;
    procedure PlaySentence(item : TListItem);
    procedure PlayFromCurrentSentence;

    function IsSplitEnabled : boolean;
    procedure SelectItem(item : TListItem; selected : Boolean);

    procedure AssignPeriod(item : TListItem; isBegin : boolean; pos : double);
    procedure SampleAssignPeriod(item : TListItem; isBegin : boolean; pos : double);
    procedure IndicateDirty(colIndex : integer; v : boolean; const title : string);
    procedure SetTimesDirty(v : boolean);
    procedure SetSentencesDirty(v : boolean);
    procedure SelectAndShowListItem(item : TListItem);

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
  KRepeatSeparationMilliseconds : integer = 500;
  KPlaySeparationMilliseconds : integer = 2000;

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

procedure TFrameReadSentences.SelectAndShowListItem(item : TListItem);
var sel : TListItem;
  hasFocus : boolean;
begin
  hasFocus := ListViewSentences.Focused;

  // If it is already selected, no action is necessary
  sel := ListViewSentences.Selected;
  if (sel <> nil) and (sel.index = item.index) and (ListViewSentences.SelCount = 1) then exit;

  // Select and show the item (avoiding flicker)
  if sel <> nil then sel.Selected := false;
  if ListViewSentences.SelCount > 0 then ListViewSentences.ClearSelection;
  item.Selected := true;
  item.Focused := true;
  item.MakeVisible(false);

  // The list can loose the focus, refocus
  if hasFocus then ListViewSentences.SetFocus;
end;

procedure TFrameReadSentences.ButtonGoToEditPlaceClick(Sender: TObject);
var i : integer;
  item : TListItem;
begin
  ListViewSentences.ItemIndex := -1;
  // Deliberately skip the first item
  for i := 1 to ListViewSentences.Items.Count - 1 do
  begin
    item := ListViewSentences.Items[i];
    if (item.SubItems.Count < 2) or (item.Caption = '') or (item.SubItems[0] = '') then
    begin
      SelectAndShowListItem(item);
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

procedure TFrameReadSentences.ButtonRepeatSettingsClick(Sender: TObject);
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

procedure TFrameReadSentences.ButtonSearchClick(Sender: TObject);

  // Searches in target or in translation. All searches are case insensitive.
  // User can add space to, for example, search for " air" and not find "hair"
  // (though this will not be if "air" is at the start).
  // Checkboxes for whole-word-only or case-sensitive might be added later.
  function Search(const s : string; start, finish : integer) : boolean;
  var i, j : integer;
    item : TListItem;
  begin
    result := false;
    for i := start to finish do
    begin
      item := ListViewSentences.Items[i];
      for j := 1 to 2 do
      begin
        if j < item.SubItems.count then
        begin
          if UTF8Pos(s, UTF8LowerCase(item.SubItems[j])) >= 1 then
          begin
            SelectAndShowListItem(item);
            result := true;
            exit;
          end;
        end;
      end;
    end;
  end;

  // Searches timing (between begin/end). It is done sequentiall and not binary,
  // because there is no hard guarantee that all timings are ordered
  function Search(timing : double; start, finish : integer) : boolean;
  var i : integer;
    item : TListItem;
    t0, t1 : double;
  begin
    result := false;
    for i := start to finish do
    begin
      item := ListViewSentences.Items[i];
      if (item.SubItems.count >= 1)
      and TryStrToFloat(item.caption, t0)
      and TryStrToFloat(item.SubItems[0], t1)
      and (t0 <= timing)
      and (timing <= t1)
      then
      begin
        SelectAndShowListItem(item);
        result := true;
        exit;
      end;
    end;
  end;

var item : TListItem;
  start : integer;
  s : string;
  timing : double;

begin
  s := UTF8LowerCase(EditSearch.text);
  if trim(s) = '' then exit;

  item := ListViewSentences.Selected;
  if item <> nil then start := item.Index;

  if TryStrToFloat(s, timing) then
  begin
    // Try to search from next of current selection, and stop at the first hit
    if Search(timing, start + 1, ListViewSentences.Items.Count - 1) then exit;

    // Not found, search to just before current selection
    if start > 0 then Search(timing, 0, start - 1);
  end
  else
  begin
    if Search(s, start + 1, ListViewSentences.Items.Count - 1) then exit;
    if start > 0 then Search(s, 0, start - 1);
  end;
end;

procedure TFrameReadSentences.TimerRepeatTimer(Sender: TObject);
var item : TListItem;
  nextIndex : integer;
begin
  TimerRepeat.Enabled := false;

  item := ListViewSentences.selected;
  if item = nil then
  begin
    // At unselect, repeating stops
    iRepeatSentenceIndex := -1;
    StopPlaying;
    exit;
  end;

  if item.Index <> iRepeatSentenceIndex then
  begin
    // At selecting another sentence, stop looping current, and play indicated
    PlaySentence(item);
    exit;
  end;

  inc(iRepeatIndex);
  if iRepeatIndex < iRepeatCount then
  begin
    // Play the same sentence again
    if GetRepeatSettings.playAudio then iBass.PlaySelection(iRepeatBeginSeconds, iRepeatEndSeconds);
    TimerRepeat.Enabled := true;
    ProgressBarSentence.Position := iRepeatIndex * 1000;
    ExtraLog(format('loop index=%d', [iRepeatIndex]));
  end
  else
  begin
    // Play next sentence
    if item.Index = iRepeatSentenceIndex then nextIndex := item.Index + 1
    else nextIndex := item.Index;

    if nextIndex < ListViewSentences.Items.Count then
    begin
      ExtraLog(format('next idx=%d', [nextIndex]));

      // Play next one
      item := ListViewSentences.Items[nextIndex];
      SelectAndShowListItem(item);
      PlaySentence(item);
    end
    else
    begin
      StopPlaying;
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

    TimerState.Enabled := false;
    try
      FormWaveForm.ShowModal;
      if FormWaveForm.ModalResult = mrOK then
      begin
        AssignPeriod(ListViewSentences.Items[first.Index], false, FormWaveForm.GetPosition1);
        AssignPeriod(ListViewSentences.Items[second.Index], true, FormWaveForm.GetPosition2);
      end;

    finally
      TimerState.Enabled := true;
    end;
  end;
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

procedure TFrameReadSentences.TimerStateOnTimer(Sender: TObject);
  function log10(v : double): double;
  begin
    result := ln(v) / ln(10);
  end;

  function ToDecibelAndScale(level : double) : double;
  var db : double;
  begin
    // Convert it to decibels
    if level > 0 then db := 20.0 * log10(level) else db := -100;
    if db < -100 then db := -100;
    // Scale between 0 (silence) and ~1.0 (loud!)
    result := (db + 100.0) / 100.0;
  end;

var pos, loopFraction, level : double;
  playing, bassActive : boolean;
begin
  bassActive := iBass.GetReport(pos, loopFraction, level);
  if bassActive then
  begin
    //log(format('TIMER - got report %.3f %.3f', [pos, level]));
    TrackBarAllSound.position := trunc(pos * 1000.0);
    ProgressBarLevel.Position := trunc(ToDecibelAndScale(level) * ProgressBarLevel.Max);

    if RadioGroupPlay.ItemIndex >= 2 then
    begin
      ProgressBarSentence.Position := trunc((iRepeatIndex + loopFraction) * 1000.0);
    end;
  end
  else
  begin
    ProgressBarLevel.Position := 0;
  end;

  playing := bassActive or TimerRepeat.Enabled;
  ButtonStop.Enabled := playing;

  ButtonPlay.Enabled := not playing;
  ButtonGoToCurrent.Enabled := not playing;
  RadioGroupPlay.Enabled := not playing;

  ShowMemos;
end;

procedure TFrameReadSentences.Panel3Resize(Sender: TObject);
begin
  PanelSentence.Height := 3 * Panel3.Height div 5;
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
    2 : if TryStrToInt(EditRepeating.Text, iRepeatCount) then
        begin
          ExtraLog('Play repeating');

          ProgressBarSentence.Max := iRepeatCount * 1000;
          ProgressBarSentence.Position := 0;

          PlaySentence(ListViewSentences.Selected);
        end;
    3 : begin
          iRepeatCount := 0;
          ProgressBarSentence.Max := 1000;
          ProgressBarSentence.Position := 0;
          PlaySentence(ListViewSentences.Selected);
        end;
  end;
  ListViewSentences.SetFocus;
end;

procedure TFrameReadSentences.StopPlaying;
begin
  if iBass <> nil then iBass.Stop;
  ProgressBarLevel.Position := 0;
  ProgressBarSentence.Position := 0;
  TimerRepeat.Enabled := false;
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
  levels := iBass.GetLevels(pos - 2.5, pos + 2.5);
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

function TFrameReadSentences.IsRepeating : boolean;
begin
  result := RadioGroupPlay.ItemIndex = 2;
end;

function TFrameReadSentences.GetRepeatSettings : TRepeatSettings;
begin
  if IsRepeating
  and (iRepeatIndex >= low(iRepeatSettings))
  and (iRepeatIndex <= high(iRepeatSettings))
  then
  begin
    result := iRepeatSettings[iRepeatIndex];
  end
  else
  begin
    // If settings are out of range, enable all
    result.playAudio := true;
    result.showTarget := true;
    result.showTranslation := true;
  end;
end;

procedure TFrameReadSentences.ShowMemos;
var showMemo : boolean;
  settings : TRepeatSettings;
begin
  // Memo's are only hidden in repeating mode. In all other cases they are shown.
  showMemo := not IsRepeating or not TimerRepeat.Enabled;

  settings := GetRepeatSettings;

  MemoSentence.Visible := showMemo or settings.showTarget;
  MemoTranslation.Visible := showMemo or settings.showTranslation;

  log(format('show index=%d target=%d trans=%d', [iRepeatIndex,
    ord(MemoSentence.Visible), ord(MemoTranslation.Visible)]));
end;

procedure TFrameReadSentences.PlaySentence(item : TListItem);
var pos1, pos2 : double;
  sep : integer;
begin
  iRepeatIndex := 0;
  iRepeatSentenceIndex := -1;
  ProgressBarSentence.Position := 0;

  if (item <> nil)
  and TryStrToFloat(item.Caption, pos1)
  and TryStrToFloat(item.SubItems[0], pos2)
  then
  begin
    iRepeatBeginSeconds := pos1;
    iRepeatEndSeconds := pos2;

    if IsRepeating then sep := KRepeatSeparationMilliseconds else sep := KPlaySeparationMilliseconds;

    TimerRepeat.Interval := trunc(1000 * (pos2 - pos1)) + sep;
    TimerRepeat.Enabled := true;

    if GetRepeatSettings.playAudio then iBass.PlaySelection(pos1, pos2);

    iRepeatSentenceIndex := item.Index;
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
// https://freeicons.io/user-interface-and-electronics/arrow-down-download-line-items-interface-ui-c-a-b-fa-icon-815# (go to edit)
// https://freeicons.io/documents-icons/icon-search-icon-7380# (search)
// https://freeicons.io/media-icons/sound-wave-icon-37695# (sound wave)
// https://freeicons.io/user-interface-4/interface-elements-ui-check-box-checkbox-todo-list-icon-43075# (repeat settings)
// https://freeicons.io/black-arrow-icons-set/combine-merge-arrow-road-sign-black-icon-direction-way-path-indication-signal-mark-icon-53173# (merge)
// https://freeicons.io/black-arrow-icons-set/arrows-down-down-sign-sign-black-icon-direction-way-path-indication-signal-mark-icon-53160# (split - rotated)

// https://freeicons.io/business-and-online-icons/chevron-down-icon-icon#
// https://freeicons.io/business-and-online-icons/chevron-up-icon-icon#

