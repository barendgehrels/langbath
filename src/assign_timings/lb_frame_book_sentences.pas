// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This frame contains a listview with sentences (usually of a book) and translations,
// and their timings, and an optional rating.
//   - it can read out loud the sentences (called: the target)
//   - it can repeat these sentences
//   - the user can edit timings (begin/end per sentence)
//   - the user can rate sentences
//   - the user can merge and split sentences (and their translations and timings)
//   - the user can search sentences (TODO)
//   - the user can copy/paste the target to facilitate translations (TODO)
//   - the user can graphically edit timings (TODO)

unit lb_frame_book_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, lb_bass, lb_time_optimizer;

type

  { TFrameReadSentences }

  TFrameReadSentences = class(TFrame)
    ButtonGoToCurrent: TSpeedButton;
    ButtonMerge: TButton;
    ButtonPlay: TSpeedButton;
    ButtonSplit: TButton;
    ButtonStop: TSpeedButton;
    EditRepeating: TEdit;
    LabelSentencesDirty: TLabel;
    LabelTimesDirty: TLabel;
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
    RadioButtonHidden: TRadioButton;
    RadioGroupPlay: TRadioGroup;
    Splitter1: TSplitter;
    TimerAssignEnd: TTimer;
    TimerAssignBegin: TTimer;
    TimerLevel: TTimer;
    TimerAllSound: TTimer;
    TimerRepeatAndNext: TTimer;
    TrackBarAllSound: TTrackBar;
    procedure ButtonSplitClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonGoToEditPlaceClick(Sender: TObject);
    procedure ButtonMergeClick(Sender: TObject);
    procedure ListViewSentencesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MemoTranslationChange(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
    procedure PanelTopResize(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure RadioButtonRatingChange(Sender: TObject);
    procedure TimerAssignEndTimer(Sender: TObject);
    procedure TimerLevelTimer(Sender: TObject);
    procedure TimerAssignBeginTimer(Sender: TObject);
    procedure TimerAllSoundTimer(Sender: TObject);
    procedure TimerRepeatAndNextTimer(Sender: TObject);
  private

    iEditTimes : boolean;
    iLastSeparationBeginPosition : double;
    iLastSeparationEndPosition : double;
    iLastSeparationBeginIndex : integer;
    iLastSeparationEndIndex : integer;
    iLastLevels : TTimedLevelArray;

    iRepeatingCount : integer;
    iPlaybackBeginPosition : double;
    iPlaybackEndPosition : double;

    iIsUpdating : boolean;

    iBass : TLbBass;
    iTimesDirty : boolean;
    iSentencesDirty : boolean;


    procedure StopPlaying;
    procedure PlaySentence(item : TListItem; timer : TTimer; additionalMs : integer);
    procedure PlayFromCurrentSentence;

    procedure SelectItem(item : TListItem; selected : Boolean);

    procedure AssignPeriod(item : TListItem; isBegin : boolean; pos : double);
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

uses LazUtf8, lb_ui_lib;

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
  LabelTimesDirty.Caption := '';
  LabelSentencesDirty.Caption := '';
end;

destructor TFrameReadSentences.Destroy;
begin
  iBass.Free;
  inherited Destroy;
end;

procedure TFrameReadSentences.ReadSound(const filename : string);
begin
  iLastSeparationBeginIndex := -1;
  iLastSeparationEndIndex := -1;

  StopPlaying;
  iBass.Free;
  iBass := TLbBass.Create(filename);
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

procedure TFrameReadSentences.ButtonSplitClick(Sender: TObject);
const KMinLength : integer = 1;
var len1, len2 : integer;
  item, newItem : TListItem;
begin
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

  // Insert a new item, and fill it with the selection
  if MemoSentence.SelStart > 1 then
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
begin

end;

procedure TFrameReadSentences.ButtonMergeClick(Sender: TObject);
var item, first, last : TListItem;
  i, index, count : integer;

begin
  index := 0;
  count := 0;
  item := ListViewSentences.Selected;
  first := item;
  while item <> nil do
  begin
    if (index = 0) or (item.Index = index + 1) then
    begin
      inc(count);
    end;
    index := item.Index;
    last := item;
    item := ListViewSentences.GetNextItem(item, sdBelow, [lisSelected]);
  end;
  if count = ListViewSentences.SelCount then
  begin
    // Merge times to one period
    first.SubItems[0] := last.subItems[0];
    // Merge texts by concatenation
    for i := 1 to first.SubItems.Count - 1 do
    begin
      first.SubItems[i] := first.SubItems[i] + ' ' + last.SubItems[i];
    end;
    // Empty rating, if any
    if first.SubItems.Count > 3 then first.SubItems[3] := '';

    // Delete merged item
    ListViewSentences.Items.Delete(last.Index);

    // Reselect it to show its new contents
    SelectItem(first, true);

    SetTimesDirty(true);
    SetSentencesDirty(true);
  end;

end;

procedure TFrameReadSentences.ListViewSentencesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SelectItem(item, selected);
end;

procedure TFrameReadSentences.SelectItem(item : TListItem; selected : Boolean);
var rating : integer;
begin
  ButtonMerge.Enabled := ListViewSentences.SelCount = 2;

  if item = nil then
  begin
    exit;
  end;

  iIsUpdating := true;
  try

    // Uncheck the other ones:
    RadioButtonHidden.Checked := true;
    RadioButton1.Checked := false;
    RadioButton2.Checked := false;
    RadioButton3.Checked := false;
    RadioButton4.Checked := false;
    RadioButton5.Checked := false;
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
      // Start one of the two timers.
      // It is assigned a tad later to determine the optimal place using level measurements.
      if Selected and (iLastSeparationBeginIndex = -1) then
      begin
        iLastSeparationBeginPosition := iBass.GetPositionSeconds;
        iLastSeparationBeginIndex := item.Index;
        TimerAssignBegin.Enabled := true;
      end
      else if not Selected and (iLastSeparationEndIndex = -1) then
      begin
        iLastSeparationEndPosition := iBass.GetPositionSeconds;
        iLastSeparationEndIndex := item.Index;
        TimerAssignEnd.Enabled := true;
      end;
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

  // Workaround for Linux: there is a visible radio button, but hidden behind a progress bar
  // because otherwise non of the other ones can be UNchecked
  RadioButtonHidden.Left := ProgressBarSentence.Left;
  RadioButtonHidden.Top := ProgressBarSentence.Top;
  {$ifdef WINDOWS}
  RadioButtonHidden.Visible := false;
  {$ENDIF}
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

function log10(v : double): double;
begin
  result := ln(v) / ln(10);
end;

procedure TFrameReadSentences.TimerLevelTimer(Sender: TObject);
var level, a, periodSeconds : double;
  cyclicCount : integer;
begin
  a := 0;
  if not iBass.Active then
  begin
    level := 0;
    iLastLevels := [];
  end
  else
  begin
    // Get over a period between timer interval
    periodSeconds := TimerLevel.Interval / 1000.0;
    // Store measurements in period of ~ 0.6 second
    cyclicCount := 1 + (600 div TimerLevel.Interval);

    level := iBass.CurrentLevel(periodSeconds);
    // Convert it to decibels
    if level > 0 then a := 20.0 * log10(level) else a := -100;
    if a < -100 then a := -100;
    // Scale between 0 (silence) and ~1.0 (loud!)
    a := (a + 100.0) / 100.0;
    // Add it to cyclic array
    AddTimedLevel(iLastLevels, a, iBass.GetPositionSeconds, cyclicCount);

    //ExtraLog(format('temp %.5f %.5f %.5f', [iBass.GetPositionSeconds, level, a]));
  end;
  ProgressBarLevel.Position := trunc(a * ProgressBarLevel.Max);
end;

procedure TFrameReadSentences.AssignPeriod(item : TListItem; isBegin : boolean; pos : double);
var s, timeStr : string;
  t : double;
begin
  // DEBUG
  if item.SubItems.Count > 2 then
  begin
    s := TimedLevelToString(iLastLevels, pos);
    if isBegin then
    begin
      ExtraLog(format('BEGIN index %d label %s at %s', [item.Index, s, item.SubItems[2]]));
    end
    else
    begin
      ExtraLog(format('END index %d label %s at %s', [item.Index, s, item.SubItems[2]]));
    end;
  end;
  // END DEBUG

  t := OptimalTime(iLastLevels, pos);

  timeStr := format('%.3f', [t]);

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


  //if iLastSeparationBeginPosition > 0 then
  //begin
  //  previous := ListViewSentences.Items[iLastSeparationBeginIndex - 1];
  //  previous.SubItems[0] := timeStr;
  //  previous.Checked := true;
  //end;
end;

procedure TFrameReadSentences.TimerAssignBeginTimer(Sender: TObject);
begin
  TimerAssignBegin.Enabled := false;
  if (iLastSeparationBeginIndex >= 0)
  and (iLastSeparationBeginIndex < ListViewSentences.Items.Count) then
  begin
    AssignPeriod(ListViewSentences.Items[iLastSeparationBeginIndex], true, iLastSeparationBeginPosition);
  end;
  iLastSeparationBeginIndex := -1;
end;

procedure TFrameReadSentences.TimerAssignEndTimer(Sender: TObject);
begin
  TimerAssignEnd.Enabled := false;
  if (iLastSeparationEndIndex >= 0)
  and (iLastSeparationEndIndex < ListViewSentences.Items.Count) then
  begin
    AssignPeriod(ListViewSentences.Items[iLastSeparationEndIndex], false, iLastSeparationEndPosition);
  end;
  iLastSeparationEndIndex := -1;
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

procedure TFrameReadSentences.SetTimesDirty(v : boolean);
begin
  iTimesDirty := v;
  if iTimesDirty then LabelTimesDirty.Caption := '*' else LabelTimesDirty.Caption := '';
end;

procedure TFrameReadSentences.SetSentencesDirty(v : boolean);
begin
  iSentencesDirty := v;
  if iSentencesDirty then LabelSentencesDirty.Caption := '*' else LabelSentencesDirty.Caption := '';
end;

end.

// Icons by:
// https://freeicons.io/profile/3 (Icon King) [play, stop]
// https://freeicons.io/profile/3277 (Gayrat Muminov) [align middle=go to edit]

