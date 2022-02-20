unit lb_frame_play_sentence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  lb_bass;

type

  //TFramePlaySentenceActorInterface = class
  //  procedure PlaySentence(const phrase : TPhrase; phase : integer); virtual; abstract;
  //end;

  { TFramePlayOneSentence }

  TFramePlayOneSentence = class(TFrame)
    ButtonPlayNextSentence: TButton;
    ButtonPlaySentence: TButton;
    ButtonStopPlaying: TButton;
    CheckBoxRepeat: TCheckBox;
    LabelWarning: TLabel;
    LabelRepeat: TLabel;
    LabelDistance: TLabel;
    LabelLength: TLabel;
    ListSentences: TListView;
    MemoSentence: TMemo;
    MemoTranslation: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelEmpty: TPanel;
    Splitter1: TSplitter;
    TimerRepeat: TTimer;
    procedure ButtonPlayNextSentenceClick(Sender: TObject);
    procedure ButtonPlaySentenceClick(Sender: TObject);
    procedure ButtonStopPlayingClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure LabelAudioLengthClick(Sender: TObject);
    procedure ListSentencesResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure TimerRepeatTimer(Sender: TObject);
  private

    //iPhrases : TArrayOfPhrase;
    iCurrentPhraseIndex : integer;
    //iActor : TFramePlaySentenceActorInterface;

    iHide : boolean;

    iBass : array[0..5] of TLbBass;
    iBassCurrent : integer;
    iTimeBegin, iTimeEnd : double;
    iRepeatIndex : integer;


    procedure ShowPhrases;
    procedure Add(index : integer);
    procedure PlayCurrentPhrase;
    procedure DownloadAndPlay;

    function Valid : boolean;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    //procedure SetActor(a : TFramePlaySentenceActorInterface);

    //function ShowSentences(const a : TArrayOfStudyWordForm; hideSearchString : boolean) : boolean;
    //procedure SetPhrases(const phrases : TArrayOfPhrase);
    //
    procedure ShowContents(showList, showSentence, showTranslation : boolean);
    procedure HideTargetWord(permanentId : integer);

    function PlayNextPhrase : integer;
    procedure StopAll;
    procedure Clear;


  end;

implementation

{$R *.lfm}

{ TFramePlayOneSentence }

uses LazUTF8, fphttpclient,
  lb_lib, lb_lib_string, lb_const;

constructor TFramePlayOneSentence.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFramePlayOneSentence.Destroy;
var i : integer;
begin
  for i := low(iBass) to high(iBass) do iBass[i].Free;
  inherited Destroy;
end;


procedure TFramePlayOneSentence.ButtonPlaySentenceClick(Sender: TObject);
begin
  PlayCurrentPhrase;
end;

procedure TFramePlayOneSentence.ButtonStopPlayingClick(Sender: TObject);
begin
  StopAll;
end;

procedure TFramePlayOneSentence.FrameResize(Sender: TObject);
var w : integer;
begin
  w := self.width div 2;
  ListSentences.width := w;
end;

procedure TFramePlayOneSentence.LabelAudioLengthClick(Sender: TObject);
begin

end;

procedure TFramePlayOneSentence.ListSentencesResize(Sender: TObject);
var w : integer;
begin
  w := ListSentences.Width div 11;
  ListSentences.Columns[0].Width := 3 * w;
  ListSentences.Columns[1].Width := 3 * w;
  ListSentences.Columns[2].Width := 2 * w;
  ListSentences.Columns[3].Width := 2 * w;
end;

procedure TFramePlayOneSentence.PaintBox1Paint(Sender: TObject);
begin
end;

procedure TFramePlayOneSentence.Panel2Resize(Sender: TObject);
begin
  MemoSentence.Height := 3 * Panel2.Height div 5;
end;

procedure TFramePlayOneSentence.TimerRepeatTimer(Sender: TObject);
var phrase : TPhrase;
  sql : TSql;
begin
  TimerRepeat.Enabled := false;
  if Valid then
  begin
    phrase := iPhrases[iCurrentPhraseIndex];

    if (phrase.iWord.permanentId > 0) and (phrase.permanentSentenceId > 0) then
    begin
      // Store it
      sql := SqlCreateInsertQuery('spoken_sentences');
      SqlAddField(sql, 'permanent_sentence_id', phrase.permanentSentenceId);
      SqlAddField(sql, 'permanent_word_id', phrase.iWord.permanentId);
      SqlAddField(sql, 'timestamp', now);
      SqlAddField(sql, 'bare_sentence', BareString(phrase.sentence)); // TEMP
      SqlAddField(sql, 'user_id', CurrentUserId);
      LbData.ExecuteSql(SqlOf(sql), DbIdUser);
      LbData.Commit(DbIdUser);
    end;

    // Mark the end of playing
    if iActor <> nil then iActor.PlaySentence(phrase, 2);
  end;

  // Play again if marked as such
  if CheckBoxRepeat.checked then
  begin
    inc(iRepeatIndex);
    if iRepeatIndex = 3 then
    begin
      PlayNextPhrase;
    end
    else
    begin
      iBass[iBassCurrent].PlaySelection(iTimeBegin, iTimeEnd, 0.2, false);
    end;
    TimerRepeat.Enabled := true;
  end;
  LabelRepeat.Caption := inttostr(iRepeatIndex);
end;

procedure TFramePlayOneSentence.ButtonPlayNextSentenceClick(Sender: TObject);
begin
  PlayNextPhrase;
end;

function TFramePlayOneSentence.Valid : boolean;
begin
  result := (iCurrentPhraseIndex >= low(iPhrases)) and (iCurrentPhraseIndex <= high(iPhrases));
end;

function TFramePlayOneSentence.PlayNextPhrase : integer;
var phrase : TPhrase;
  translation : TTranslation;
  item : TListItem;
begin
  iRepeatIndex := 0;
  inc(iCurrentPhraseIndex);

  if iCurrentPhraseIndex > high(iPhrases) then
  begin
    // Rotate
    iCurrentPhraseIndex := 0;
  end;

  if not Valid or (length(iPhrases) <> ListSentences.Items.count) then
  begin
    MemoSentence.Text := '';
    MemoTranslation.Text := '';
    LabelLength.caption := '';
    result := -1;
    exit;
  end;

  result := iCurrentPhraseIndex;

  // Reselect
  item := ListSentences.Selected;
  if item <> nil then
  begin
    item.Selected := false;
  end;

  item := ListSentences.items[iCurrentPhraseIndex];
  item.Selected:= true;
  item.Focused := true;
  item.MakeVisible(false);

  phrase := iPhrases[iCurrentPhraseIndex];

  if iHide then
  begin
    MemoSentence.Text := DisplayString(phrase.sentenceHidden);
  end
  else
  begin
    MemoSentence.Text := DisplayString(phrase.sentence);
  end;

  LabelLength.caption := IntToStr(Utf8Length(phrase.sentence));

  // TODO preferred language-order should come from database-user-settings
  translation := GetTranslation(phrase.id,
      [LanguageIdDutch, LanguageIdEnglish, LanguageIdSpanish, LanguageIdGerman]);
  LabelDistance.caption := IntToStr(translation.distance);
  MemoTranslation.Text := translation.translation;

  DownloadAndPlay;
end;

procedure TFramePlayOneSentence.PlayCurrentPhrase;
var
  b : integer;
  phrase : TPhrase;
  loop : boolean;
begin
  if not Valid then
  begin
    exit;
  end;

  // TEMP HERE (TODO: slowly stop the one playing)
  StopAll;
  // END TEMP

  phrase := iPhrases[iCurrentPhraseIndex];

  // Calculate the next slot.
  b := (iBassCurrent + 1) mod length(iBass);

  if iBass[b] <> nil then
  begin
    iBass[b].Stop;
    iBass[b].Free;
  end;

  // Loop can be made by bass (without pauze)
  // but it's more convenient to have a pauze in between.
  loop := false; // CheckBoxRepeat.Checked;

  iTimeBegin := phrase.timeBegin;
  iTimeEnd := phrase.timeEnd;

  iBass[b] := TLbBass.Create(phrase.soundFilename, true);
  if (phrase.timeBegin < 0) or (phrase.timeEnd <= 0) or (phrase.timeBegin >= phrase.timeEnd) then
  begin
    iTimeBegin := 0;
    iTimeEnd := iBass[b].LengthSeconds;
  end;

  iBassCurrent := b;
  iBass[iBassCurrent].PlaySelection(iTimeBegin, iTimeEnd, 0.2, loop);

  iActor.PlaySentence(phrase, 1);

  TimerRepeat.Interval := trunc(1000 * (iTimeEnd - iTimeBegin) + 1000);
  TimerRepeat.Enabled := true;
end;

procedure TFramePlayOneSentence.DownloadAndPlay;
var currentPhrase : TPhrase;
begin
  if not Valid then
  begin
    exit;
  end;
  currentPhrase := iPhrases[iCurrentPhraseIndex];

  if FileExists(currentPhrase.soundFilename) then
  begin
    Log('Play ' + currentPhrase.soundFilename + ' for ' + currentPhrase.sentence);
    PlayCurrentPhrase;
    exit;
  end;

  // Try to download it
  with TFPHttpClient.Create(Nil) do
  try
    try
      get(currentPhrase.url, currentPhrase.soundFilename);
    except
      on E: Exception do
      begin
        Log('Can''t download ' + currentPhrase.url + ' for ' + currentPhrase.sentence);
        DeleteFile(currentPhrase.soundFilename);

        MemoSentence.Text := '(Cannot download)';
        CheckBoxRepeat.Checked := false;

        exit;
      end;
    end;
  finally
    Free;
  end;

  // PlayCurrentPhrase downloaded file (if exists)
  Log('Downloaded ' + currentPhrase.soundFilename + ' for ' + currentPhrase.sentence);
  PlayCurrentPhrase;
end;

procedure TFramePlayOneSentence.Add(index : integer);
var
  phrase : TPhrase;
  sentence : string;
  src : TSourceType;
  it : TListItem;
  translation : TTranslation;
begin
  phrase := iPhrases[index];
  src := SrModels.GetSourceType(phrase.sourceTypeId);

  it := ListSentences.Items.Add;
  if iHide then sentence := DisplayString(phrase.sentenceHidden)
  else sentence := DisplayString(phrase.sentence);

  it.Caption := sentence;

  // TODO: only on request
  translation := GetTranslation(phrase.id, [LanguageIdDutch, LanguageIdEnglish, LanguageIdSpanish, LanguageIdGerman]);
  it.SubItems.Add(translation.translation);
  // END TODO

  // Indication of the source type
  if phrase.sourceTypeId = SourceTypeIdTatoeba then it.SubItems.Add('Татоэба');
  if phrase.sourceTypeId = SourceTypeIdOpenRussian then it.SubItems.Add('OpenRussian');

  if phrase.sourceTypeId >= SourceTypeIdMaxFixed then
  begin
    it.SubItems.Add(src.Author);
    it.SubItems.Add(src.Title);
  end
  else
  begin
    it.SubItems.Add('');
  end;

  it.SubItems.Add(inttostr(phrase.rating));
  it.SubItems.Add(inttostr(phrase.permanentSentenceId));
  if phrase.sourceTypeId < SourceTypeIdMaxFixed then it.SubItems.Add(inttostr(phrase.iWord.PermanentId));
end;

procedure TFramePlayOneSentence.ShowPhrases;
var i : integer;
begin
  ListSentences.items.BeginUpdate;
  try
    ListSentences.items.clear;
    for i := low(iPhrases) to high(iPhrases) do
    begin
      Add(i);
    end;
  finally
    ListSentences.items.EndUpdate;
  end;
  LabelWarning.Visible := length(iPhrases) = 0;
end;

function TFramePlayOneSentence.ShowSentences(const a : TArrayOfStudyWordForm; hideSearchString : boolean) : boolean;
begin
  StopAll;
  iHide := hideSearchString;

  iPhrases := RetrieveSentences(a);
  ShowPhrases;
  iCurrentPhraseIndex := -1;
  iRepeatIndex := 0;
  PlayNextPhrase;
  result := length(iPhrases) > 0;
end;

procedure TFramePlayOneSentence.HideTargetWord(permanentId : integer);
begin
  iHide := true;
  ShowPhrases;
end;


procedure TFramePlayOneSentence.SetPhrases(const phrases : TArrayOfPhrase);
begin
  StopAll;

  iPhrases := phrases;
  ShowPhrases;
  iCurrentPhraseIndex := -1;
  PlayNextPhrase;
end;

procedure TFramePlayOneSentence.ShowContents(showList, showSentence, showTranslation : boolean);
begin
  ListSentences.Visible := showList;
  MemoSentence.Visible := showSentence;
  MemoTranslation.Visible := showTranslation;
end;


procedure TFramePlayOneSentence.SetActor(a : TFramePlaySentenceActorInterface);
begin
  iActor := a;
end;

procedure TFramePlayOneSentence.StopAll;
var i : integer;
begin
  TimerRepeat.Enabled := false;

  // Stop all playing sounds
  for i := low(iBass) to high(iBass) do if iBass[i] <> nil then iBass[i].Stop;
end;

procedure TFramePlayOneSentence.Clear;
begin
  ListSentences.Items.Clear;
  MemoSentence.Text := '';
  MemoTranslation.Text := '';
  LabelLength.Caption := '';
  //LabelAudioLength.Caption := '';
end;


end.


(*
geweldige nieuwe query:
- voor eenvoudige zinnen
- beginnend met "ik", "jij", etc

select s.sentence_id,s.source_type_id,p.suitability,s.bare_sentence from sentences s join sentences_properties p on s.sentence_id=p.sentence_id
where  p.word_count<=7
and s.source_type_id<>2
and s.sentence_id in (select sentence_id from lnk_sentences_words k
  where word_id in (59063,59069,59068,59064) and k.word_index=1)
order by p.suitability
-- nulls last


UPDATED
select s.sentence_id,s.source_type_id,p.suitability,s.bare_sentence from sentences s 
left join sentences_properties p on s.sentence_id=p.sentence_id
where  
 p.word_count<=7 and p.word_count >= 4
 and s.source_type_id<>2
 and s.sentence_id in (select sentence_id from lnk_sentences_words k 
 where k.word_index = 1 and k.word_id  in (select word_id from words where type_id=5))
 order by p.suitability

*)
