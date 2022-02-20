unit lb_frame_show_sentences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  SqlDb, lb_bass, lb_const;

type

  TFragment = record
    sentenceId : integer;
    wordId : integer;
    translationId : integer;
    filename : string;
    url : string;
    timeBegin, timeEnd : double;
  end;

  { TFrameShowSentences }

  TFrameShowSentences = class(TFrame)
    ButtonTrain: TButton;
    ButtonPlay: TSpeedButton;
    ButtonStop: TSpeedButton;
    CheckBoxRepeat: TCheckBox;
    LabelError: TLabel;
    ListSentences: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TimerRepeat: TTimer;
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonTrainClick(Sender: TObject);
    procedure ListSentencesResize(Sender: TObject);
    procedure ListSentencesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TimerRepeatTimer(Sender: TObject);
  private
    iConnection : TSqlConnection;
    iFolderSoundCache : string;
    iUrls : array[1..2] of string;

    iBass : array[0..5] of TLbBass;
    iBassCurrent : integer;

    iFragments : array of TFragment;
    iTimeBegin, iTimeEnd : double;

    procedure DownloadFragment(const fragment : TFragment);
    function GetFragment(item : TListItem) : TFragment;
    procedure PlayCurrentSentence;
public
    procedure ShowSentencesOfWord(wordId : integer);
  public
    property Connection : TSqlConnection write iConnection;
    property FolderSoundCache : string write iFolderSoundCache;
    property UrlOpenRussian : string write iUrls[sourceTypeIdOpenRussian];
    property UrlTatoeba : string write iUrls[sourceTypeIdTatoeba];
  end;

implementation

{$R *.lfm}

uses FpHttpClient, OpenSslSockets, lb_form_train, lb_lib;

function TFrameShowSentences.GetFragment(item: TListItem): TFragment;
begin
  Initialize(result);

  if (item <> nil)
  and (item.Index >= low(iFragments))
  and (item.index <= high(iFragments))
  then
  begin
    result := iFragments[item.Index];

    if not FileExists(result.filename) then
    begin
      result.filename := format('%s/%s', [iFolderSoundCache, result.filename]);
      // if it still does not exist, it will be downloaded
      if not FileExists(result.filename) then
      begin
        DownloadFragment(result);
      end;
    end
  end;
end;

procedure TFrameShowSentences.ListSentencesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var b : integer;
  fragment : TFragment;
begin
  LabelError.caption := '';

  if not selected then exit;
  fragment := GetFragment(item);

  if (fragment.sentenceId > 0)
  and FileExists(fragment.filename) then
  begin
    // Use next slot or circulate to first
    b := (iBassCurrent + 1) mod length(iBass);

    if iBass[b] <> nil then
    begin
      iBass[b].Stop;
      iBass[b].Free;
    end;

    iTimeBegin := fragment.timeBegin;
    iTimeEnd := fragment.timeEnd;

    iBass[b] := TLbBass.Create(fragment.filename, true);
    if (fragment.timeBegin < 0) or (fragment.timeEnd <= 0) or (fragment.timeBegin >= fragment.timeEnd) then
    begin
      iTimeBegin := 0;
      iTimeEnd := iBass[b].LengthSeconds;
    end;

    iBassCurrent := b;

    PlayCurrentSentence;
  end;
end;

procedure TFrameShowSentences.TimerRepeatTimer(Sender: TObject);
begin
  TimerRepeat.Enabled := false;
  if CheckBoxRepeat.Checked then
  begin
    PlayCurrentSentence;
  end;
end;

procedure TFrameShowSentences.DownloadFragment(const fragment : TFragment);
var s : string;
begin
  LabelError.caption := '';

  // Try to download it
  with TFPHttpClient.Create(Nil) do
  try
    try
      get(fragment.url, fragment.filename);
      Log('Downloaded ' + fragment.url + ' into ' + fragment.filename);
      LabelError.caption := format('Downloaded %s', [ExtractFilename(fragment.Filename)]);
    except
      on E: Exception do
      begin
        DeleteFile(fragment.Filename);
        s := format('Cannot download %s : %s', [fragment.url, e.message]);
        Log(s);
        LabelError.caption := s;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TFrameShowSentences.PlayCurrentSentence;
var c : integer;
begin
  c := iBassCurrent;
  if (c >= low(iBass))
  and (c <= high(iBass))
  and (iBass[c] <> nil)
  and not iBass[c].Active
  then
  begin
    iBass[c].PlaySelection(iTimeBegin, iTimeEnd, 0.2, false);

    TimerRepeat.Interval := trunc(1000 * (iTimeEnd - iTimeBegin) + 1000);
    TimerRepeat.Enabled := true;
  end;
end;

procedure TFrameShowSentences.ButtonPlayClick(Sender: TObject);
begin
  PlayCurrentSentence;
end;

procedure TFrameShowSentences.ButtonStopClick(Sender: TObject);
var i : integer;
begin
  TimerRepeat.enabled := false;
  for i := low(iBass) to high(iBass) do if iBass[i] <> nil then iBass[i].Stop;
end;

procedure TFrameShowSentences.ButtonTrainClick(Sender: TObject);
var fragment : TFragment;
begin
  fragment := GetFragment(ListSentences.Selected);
  if fragment.sentenceId > 0 then
  begin
    ListSentences.Visible := false;
    try
      FormTrain.SetIds(fragment.sentenceId, fragment.wordId, fragment.TranslationId);
      FormTrain.ShowModal;

    finally
      ListSentences.Visible := true;
    end;
  end;
end;

procedure TFrameShowSentences.ListSentencesResize(Sender: TObject);
begin

end;

procedure TFrameShowSentences.ShowSentencesOfWord(wordId: integer);
const sql =
  'select s.sentence_id,s.sentence,s.permanent_id,s.rating,s.source_type_id'
  + ' ,t.language_id,t.translation_id,t.sentence as translation'
  + ' ,st.author,st.title,p.suitability'
  + ' ,u.sound_file,u.is_human,k.time_begin,k.time_end'
  + ' from sentences s'
  + ' join sentences_properties p on s.sentence_id=p.sentence_id'
  + ' left join translations t on s.sentence_id=t.sentence_id'
  + ' left join source_types st on s.source_type_id=st.source_type_id'
  + ' left join lnk_sentences_sounds k on s.sentence_id=k.sentence_id'
  + ' left join sounds u on k.sound_id=u.sound_id'
  + ' where s.sentence_id in (select sentence_id from lnk_sentences_words where word_id=:WORDID)'
  + ' order by u.is_human desc,p.suitability,s.sentence_id,t.language_id';
var q : TSQLQuery;
  item : TListItem;
  n, previousSentenceId, sourceTypeId, sentenceId, translationId, languageId  : integer;
  isHuman : boolean;
  url : string;
begin
  LabelError.caption := '';

  iFragments := [];
  ListSentences.Items.Clear;
  previousSentenceId := -1;
  item := nil;
  n := 0;
  isHuman := true;

  q := TSQLQuery.Create(nil);
  try
    q.Database := iConnection;
    q.SQL.Text := format(sql, [wordId]);
    q.Params[0].Value := wordId;
    q.Prepare;
    q.Open;

    log(q.sql.text);

    while not q.eof do
    begin
      sentenceId := q.FieldByName('sentence_id').AsInteger;
      translationId := q.FieldByName('translation_id').AsInteger;
      languageId := q.FieldByName('language_id').AsInteger;
      sourceTypeId := q.FieldByName('source_type_id').AsInteger;

      if sentenceId <> previousSentenceId then
      begin
        isHuman := q.FieldByName('is_human').AsBoolean;

        // If sounds are not natural (human), don't offer more than 50 alternatives
        if not isHuman and (n >= 50) then exit;

        item := ListSentences.Items.Add;
        SetLength(iFragments, n + 1);
        iFragments[n].sentenceId := sentenceId;
        iFragments[n].wordId := wordId;
        iFragments[n].translationId := translationId;
        iFragments[n].filename := q.FieldByName('sound_file').AsString;
        iFragments[n].timeBegin := q.FieldByName('time_begin').AsFloat;
        iFragments[n].timeEnd := q.FieldByName('time_end').AsFloat;

        if (sourceTypeId >= 1) and (sourceTypeId <= 2) then url := iUrls[sourceTypeId]
        else url := '';

        iFragments[n].url := format('%s/%s', [url, iFragments[n].filename]);

        inc(n);
      end
      else if (languageId = LanguageIdDutch) and (item <> nil) then
      begin
        // Replace the contents for a more favorite language
        assert(n > 0);
        iFragments[n - 1].translationId := translationId;
        item.SubItems.Clear;
      end;

      if item <> nil then
      begin
        item.Caption := q.FieldByName('sentence').AsString;
        item.SubItems.Add(q.FieldByName('translation').AsString);
        item.SubItems.Add(q.FieldByName('author').AsString);
        item.SubItems.Add(q.FieldByName('title').AsString);
        item.SubItems.Add(q.FieldByName('suitability').AsString);
        item.SubItems.Add(q.FieldByName('permanent_id').AsString);
      end;

      previousSentenceId := sentenceId;
      q.next;
    end;

  finally
    q.free;
  end;
  if n = 0 then
  begin
    LabelError.caption := 'No sentences found';
  end;
end;

end.

