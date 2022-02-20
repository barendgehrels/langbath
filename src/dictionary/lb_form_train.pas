unit lb_form_train;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SqlDb;

type

  { TFormTrain }

  TFormTrain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxRepetition: TCheckBox;
    LabelScore: TLabel;
    MemoTranslation: TMemo;
    MemoSentence: TMemo;
    PaintBoxVerifyResult: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelTranslation: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxRepetitionChange(Sender: TObject);
    procedure PaintBoxVerifyResultPaint(Sender: TObject);
  private
    iConnectionLanguage : TSqlConnection;
    iConnectionTrain : TSqlConnection;
    iReference : string;
    iResult : string;

    iSentenceId, iWordId, iTranslationId : integer;

  public
    procedure SetIds(sentenceId, wordId, translationId : integer);
    procedure Clear(all : boolean);

  public
    property ConnectionLanguage : TSqlConnection write iConnectionLanguage;
    property ConnectionTrain : TSqlConnection write iConnectionTrain;
  end;

var
  FormTrain: TFormTrain;

implementation

{$R *.lfm}

uses LazUtf8, lb_score, lb_lib_string, lb_needleman_wunsch, lb_draw_text,
  lb_db_functions, lb_sql_dml_insert, lb_lib;

procedure TFormTrain.Button1Click(Sender: TObject);

  function Score(const s, ref : string) : double;
  begin
    result := GetScore(s, ref) * 100.0;
  end;

var score1, score2 : double;
  enteredText, reference : string;
begin
  LabelScore.Caption := '';
  if iReference = '' then exit;

  enteredText := DisplayString(MemoSentence.text);
  reference := DisplayString(iReference);

  score1 := Score(enteredText, reference);

  // Without correct punctuation and all in lowercase
  score2 := Score(RemovePunctuation(UTF8LowerString(enteredText)),
                  RemovePunctuation(UTF8LowerString(reference)));

  LabelScore.Caption := format('%.0f%%', [score2]);
  //Log('Check "' + enteredText
  //  + '" and "' + reference + ' -> ' + FloatToStr(score1) + ' ' + FloatToStr(score2));

  iReference := RemovePunctuation(UTF8LowerString(reference));
  iResult := RemovePunctuation(UTF8LowerString(enteredText));

  PaintBoxVerifyResult.Invalidate;
end;

procedure TFormTrain.Button2Click(Sender: TObject);
begin
  Clear(false);
end;

procedure TFormTrain.CheckBoxRepetitionChange(Sender: TObject);
var psid, pwid, lid, id : integer;
begin
  psid := QueryAsInteger(iConnectionLanguage, 'select permanent_id from sentences where sentence_id=%d', [iSentenceId]);
  pwid := QueryAsInteger(iConnectionLanguage, 'select permanent_id from words where word_id=%d', [iWordId]);
  lid := QueryAsInteger(iConnectionLanguage, 'select language_id from translations where translation_id=%d', [iTranslationId]);

  if (psid <= 0) or (pwid <= 0) or (lid <= 0) then exit;

  if CheckBoxRepetition.Checked then
  begin
    id := QueryAsInteger(iConnectionTrain,
      'select train_id from train where permanent_word_id=%d'
      + ' and permanent_sentence_id=%d'
      + ' and language_id=%d', [pwid, psid, lid]);
    if id <= 0 then
    begin
      SqlInsert(iConnectionTrain, 'train',
        [cv('permanent_word_id', pwid),
        cv('permanent_sentence_id', psid),
        cv('language_id', lid),
        cv('timestamp', now),
        cv('interval_age', 0.46875),
        cv('interval_update', now)]);
      iConnectionTrain.Transaction.Commit;
    end;
  end;
  Log(format('sentence_id: %d, word_id: %d, translation_id: %d',
    [iSentenceId, iWordId, iTranslationId]));
end;

procedure TFormTrain.PaintBoxVerifyResultPaint(Sender: TObject);
var bitmap : TBitmap;
  s1, s2 : string;
begin
  if (iReference = '') or (iResult = '') then exit;

  AlignWithNeedlemanWunsch(iReference, iResult, '*', -10, s1, s2);

  // Draw indiretly, using a bitmap, to avoid flickering.
  // We might make it a class member and draw only when things change.
  bitmap := TBitmap.Create;
  try
    bitmap.SetSize(PaintBoxVerifyResult.Width, PaintBoxVerifyResult.Height);
    bitmap.Canvas.Brush.Color := self.color;
    bitmap.Canvas.FillRect(0, 0, bitmap.Width, bitmap.Height);

    bitmap.Canvas.Font.Name := 'Segoe UI';
    bitmap.Canvas.Font.Style := [];
    bitmap.Canvas.Font.Height := 36;
    bitmap.Canvas.Font.Color := clNavy;

    bitmap.Canvas.MoveTo(3, 3);
    DrawTextConsideringAlignment(bitmap.Canvas, s1, s2);

    PaintBoxVerifyResult.Canvas.Draw(0, 0, bitmap);
  finally
    bitmap.Free;
  end;
end;

procedure TFormTrain.SetIds(sentenceId, wordId, translationId: integer);
var q : TSQLQuery;
begin
  Clear(true);

  q := TSQLQuery.Create(nil);
  try
    q.Database := iConnectionLanguage;
    q.SQL.Text := format('select * from sentences where sentence_id = %d', [sentenceId]);
    q.Open;

    if not q.eof then
    begin
      iReference := q.fieldbyname('bare_sentence').AsString;

      iSentenceId := sentenceId;
      iWordId := wordId;
      iTranslationId := translationId;
    end;

  finally
    q.free;
  end;

  MemoTranslation.text := QueryAsString(iConnectionLanguage,
    'select sentence from translations where translation_id=%d', [translationId]);
end;

procedure TFormTrain.Clear(all : boolean);
begin
  iSentenceId := 0;
  iWordId := 0;
  iTranslationId := 0;
  iResult := '';
  MemoSentence.text := '';
  LabelScore.caption := '';

  if all then
  begin
    iReference := '';
    MemoTranslation.text := '';
  end;

  PaintBoxVerifyResult.Invalidate;
end;

end.

