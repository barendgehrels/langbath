unit lb_draw_word_forms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, lb_word_form_properties;


function GetMaxWidth(aCanvas : TCanvas; const forms : TArrayOfWordFormProperties) : integer;
procedure DrawWordForms(bitmap : TBitmap; const forms : TArrayOfWordFormProperties;
  const selection : TWordFormProperties; AtX, AtY, MaxW, LineHeight : integer);

implementation

uses lb_const, lb_draw_text;

const
  KFormSep = ' / ';


function GetMaxWidth(aCanvas : TCanvas; const forms : TArrayOfWordFormProperties) : integer;
var i, c, tw, w : integer;
begin
  result := 0;
  c := -1;
  w := 0;
  for i := low(forms) to high(forms) do
  begin
    if forms[i].numberId = NumberIdSingular then
    begin
      tw := aCanvas.TextWidth(forms[i].wordForm);
      if forms[i].caseId = c then w := w + tw else w := tw;
      if w > result then result := w;
      c := forms[i].caseId;
    end;
  end;
  //
  result := result + aCanvas.TextWidth(KFormSep);
end;

procedure DrawWordForms(bitmap : TBitmap;
  const forms : TArrayOfWordFormProperties;
  const selection : TWordFormProperties;
  AtX, AtY, MaxW, LineHeight : integer);

  function GetX(const form : TWordFormProperties; maxW : integer) : integer;
  var col : integer;
  begin
    if selection.typeId = typeIdNoun then col := form.numberId - 1
    else col := 2 - form.tenseId;
    result := AtX + col * (maxW + AtX);
  end;

  function GetY(const form : TWordFormProperties) : integer;
  var row : integer;
  begin
    if selection.typeId = typeIdNoun then row := form.caseId - 1
    else row := (form.numberId - 1) * 3 + form.personId;
    result := AtY + row * LineHeight
  end;

var i : integer;
  previous : TWordFormProperties;
begin
  Initialize(previous);

  for i := low(forms) to high(forms) do
  begin
    if (forms[i].tenseId = 2) or (forms[i].tenseId = 0) then
    begin
      if (forms[i].caseId = selection.caseId)
      and (forms[i].numberId = selection.numberId)
      and (forms[i].personId = selection.personId)
      then bitmap.Canvas.Font.Color := clBlue
      else bitmap.Canvas.Font.Color := clBlack;

      if (forms[i].caseId <> previous.caseId)
      or (forms[i].tenseId <> previous.tenseId)
      or (forms[i].numberId <> previous.numberId)
      or (forms[i].personId <> previous.personId)
      then
      begin
        // Go to next position
        bitmap.Canvas.MoveTo(GetX(forms[i], maxW), GetY(forms[i]))
      end
      else
      begin
        // Add a separator "/" and dra next one afterwards
        bitmap.Canvas.TextOut(bitmap.Canvas.PenPos.X, bitmap.Canvas.PenPos.Y, KFormSep);
      end;

      DrawTextWithBoldAccents(bitmap.Canvas, forms[i].wordForm);

      previous := forms[i];
    end;
  end;
end;

end.

