// Language Bath - Shared
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Functions to draw text

unit lb_draw_text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure DrawTextWithBoldAccents(canvas : TCanvas; const s : string);
procedure DrawTextConsideringAlignment(canvas : TCanvas; const reference, entered : string);

implementation

uses LazUtf8, lb_lib_string;

procedure DrawTextWithBoldAccents(canvas : TCanvas; const s : string);

  function RemoveAccent(var ch : string) : boolean;
  begin
    result := true;
    if ch = 'а́' then ch := 'а'
    else if ch = 'е́' then ch := 'е'
    else if ch = 'и́' then ch := 'и'
    else if ch = 'о́' then ch := 'о'
    else if ch = 'у́' then ch := 'у'
    else result := false;
  end;

var i, len : integer;
  ch : string;
  skipNext : boolean;
begin
  len := UTF8Length(s);
  skipNext := false;
  for i := 1 to len do
  begin
    if skipNext then skipNext := false
    else
    begin
      ch := utf8copy(s, i, 1);
      canvas.Font.Style := [];
      if RemoveAccent(ch) then canvas.Font.Style := [fsBold]
      else if (i < len) and (utf8copy(s, i + 1, 1) = '''') then
      begin
        canvas.Font.Style := [fsBold];
        skipNext := true;
      end;
      canvas.TextOut(canvas.PenPos.X, canvas.PenPos.Y, ch);
    end;
  end;
end;

function TextWidthUntilSpace(const canvas : TCanvas; const s : string; index : integer) : integer;
var sub, ch : string;
  len, i : integer;
begin
  result := 0;
  sub := '';
  len := UTF8Length(s);
  for i := index to len do
  begin
    ch := utf8copy(s, i, 1);
    if (ch = ' ') then
    begin
      result := canvas.TextWidth(sub);
      exit;
    end;
    sub := sub + ch;
  end;
  result := canvas.TextWidth(sub);
end;

procedure DrawTextConsideringAlignment(canvas : TCanvas; const reference, entered : string);

  function Equivalent(const s1, s2 : string) : boolean;
  begin
    result := RussianReplaceEquivalents(s1) = RussianReplaceEquivalents(s2);
  end;

  // Returns true if both are punctuations, or the reference is a punctuation
  // and the entered was missing (= '*' now hardcoded)
  function PunctuationDifference(const r, e : string) : boolean;
  begin
    result := IsPunctuation(r)
      and (IsPunctuation(e) or (e = '*'))
  end;

const extra = 0; // unused
var currentX, currentY, i, len : integer;
  charRef, charEntered : string;
  color : TColor;
begin
  len := UTF8Length(reference);
  if len <> UTF8Length(entered) then exit;

  currentX := 3;
  currentY := 3;

  i := 1;
  while i <= len do
  begin
    charRef := utf8copy(reference, i, 1);
    charEntered := utf8copy(entered, i, 1);
    if (charRef = #13) or (charEntered = #13) then
    begin
      // Honour newline
      currentX := 3;
      currentY := currentY + canvas.Font.Height;

      if (i < len) and (utf8copy(reference, i + 1, 1) = #10) then
      begin
        // Skip the second char of a #13#10 sequence
        inc(i);
      end;
    end
    else if (charRef = ' ') and (currentX + TextWidthUntilSpace(canvas, reference, i + 1) > canvas.Width) then
    begin
      // Word wrap
      currentX := 3;
      currentY := currentY + canvas.Font.Height;
    end;

    color := clNavy;
    if charRef <> charEntered then
    begin
      // It's not the same. Use color according to severity
      if UTF8LowerString(charRef) = UTF8LowerString(charEntered) then color := clNavy
      else if Equivalent(charRef, charEntered) then color := clNavy
      else if PunctuationDifference(charRef, charEntered) then color := clTeal
      else color := clRed;
    end;

    // Display from reference text (it might show capitals, for example)
    // Also in case of errors, display from reference test.

    // Somehow "j" is covering parts of the i/k before... try to enhance that.
    inc(currentX, 1);
    if charRef = 'j' then inc(currentX, 1);

    canvas.moveto(currentX, currentY);
    canvas.Font.Color := color;
    canvas.TextOut(canvas.PenPos.X, canvas.PenPos.Y, charRef);
    currentX := currentX + canvas.TextWidth(charRef) + extra;

    inc(i);
  end;
end;

end.

