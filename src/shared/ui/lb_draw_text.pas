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
var currentX, currentY, w, i, len : integer;
  ch1, ch2 : string;
begin
  len := UTF8Length(reference);
  if len <> UTF8Length(entered) then exit;

  currentX := 3;
  currentY := 3;

  for i := 1 to len do
  begin
    ch1 := utf8copy(reference, i, 1);
    ch2 := utf8copy(entered, i, 1);
    w := canvas.TextWidth(ch1);
    currentX := currentX + w;
    if (ch1 = ' ') and (currentX + TextWidthUntilSpace(canvas, reference, i + 1) > canvas.Width) then
    begin
      currentX := 3;
      currentY := currentY + canvas.Font.Height;
      canvas.moveto(currentX, currentY);
    end;

    if RussianReplaceEquivalents(ch1) = RussianReplaceEquivalents(ch2) then
    begin
      canvas.Font.Color := clNavy;
      // It's either the same, or the entered text is accepted.
      // So display from entered text
      canvas.TextOut(canvas.PenPos.X, canvas.PenPos.Y, ch2);
    end
    else
    begin
      canvas.Font.Color := clRed;
      canvas.TextOut(canvas.PenPos.X, canvas.PenPos.Y, ch1);
    end;
  end;

  canvas.Font.Color := clNavy;
  canvas.Font.Style := [fsBold];
end;

end.

