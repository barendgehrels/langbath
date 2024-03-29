// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Program "describe_picture"
// It's still experimental.
// Use case:
// 1: The user retrieves a random picture (possibly with a topic)
// 2: The user describes the picture in the language he is learning (the TARGET language)
// 3: He/she presses "languagetool.org" and get corrections and hints
// 4: He/she presses "deepl" and the text is translated into another language, and then
//    translated back. That way the user sometimes gets corrections, but sometimes
//    alternatives (and sometimes nonsense).
//    For example: RUSSIAN -> POLISH -> RUSSIAN (because polish is related)
//             or :SPANISH -> (PORTUGUESE,CATALAN) -> SPANISH and the most matching one is selected
//                 (matching is done with Levenshtein distance)
// 5: Also, the user can get a translation in his own language (to verify the meaning)

program memorize_text;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lb_form_memorize_text,
  lb_frame_memorize_text, lb_lib,
  lb_lib_json, lb_align_lengths;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMemorizeText, FormMemorizeText);
  Application.Run;
end.

