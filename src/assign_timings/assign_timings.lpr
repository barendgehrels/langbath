// Language Bath - Assign Timings
// Copyright (c) 2020 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See lb_frame_book_sentences for the main description

program assign_timings;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lb_form_assign_timings, lb_write_timings, lb_ui_lib, lb_book_settings,
  lb_time_optimizer, lb_form_edit_book_settings, lb_form_wave_form
  { you can add units after this }
  ;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='langbath';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormAssignTimes, FormAssignTimes);
  Application.CreateForm(TFormEditBookSettings, FormEditBookSettings);
  Application.CreateForm(TFormWaveForm, FormWaveForm);
  Application.Run;
end.

