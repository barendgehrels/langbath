program dictionary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, dictionary_main, lb_frame_search_words, lb_frame_show_sentences, lb_frame_db_image, lb_db_search,
  lb_form_train, lb_ini_dictionary, lb_db_train, lb_word_form_properties, lb_db_get_word_form_properties
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormTrain, FormTrain);
  Application.Run;
end.

