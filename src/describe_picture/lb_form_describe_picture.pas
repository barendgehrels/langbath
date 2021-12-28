// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Main form

unit lb_form_describe_picture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, lb_frame_describe_picture, lb_describe_picture_settings;

type


  { TFormDescribePicture }

  TFormDescribePicture = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

    iFrame : TFrameDescribe;

    function ReadSettings : TDescribePictureSettings;

  public

  end;

var
  FormDescribePicture: TFormDescribePicture;

implementation

{$R *.lfm}

uses Controls, IniFiles, lb_lib;

{ TFormDescribePicture }

function IniFileName : string;
const
  KIniFile : string = 'langbath.ini';
begin
  result := ConfigDir + '\' + KIniFile;
end;

procedure TFormDescribePicture.FormCreate(Sender: TObject);
begin
  WindowState := wsMaximized;

  iFrame := TFrameDescribe.Create(self, ReadSettings);
  iFrame.Parent := self;
  iFrame.Align := alClient;

end;

procedure TFormDescribePicture.FormDestroy(Sender: TObject);
begin
  FreeAndNil(iFrame);
end;

function TFormDescribePicture.ReadSettings : TDescribePictureSettings;
const kSection = 'describe_picture';
var ini : TIniFile;
begin
  Initialize(result);

  ini := TIniFile.Create(IniFileName);
  try
    result.iDeepLApiUrl := ini.ReadString(KSection, 'deepl_api_url', '');
    result.iDeepLApiKey := ini.ReadString(KSection, 'deepl_api_key', '');
    result.iUnsplashApiUrl := ini.ReadString(KSection, 'unsplash_api_url', '');
    result.iUnsplashApiKey := ini.ReadString(KSection, 'unsplash_api_key', '');
    result.iTargetLanguage := ini.ReadString(KSection, 'target_language', '');
    result.iCheckLanguage := ini.ReadString(KSection, 'check_language', '');
    result.iViaLanguages := SplitString(ini.ReadString(KSection, 'via_languages', ''), ',');
  finally
    ini.free;
  end;
end;

end.



