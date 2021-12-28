// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Settings, read from inifile

unit lb_describe_picture_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDescribePictureSettings = record
    iDeepLApiUrl : string;
    iDeepLApiKey : string;
    iUnsplashApiUrl : string;
    iUnsplashApiKey : string;
    iTargetLanguage : string;
    iCheckLanguage : string;
    iViaLanguages : array of string;
  end;


implementation

end.

