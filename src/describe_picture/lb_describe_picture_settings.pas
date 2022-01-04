// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Settings, read from inifile

unit lb_describe_picture_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lb_lib_deepl;

type


  // Record with settings (stored in ini-file)
  TDescribePictureSettings = record

    // Target language: user enters text in this language, it's translated (using VIA)
    // and translated back into this language
    iTargetLanguage : string;

    // For Unsplash: URL and KEY
    iUnsplashApiUrl : string;
    iUnsplashApiKey : string;

    iDeepLSettings : TDeepLSettings;
  end;


implementation

end.

