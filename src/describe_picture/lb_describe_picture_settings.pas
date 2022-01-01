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
  // Record with settings (stored in ini-file)
  TDescribePictureSettings = record
    // For DeepL: URL and KEY
    iDeepLApiUrl : string;
    iDeepLApiKey : string;

    // For Unsplash: URL and KEY
    iUnsplashApiUrl : string;
    iUnsplashApiKey : string;

    // Target language: user enters text in this language, it's translated (using VIA)
    // and translated back into this language
    iTargetLanguage : string;

    // Via languages: one or more languages to translate the entered text to, and then from
    iViaLanguages : array of string;

    // Other language (usually the user's native language) to check if the sentence makes sense.
    iCheckLanguage : string;
  end;


implementation

end.

