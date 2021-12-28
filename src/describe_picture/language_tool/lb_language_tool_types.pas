// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/src/LICENSE

// Defines types for the "languagetool" utility (languagetool.org)
unit lb_language_tool_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLanguageToolHint = record
    offset : integer; // It is a one-based index into the UTF8 string
    length : integer;
    inputPart : string;
    replacement : string;
    replacements : array of string;
    message : string;
    issueType : string;
    categoryId : string;
  end;

  TLanguageToolCorrection = record
    detectedLanguageCode : string;
    detectedLanguage : string;
    hints : array of TLanguageToolHint;
  end;


implementation

end.

