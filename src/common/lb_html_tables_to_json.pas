// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// This unit contains a converter from HTML to JSON, where:
//   - all HTML tables, if any, are converted to JSON arrays
//   - all other formatting is lost
//   - the JSON is formatted such that it could be converted back to HTML (not tested yet)
//   - nested tables are not supported, their contents is placed in the cell it's containing
// It makes use of the FPC FastHtmlParser
// TODO: rowspan/colspan (will be properties in produced json, they're not really merged)
//
// Sample output:
// {"ntables": 1, "tables": [{"nrows": 3, "nheads": 1, "ncols": 3, "data":
// [["Firstname","Lastname","Age"],["Jill","Smith","50"],["Eve","Jackson","94"]]}]}
//

unit lb_html_tables_to_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  THtmlTablesToJson = class
    inMainHeader : integer;
    inTable : integer;
    inCell : integer;

    headCount : integer;
    rowIndex : integer;
    colIndex : integer;
    tableIndex : integer;

    mainHeader : string;
    outputCell : string;
    outputRow : string;
    currentOutputTable : string;
    outputTables : array of string;

    procedure ProcessHeader(isStart : boolean);
    procedure ProcessTable(isStart : boolean);
    procedure ProcessTableRow(isStart : boolean);
    procedure ProcessTableData(isStart, isHead : boolean);
    procedure ProcessText(const s: string);

    procedure ProcessTag(const tag : string; isStart : boolean);
    procedure OnTagEvent(UpperCaseTag, ActualTag: string);
    procedure OnTextEvent(Text: string);

  public
    constructor Create;

    procedure Parse(const htmlString : string);
  end;

// Convert specified HTML string (not a filename) to a JSON string
function HtmlTablesToJson(const htmlString : string) : string;

implementation

uses FastHtmlParser;

function Conditional(k : integer) : string;
begin
  if k > 0 then result := ',' else result := '';
end;

procedure AppendString(var s : string; const fmt : String;
  const args : Array of const);
begin
  s := s + format(fmt, args);
end;

procedure AppendString(var s : string; const t : String);
begin
  s := s + t;
end;

constructor THtmlTablesToJson.Create;
begin
  outputTables := [];
end;

procedure THtmlTablesToJson.ProcessHeader(isStart: boolean);
begin
  if isStart then
  begin
    mainHeader := '';
    inc(inMainHeader);
  end
  else dec(inMainHeader);
end;

procedure THtmlTablesToJson.ProcessTable(isStart: boolean);
var n : integer;
begin
  if IsStart then
  begin
    if inTable = 0 then
    begin
      headCount := 0;
      rowIndex := 0;
      colIndex := 0;
    end;

    inc(inTable);
  end
  else
  begin
    dec(inTable);
    if inTable = 0 then
    begin
      n := length(outputTables);
      SetLength(outputTables, n + 1);

      outputTables[n] := '';
      AppendString(outputTables[n],
        '"nrows": %d, "nheads": %d, "ncols": %d', [rowIndex, headCount, colIndex]);
      if mainHeader <> '' then
      begin
        AppendString(outputTables[n], ', "h1": "%s"', [mainHeader]);
      end;
      AppendString(outputTables[n], ', "data": [%s]', [currentOutputTable]);

      currentOutputTable := '';
      inc(tableIndex);
    end;
  end;
end;

procedure THtmlTablesToJson.ProcessTableRow(isStart : boolean);
begin
  if isStart then colIndex := 0
  else
  begin
    AppendString(currentOutputTable, '%s[%s]', [Conditional(rowIndex), outputRow]);

    outputRow := '';
    inc(rowIndex);
  end;
end;

procedure THtmlTablesToJson.ProcessTableData(isStart, isHead : boolean);
begin
  if isStart then
  begin
    outputCell := '';
    inc(inCell);
  end
  else
  begin
    AppendString(outputRow, '%s"%s"', [Conditional(colIndex), trim(OutputCell)]);

    if isHead and (colIndex = 0) then inc(headCount);
    inc(colIndex);
    dec(inCell);
  end;
end;

procedure THtmlTablesToJson.ProcessText(const s: string);

  function ValidJsonString : string;
  begin
    // Replace newlines and quotes by equivalents keeping JSON valid
    // and still recognizable in postprocessing steps
    result := StringReplace(s, '"', '&quot;', [rfReplaceAll]);

    result := StringReplace(result, #10, '', [rfReplaceAll]);
    result := StringReplace(result, #13, '\n', [rfReplaceAll]);
    result := StringReplace(result, #9, '\t', [rfReplaceAll]);
  end;

begin
  if (inTable > 0) and (inCell > 0) then
  begin
    outputCell := outputCell + ValidJsonString();
  end
  else if inMainHeader > 0 then
  begin
    mainHeader := mainHeader + ValidJsonString();
  end;
end;

procedure THtmlTablesToJson.ProcessTag(const tag: string; isStart: boolean);
begin
  if tag = 'h1' then ProcessHeader(isStart)
  else if tag = 'table' then ProcessTable(isStart);

  if inTable <> 1 then
  begin
    // Nested tables are not fully supported
    // but all text inside it is still written in the cell of the main table
    exit;
  end;

  if tag = 'th' then ProcessTableData(isStart, true)
  else if tag = 'td' then ProcessTableData(isStart, false)
  else if tag = 'tr' then ProcessTableRow(isStart)
  else if tag = 'br' then ProcessText('\n')
end;

{$push}{$warn 5024 off} // Avoid warning about unused actualTag
procedure THtmlTablesToJson.OnTagEvent(UpperCaseTag, ActualTag: string);
  procedure AdaptTag(out atag : string; out isStart : boolean);
  var p : integer;
    isEnd : boolean;
  begin
    atag := LowerCase(UpperCaseTag);

    if pos('<', atag) = 1 then delete(atag, 1, 1);

    isEnd := pos('/', atag) = 1;
    if isEnd then delete(atag, 1, 1);
    p := pos(' ', atag);
    if p > 0 then atag := copy(atag, 1, p - 1)
    else if atag.EndsWith('>') then
    begin
      delete(atag, length(atag), 1);
    end;

    isStart := not isEnd;
  end;

var atag : string;
  isStart : boolean;
begin
  AdaptTag(atag, isStart);
  ProcessTag(atag, isStart);
end;
{$pop}

procedure THtmlTablesToJson.OnTextEvent(Text: string);
begin
  ProcessText(text);
end;

procedure THtmlTablesToJson.Parse(const htmlString : string);
var parser: THTMLParser;
begin
  parser := THTMLParser.create(htmlString);
  try
    parser.OnFoundTag := @OnTagEvent;
    parser.OnFoundText := @OnTextEvent;
    parser.Exec;
  finally
    parser.free;
  end;
end;

function HtmlTablesToJson(const htmlString : string) : string;
var i : integer;
  converter : THtmlTablesToJson;
begin
  result := '';

  converter := THtmlTablesToJson.create;
  try
    converter.Parse(htmlString);

    AppendString(result, '{"ntables": %d, "tables": [', [length(converter.outputTables)]);
    for i := low(converter.OutputTables) to high(converter.outputTables) do
    begin
      AppendString(result, '%s{%s}', [Conditional(i), converter.outputTables[i]]);
    end;
    AppendString(result, ']}');

  finally
    converter.free;
  end;
end;

end.

