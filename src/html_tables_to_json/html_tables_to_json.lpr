// Language Bath
// Copyright (c) 2021 Barend Gehrels, Amsterdam, the Netherlands.
// Use, modification and distribution is subject to the MIT License
// https://raw.githubusercontent.com/barendgehrels/langbath/main/LICENSE

// See description in the main unit

program html_tables_to_json;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FpJson, JsonParser, lb_html_tables_to_json, lib_file_lib;

procedure JsonVerify(const jsonFilename : string);

  function GetTag(jsonData : TJsonData; const tag : string) : string;
  var sub : TJsonData;
  begin
    result := '';
    sub := jsonData.FindPath(tag);
    if sub <> nil then
    begin
      result := sub.AsString;
    end;
  end;

var
  head, rows, cols, first, s: String;
  i, n : integer;
  jsonData : TJSONData;
begin
  s := TextFileAsString(jsonFilename);

  try
    jsonData := GetJSON(s, true);
    if TryStrToInt(GetTag(jsonData, 'ntables'), n) and (n > 0) then
    begin
      for i := 0 to n - 1 do
      begin
        head := GetTag(jsonData, format('tables[%d].h1', [i]));
        rows := GetTag(jsonData, format('tables[%d].nrows', [i]));
        cols := GetTag(jsonData, format('tables[%d].ncols', [i]));
        first := GetTag(jsonData, format('tables[%d].data[0][0]', [i]));
        writeln(format('%s %s * %s: %s', [head, rows, cols, first]));
      end;
    end
    else
    begin
      writeln('No tables found');
    end;
  except
    on e : exception do
    begin
      writeln(e.Message, ' in ', jsonFilename);
    end
  end;
end;

procedure ConvertHtmlTablesToJson(const inputHtmlFilename, outputJsonFilename : string;
    verify : boolean);
var txt : textfile;
begin
  assign(txt, outputJsonFilename);
  {$I-}
  rewrite(txt);
  {$I+}
  if IOresult <> 0 then exit;

  writeln(txt, HtmlTablesToJson(TextFileAsString(inputHtmlFilename)));

  closefile(txt);

  if verify then
  begin
    JsonVerify(outputJsonFilename);
  end;
end;

procedure Sample;
const html : string =
  '<html><body><table border="1">'
  + '<tr><th>Firstname</th><th>Lastname</th><th>Age</th></tr>'
  + '<tr><td>Jill</td><td>Smith</td><td>50</td></tr>'
  + '<tr><td>Eve</td><td>Jackson</td><td>94</td></tr>'
  + '</table></body></html/>';
begin
  writeln(HtmlTablesToJson(html));
end;

var error, verify : boolean;
  url, inputFilename, outputFilename : string;
  useUrl : boolean;
begin
  if (paramCount = 1) and (paramstr(1) = 'sample') then
  begin
    sample;
    halt;
  end;

  if ParamCount < 2 then
  begin
    writeln('Usage: ' + ParamStr(0) + ' [input html file/url] [output json file] {verify}');
    halt;
  end;

  error := false;
  inputFilename := Paramstr(1);
  outputFilename := Paramstr(2);
  useUrl := IsUrl(inputFilename);

  // Use the first parameter as either a URL or a filename
  if not useUrl and not FileExists(inputFilename) then
  begin
    error := true;
    writeln('File not found: ' + inputFilename);
  end;

  if FileExists(outputFilename) then
  begin
    error := true;
    writeln('JSON file already exists: ' + outputFilename);
  end;

  if not error and useUrl then
  begin
    url := inputFilename;
    inputFilename := GetTempFileName(GetTempDir, 'lb') + '.html';

    if not DownloadHtmlToFile(url, inputFilename)
    or not FileExists(inputFilename) then
    begin
      writeln('Cannot download ', url);
      error := true;
    end
    else
    begin
      writeln('Downloaded ', url, ' to temporary file ', inputFilename);
    end;
  end;

  if not error then
  begin
    verify := (ParamCount >= 3) and (paramstr(3) = 'verify');
    ConvertHtmlTablesToJson(inputFileName, outputFileName, verify);
  end;

  // Remove the URL downloaded earlier.
  // Defensive check if it is in the temp folder
  if useUrl
  and inputFilename.StartsWith(GetTempDir)
  and FileExists(inputFilename)
  then
  begin
    writeln('Deleting temporary file ', inputFilename);
    DeleteFile(inputFilename);
  end;
end.


