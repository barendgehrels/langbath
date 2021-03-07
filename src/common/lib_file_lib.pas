unit lib_file_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Returns true if the specified string starts with http:// or https://
function IsUrl(const url : string) : boolean;

// Reads a file and returns its contents as a string
function TextFileAsString(const filename : string) : string;

// Returns the size of the file
function GetFileSize(const filename : string) : int64;

// Downloads a url to a file
function DownloadHtmlToFile(const url, filename : string) : boolean;

implementation

uses LazUTF8, FpHttpClient, OpenSslSockets, FileUtil;

function IsUrl(const url: string): boolean;
var u : string;
begin
  u := lowercase(url);
  result := u.StartsWith('http://') or u.StartsWith('https://');
end;

function TextFileAsString(const filename : string) : string;
var txt : TextFile;
  s : string;
begin
  result := '';

  AssignFile(txt, filename);
  {$I-} Reset(txt); {$I+}
  if IOResult <> 0 then exit;

  try
    while not eof(txt)  do
    begin
      ReadLn(txt, s);
      result := result + s;
    end;
  finally
    CloseFile(txt);
  end;
end;

function DownloadHtmlToFile(const url, filename : string) : boolean;
begin
  result := true;
  // Try to download it
  with TFPHttpClient.Create(Nil) do
  try
    try
      get(url, filename);
    except
      on E: Exception do
      begin
        result := false;
        DeleteFile(filename);
      end;
    end;
  finally
    Free;
  end;
end;

function GetFileSize(const filename : string) : int64;
var f : file of byte;
begin
  result := 0;
  assign (f, filename);
  {$I-}
  reset (f);
  {$I+}
  if IOResult <> 0 then exit;

  result := FileSize(f);
  close(f);
end;

end.

