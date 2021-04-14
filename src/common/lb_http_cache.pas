unit lb_http_cache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function UseCacheOrDownload(const url, baseName, cacheFolder : string) : string;

implementation

uses LazUTF8, FpHttpClient, OpenSslSockets, lb_lib;

function GetFileSize(const filename : string) : integer;
var f : file Of byte;
begin
  assign (f, filename);
  reset (f);
  result := FileSize(f);
  close(f);
end;

function UseCacheOrDownload(const url, baseName, cacheFolder: string): string;
begin
  result := format('%s/%s.html', [cacheFolder, baseName]);
  if FileExists(result) then
  begin
    log(format('Use %s for %s', [result, baseName]));
    exit;
  end;

  // Try to download it
  with TFPHttpClient.Create(Nil) do
  try
    try
      get(url, result);
      Log(format('Cached %s into %s [%d bytes]', [url, result, GetFileSize(result)]));
    except
      on E: Exception do
      begin
        Log('Can''t download ' + url);
        DeleteFile(result);
      end;
    end;
  finally
    Free;
  end;
end;

end.

