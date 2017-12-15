unit UGlobalUnit;

interface

uses
  System.SysUtils,
  IdHTTP,
  System.Classes,
  System.RegularExpressions,
  IdSSLOpenSSL,
  Vcl.Forms;

type
  // TTypeLanguage = (tlPersian, tlEnglish);
  TTutorState     = (tsNon, tsReady, tsStarted, tsFinished);
  TKeyboardLayout = (klEnglish, klMicrosoftPersian, klPersian);

const
  NULL_CHAR           = #13;
  TUTOR_EXT           = '.ttd';
  NULL_CHAR_INDEX     = -1;
  APPLICATION_VERSION = '1.8';
  VIRTUAL_SPACE       = #$200C;

var
  LatestVersion : string;

function ApplicationPath: String;
procedure GetLatestVersion;

implementation

function ApplicationPath: String;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

Function ExtractVersion(InputtedTerm: String): String;
var
  RegularExpression: TRegEx;
  Match            : TMatch;
begin
  try
    Result := '';
    Match := RegularExpression.Match(InputtedTerm, '("tag_name": ")(.*?)(")');
    if Match.Groups.Count <> 4 then
    begin
      Exit;
    end;
    Result := Match.Groups[2].Value;
  finally
  end;
end;

procedure GetLatestVersion;
var
  lHTTP      : TIdHTTP;
  LHandler   : TIdSSLIOHandlerSocketOpenSSL;
  Src        : String;
  LastVersion: String;
begin
  try
    lHTTP := TIdHTTP.Create(nil);
    try
      LHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      try
        lHTTP.IOHandler := LHandler;
        lHTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';
        Src := lHTTP.Get('https://api.github.com/repos/majid110/PersianTypeTutor/releases/latest');
        LatestVersion := ExtractVersion(Src);
      finally
        LHandler.Free;
      end;
    finally
      FreeAndNil(lHTTP);
    end;
  except
  end;
end;

end.
