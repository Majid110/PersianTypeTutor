unit UGlobalUnit;

interface

uses
  System.SysUtils,
  Vcl.Forms;

type
  //TTypeLanguage = (tlPersian, tlEnglish);
  TTutorState = (tsNon, tsReady, tsStarted, tsFinished);
  TKeyboardLayout = (klEnglish, klMicrosoftPersian, klPersian);
const
  NULL_CHAR = #13;
  TUTOR_EXT = '.ttd';
  NULL_CHAR_INDEX = -1;
  APPLICATION_VERSION = '1.7';
  VIRTUAL_SPACE = #$200C;

function ApplicationPath: String;

implementation

function ApplicationPath: String;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

end.
