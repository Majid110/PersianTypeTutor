unit UKeyboard;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  UGlobalUnit,
  UKeyboardKey,
  UFingerCaption,
  UTutor,
  UObserver;

type

  KeyRecord = record
    KeyCode: AnsiChar;
    KeyComponentIndex: Byte;
    ShiftedKeyCode: AnsiChar;
    KeyboardKey: TKeyboardKey;
  end;

  TKeyArray = array [0 .. 256] of KeyRecord;

  TKeyboard = class(TFrame, IObserver)
    pnlKeyboard: TPanel;
    kk66: TKeyboardKey;
    kk78: TKeyboardKey;
    kk77: TKeyboardKey;
    kk188: TKeyboardKey;
    kk190: TKeyboardKey;
    kk191: TKeyboardKey;
    kk86: TKeyboardKey;
    kk67: TKeyboardKey;
    kk88: TKeyboardKey;
    kk90: TKeyboardKey;
    kkLeftShift: TKeyboardKey;
    kkLeftCtrl: TKeyboardKey;
    kkLeftWinKey: TKeyboardKey;
    kkLeftAlt: TKeyboardKey;
    kkRightAlt: TKeyboardKey;
    kkRightWinKey: TKeyboardKey;
    kkPopupMenu: TKeyboardKey;
    kkRightCtrl: TKeyboardKey;
    kkRightShift: TKeyboardKey;
    kk20: TKeyboardKey;
    kk65: TKeyboardKey;
    kk83: TKeyboardKey;
    kk68: TKeyboardKey;
    kk70: TKeyboardKey;
    kk71: TKeyboardKey;
    kk72: TKeyboardKey;
    kk74: TKeyboardKey;
    kk75: TKeyboardKey;
    kk76: TKeyboardKey;
    kk186: TKeyboardKey;
    kk81: TKeyboardKey;
    kk87: TKeyboardKey;
    kk69: TKeyboardKey;
    kk82: TKeyboardKey;
    kk84: TKeyboardKey;
    kk89: TKeyboardKey;
    kk85: TKeyboardKey;
    kk73: TKeyboardKey;
    kk79: TKeyboardKey;
    kk80: TKeyboardKey;
    kk219: TKeyboardKey;
    kk221: TKeyboardKey;
    kk222: TKeyboardKey;
    kk49: TKeyboardKey;
    kk50: TKeyboardKey;
    kk51: TKeyboardKey;
    kk52: TKeyboardKey;
    kk53: TKeyboardKey;
    kk54: TKeyboardKey;
    kk55: TKeyboardKey;
    kk56: TKeyboardKey;
    kk57: TKeyboardKey;
    kk48: TKeyboardKey;
    kk189: TKeyboardKey;
    kk187: TKeyboardKey;
    kkBackspace: TKeyboardKey;
    kk220: TKeyboardKey;
    kkEnter: TKeyboardKey;
    kkSpace: TKeyboardKey;
    kkTab: TKeyboardKey;
    kk192: TKeyboardKey;
  private
    KeyArray       : TKeyArray;
    ShiftPressed   : Boolean;
    KeyIndex       : Byte;
    FKeyboardLayout: TKeyboardLayout;
    procedure Update(Subject: TObject);
    procedure FillKeyArrayFromKeyboard;
    procedure SetKeyboardKeyCaptions;
    function GetCharacterCode(Ch: AnsiChar; var ShiftNeed: Boolean): Byte;
    procedure RestoreShiftKeyColor;
    procedure RestoreKeyColor;
    procedure SetKeyboardLayout(const Value: TKeyboardLayout);
    procedure FillArrayBasedOnKeyboardLayout;
  public
    procedure Init;
    procedure FillNextKey(Letter: Char);
    function CurrentKey: TKeyboardKey;
    function SheftNeeded: Boolean;
    function TranslateKey(Key: word; Shift: TShiftState): AnsiChar;
  published
    property KeyboardLayout: TKeyboardLayout read FKeyboardLayout write SetKeyboardLayout;

  const
    ACTIVE_KEY_CLR   = $008080FF;
    INACTIVE_KEY_CLR = clWhite;
    RICH_TEXT_COLOR  = Clyellow;
    CARET_COLOR      = Clyellow;
  end;

implementation

{$R *.dfm}

{ TKeyboard }

procedure TKeyboard.Init;
begin
  RestoreKeyColor;
  FillKeyArrayFromKeyboard;
  SetKeyboardKeyCaptions;
end;

function TKeyboard.SheftNeeded: Boolean;
begin
  Result := ShiftPressed;
end;

procedure TKeyboard.FillKeyArrayFromKeyboard;
var
  Idx: Integer;
  I  : Integer;
begin
  with pnlKeyboard do
  begin
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TKeyboardKey then
      begin
        Idx := (Controls[I] as TKeyboardKey).KeyNumber;
        KeyArray[Idx].KeyboardKey := (Controls[I] as TKeyboardKey);
        if Idx <> 0 then
          KeyArray[Idx].KeyComponentIndex := I;
      end;
  end;
end;

procedure TKeyboard.SetKeyboardKeyCaptions;
var
  Idx: Integer;
  I  : Integer;
begin
  for I := 0 to 256 do
  begin
    Idx := KeyArray[I].KeyComponentIndex;
    if (Idx <> 0) or ((Idx < 48) and (Idx > 57)) then
      (pnlKeyboard.Controls[Idx] as TKeyboardKey).Caption := UpperCase(KeyArray[I].KeyCode);
  end;
  // kk66.Caption := UpperCase(KeyArray[66].KeyCode);
end;

procedure TKeyboard.SetKeyboardLayout(const Value: TKeyboardLayout);
begin
  FKeyboardLayout := Value;
  FillArrayBasedOnKeyboardLayout;
end;

function TKeyboard.GetCharacterCode(Ch: AnsiChar; var ShiftNeed: Boolean): Byte;
var
  I: Integer;
begin
  if Ch = ' ' then
  begin
    ShiftNeed := False;
    Exit(32);
  end;

  for I := 0 to 255 do
    if (KeyArray[I].KeyCode = Ch) then
    begin
      ShiftNeed := False;
      Exit(I);
    end
    else if (KeyArray[I].ShiftedKeyCode = Ch) then
    begin
      ShiftNeed := True;
      Exit(I);
    end;
  Exit(0);
end;

procedure TKeyboard.RestoreShiftKeyColor;
begin
  kkLeftShift.Brush.Color := INACTIVE_KEY_CLR;
  kkRightShift.Brush.Color := INACTIVE_KEY_CLR;
end;

procedure TKeyboard.RestoreKeyColor;
begin
  if KeyArray[KeyIndex].KeyboardKey <> nil then
    KeyArray[KeyIndex].KeyboardKey.Brush.Color := INACTIVE_KEY_CLR;
  RestoreShiftKeyColor;
end;

function TKeyboard.CurrentKey: TKeyboardKey;
begin
  Result := KeyArray[KeyIndex].KeyboardKey;
end;

function TKeyboard.TranslateKey(Key: word; Shift: TShiftState): AnsiChar;
var
  S: Ansistring;
begin
  if (Key >= 48) and (Key <= 57) and not(ssShift in Shift) then
  begin
    S := IntToStr(Key - 48);
    Result := S[1];
  end
  else
  begin
    if ssShift in Shift then
      Result := KeyArray[Key].ShiftedKeyCode
    else
      Result := KeyArray[Key].KeyCode;
  end;
end;

procedure TKeyboard.FillArrayBasedOnKeyboardLayout;
begin
  // space key
  KeyArray[32].KeyCode := ' ';
  KeyArray[32].ShiftedKeyCode := #$200C;
  KeyArray[32].KeyboardKey := kkSpace;

  case FKeyboardLayout of
    klEnglish:
      begin
        KeyArray[90].KeyCode := 'z';
        KeyArray[90].ShiftedKeyCode := 'Z';

        KeyArray[88].KeyCode := 'x';
        KeyArray[88].ShiftedKeyCode := 'X';

        KeyArray[67].KeyCode := 'c';
        KeyArray[67].ShiftedKeyCode := 'C';

        KeyArray[86].KeyCode := 'v';
        KeyArray[86].ShiftedKeyCode := 'V';

        KeyArray[66].KeyCode := 'b';
        KeyArray[66].ShiftedKeyCode := 'B';

        KeyArray[78].KeyCode := 'n';
        KeyArray[78].ShiftedKeyCode := 'N';

        KeyArray[77].KeyCode := 'm';
        KeyArray[77].ShiftedKeyCode := 'M';

        KeyArray[188].KeyCode := ',';
        KeyArray[188].ShiftedKeyCode := '<';

        KeyArray[190].KeyCode := '.';
        KeyArray[190].ShiftedKeyCode := '>';

        KeyArray[191].KeyCode := '/';
        KeyArray[191].ShiftedKeyCode := '?';

        KeyArray[65].KeyCode := 'a';
        KeyArray[65].ShiftedKeyCode := 'A';

        KeyArray[83].KeyCode := 's';
        KeyArray[83].ShiftedKeyCode := 'S';

        KeyArray[68].KeyCode := 'd';
        KeyArray[68].ShiftedKeyCode := 'D';

        KeyArray[70].KeyCode := 'f';
        KeyArray[70].ShiftedKeyCode := 'F';

        KeyArray[71].KeyCode := 'g';
        KeyArray[71].ShiftedKeyCode := 'G';

        KeyArray[72].KeyCode := 'h';
        KeyArray[72].ShiftedKeyCode := 'H';

        KeyArray[74].KeyCode := 'j';
        KeyArray[74].ShiftedKeyCode := 'J';

        KeyArray[75].KeyCode := 'k';
        KeyArray[75].ShiftedKeyCode := 'K';

        KeyArray[76].KeyCode := 'l';
        KeyArray[76].ShiftedKeyCode := 'L';

        KeyArray[186].KeyCode := ';';
        KeyArray[186].ShiftedKeyCode := ':';

        KeyArray[222].KeyCode := '''';
        KeyArray[222].ShiftedKeyCode := '"';

        KeyArray[81].KeyCode := 'q';
        KeyArray[81].ShiftedKeyCode := 'Q';

        KeyArray[87].KeyCode := 'w';
        KeyArray[87].ShiftedKeyCode := 'W';

        KeyArray[69].KeyCode := 'e';
        KeyArray[69].ShiftedKeyCode := 'E';

        KeyArray[82].KeyCode := 'r';
        KeyArray[82].ShiftedKeyCode := 'R';

        KeyArray[84].KeyCode := 't';
        KeyArray[84].ShiftedKeyCode := 'T';

        KeyArray[89].KeyCode := 'y';
        KeyArray[89].ShiftedKeyCode := 'Y';

        KeyArray[85].KeyCode := 'u';
        KeyArray[85].ShiftedKeyCode := 'U';

        KeyArray[73].KeyCode := 'i';
        KeyArray[73].ShiftedKeyCode := 'I';

        KeyArray[79].KeyCode := 'o';
        KeyArray[79].ShiftedKeyCode := 'O';

        KeyArray[80].KeyCode := 'p';
        KeyArray[80].ShiftedKeyCode := 'P';

        KeyArray[219].KeyCode := '[';
        KeyArray[219].ShiftedKeyCode := '{';

        KeyArray[221].KeyCode := ']';
        KeyArray[221].ShiftedKeyCode := '}';

        KeyArray[192].KeyCode := '`';
        KeyArray[192].ShiftedKeyCode := '~';

        KeyArray[49].KeyCode := '1';
        KeyArray[49].ShiftedKeyCode := '!';

        KeyArray[50].KeyCode := '2';
        KeyArray[50].ShiftedKeyCode := '@';

        KeyArray[51].KeyCode := '3';
        KeyArray[51].ShiftedKeyCode := '#';

        KeyArray[52].KeyCode := '4';
        KeyArray[52].ShiftedKeyCode := '$';

        KeyArray[53].KeyCode := '5';
        KeyArray[53].ShiftedKeyCode := '%';

        KeyArray[54].KeyCode := '6';
        KeyArray[54].ShiftedKeyCode := '^';

        KeyArray[55].KeyCode := '7';
        KeyArray[55].ShiftedKeyCode := '&';

        KeyArray[56].KeyCode := '8';
        KeyArray[56].ShiftedKeyCode := '*';

        KeyArray[57].KeyCode := '9';
        KeyArray[57].ShiftedKeyCode := '(';

        KeyArray[48].KeyCode := '0';
        KeyArray[48].ShiftedKeyCode := ')';

        KeyArray[189].KeyCode := '-';
        KeyArray[189].ShiftedKeyCode := '_';

        KeyArray[187].KeyCode := '=';
        KeyArray[187].ShiftedKeyCode := '+';

        KeyArray[220].KeyCode := '\';
        KeyArray[220].ShiftedKeyCode := '|';
      end;
    klMicrosoftPersian:
      begin
        KeyArray[90].KeyCode := 'ظ';
        KeyArray[90].ShiftedKeyCode := 'ة';

        KeyArray[88].KeyCode := 'ط';
        KeyArray[88].ShiftedKeyCode := 'ي';

        KeyArray[67].KeyCode := 'ز';
        KeyArray[67].ShiftedKeyCode := 'ژ';

        KeyArray[86].KeyCode := 'ر';
        KeyArray[86].ShiftedKeyCode := 'ؤ';

        KeyArray[66].KeyCode := 'ذ';
        KeyArray[66].ShiftedKeyCode := 'إ';

        KeyArray[78].KeyCode := 'د';
        KeyArray[78].ShiftedKeyCode := 'أ';

        KeyArray[77].KeyCode := 'ئ';
        KeyArray[77].ShiftedKeyCode := 'ء';

        KeyArray[188].KeyCode := 'و';
        KeyArray[188].ShiftedKeyCode := '<';

        KeyArray[190].KeyCode := '.';
        KeyArray[190].ShiftedKeyCode := '>';

        KeyArray[191].KeyCode := '/';
        KeyArray[191].ShiftedKeyCode := '؟';

        KeyArray[65].KeyCode := 'ش';
        KeyArray[65].ShiftedKeyCode := 'َ';

        KeyArray[83].KeyCode := 'س';
        KeyArray[83].ShiftedKeyCode := 'ُ';

        KeyArray[68].KeyCode := 'ی';
        KeyArray[68].ShiftedKeyCode := 'ِ';

        KeyArray[70].KeyCode := 'ب';
        KeyArray[70].ShiftedKeyCode := 'ّ';

        KeyArray[71].KeyCode := 'ل';
        KeyArray[71].ShiftedKeyCode := 'ۀ';

        KeyArray[72].KeyCode := 'ا';
        KeyArray[72].ShiftedKeyCode := 'آ';

        KeyArray[74].KeyCode := 'ت';
        KeyArray[74].ShiftedKeyCode := 'ـ';

        KeyArray[75].KeyCode := 'ن';
        KeyArray[75].ShiftedKeyCode := '«';

        KeyArray[76].KeyCode := 'م';
        KeyArray[76].ShiftedKeyCode := '»';

        KeyArray[186].KeyCode := 'ک';
        KeyArray[186].ShiftedKeyCode := ':';

        KeyArray[222].KeyCode := 'گ';
        KeyArray[222].ShiftedKeyCode := '"';

        KeyArray[81].KeyCode := 'ض';
        KeyArray[81].ShiftedKeyCode := 'ً';

        KeyArray[87].KeyCode := 'ص';
        KeyArray[87].ShiftedKeyCode := 'ٌ';

        KeyArray[69].KeyCode := 'ث';
        KeyArray[69].ShiftedKeyCode := 'ٍ';

        KeyArray[82].KeyCode := 'ق';
        // KeyArray[82].ShiftedKeyCode := '';

        KeyArray[84].KeyCode := 'ف';
        KeyArray[84].ShiftedKeyCode := '،';

        KeyArray[89].KeyCode := 'غ';
        KeyArray[89].ShiftedKeyCode := '؛';

        KeyArray[85].KeyCode := 'ع';
        KeyArray[85].ShiftedKeyCode := ',';

        KeyArray[73].KeyCode := 'ه';
        KeyArray[73].ShiftedKeyCode := ']';

        KeyArray[79].KeyCode := 'خ';
        KeyArray[79].ShiftedKeyCode := '[';

        KeyArray[80].KeyCode := 'ح';
        KeyArray[80].ShiftedKeyCode := '\';

        KeyArray[219].KeyCode := 'ج';
        KeyArray[219].ShiftedKeyCode := '}';

        KeyArray[221].KeyCode := 'چ';
        KeyArray[221].ShiftedKeyCode := '{';

        KeyArray[192].KeyCode := '÷';
        KeyArray[192].ShiftedKeyCode := '×';

        KeyArray[49].KeyCode := '1';
        KeyArray[49].ShiftedKeyCode := '!';

        KeyArray[50].KeyCode := '2';
        KeyArray[50].ShiftedKeyCode := '@';

        KeyArray[51].KeyCode := '3';
        KeyArray[51].ShiftedKeyCode := '#';

        KeyArray[52].KeyCode := '4';
        KeyArray[52].ShiftedKeyCode := '$';

        KeyArray[53].KeyCode := '5';
        KeyArray[53].ShiftedKeyCode := '%';

        KeyArray[54].KeyCode := '6';
        KeyArray[54].ShiftedKeyCode := '^';

        KeyArray[55].KeyCode := '7';
        KeyArray[55].ShiftedKeyCode := '&';

        KeyArray[56].KeyCode := '8';
        KeyArray[56].ShiftedKeyCode := '*';

        KeyArray[57].KeyCode := '9';
        KeyArray[57].ShiftedKeyCode := ')';

        KeyArray[48].KeyCode := '0';
        KeyArray[48].ShiftedKeyCode := '(';

        KeyArray[189].KeyCode := '-';
        KeyArray[189].ShiftedKeyCode := '_';

        KeyArray[187].KeyCode := '=';
        KeyArray[187].ShiftedKeyCode := '+';

        KeyArray[220].KeyCode := 'پ';
        KeyArray[220].ShiftedKeyCode := '|';
      end;
    klPersian:
      begin
        KeyArray[90].KeyCode := 'ظ';
        KeyArray[90].ShiftedKeyCode := 'ك';

        KeyArray[88].KeyCode := 'ط';
        KeyArray[88].ShiftedKeyCode := 'ٓ';

        KeyArray[67].KeyCode := 'ز';
        KeyArray[67].ShiftedKeyCode := 'ژ';

        KeyArray[86].KeyCode := 'ر';
        KeyArray[86].ShiftedKeyCode := 'ٰ';

        KeyArray[66].KeyCode := 'ذ';
        KeyArray[66].ShiftedKeyCode := '‌';

        KeyArray[78].KeyCode := 'د';
        KeyArray[78].ShiftedKeyCode := 'ٔ';

        KeyArray[77].KeyCode := 'پ';
        KeyArray[77].ShiftedKeyCode := 'ء';

        KeyArray[188].KeyCode := 'و';
        KeyArray[188].ShiftedKeyCode := '>';

        KeyArray[190].KeyCode := '.';
        KeyArray[190].ShiftedKeyCode := '<';

        KeyArray[191].KeyCode := '/';
        KeyArray[191].ShiftedKeyCode := '؟';

        KeyArray[65].KeyCode := 'ش';
        KeyArray[65].ShiftedKeyCode := 'ؤ';

        KeyArray[83].KeyCode := 'س';
        KeyArray[83].ShiftedKeyCode := 'ئ';

        KeyArray[68].KeyCode := 'ی';
        KeyArray[68].ShiftedKeyCode := 'ي';

        KeyArray[70].KeyCode := 'ب';
        KeyArray[70].ShiftedKeyCode := 'إ';

        KeyArray[71].KeyCode := 'ل';
        KeyArray[71].ShiftedKeyCode := 'أ';

        KeyArray[72].KeyCode := 'ا';
        KeyArray[72].ShiftedKeyCode := 'آ';

        KeyArray[74].KeyCode := 'ت';
        KeyArray[74].ShiftedKeyCode := 'ة';

        KeyArray[75].KeyCode := 'ن';
        KeyArray[75].ShiftedKeyCode := '»';

        KeyArray[76].KeyCode := 'م';
        KeyArray[76].ShiftedKeyCode := '«';

        KeyArray[186].KeyCode := 'ک';
        KeyArray[186].ShiftedKeyCode := ':';

        KeyArray[222].KeyCode := 'گ';
        KeyArray[222].ShiftedKeyCode := '؛';

        KeyArray[81].KeyCode := 'ض';
        KeyArray[81].ShiftedKeyCode := 'ْ';

        KeyArray[87].KeyCode := 'ص';
        KeyArray[87].ShiftedKeyCode := 'ٌ';

        KeyArray[69].KeyCode := 'ث';
        KeyArray[69].ShiftedKeyCode := 'ٍ';

        KeyArray[82].KeyCode := 'ق';
        KeyArray[82].ShiftedKeyCode := 'ً';

        KeyArray[84].KeyCode := 'ف';
        KeyArray[84].ShiftedKeyCode := 'ُ';

        KeyArray[89].KeyCode := 'غ';
        KeyArray[89].ShiftedKeyCode := 'ِ';

        KeyArray[85].KeyCode := 'ع';
        KeyArray[85].ShiftedKeyCode := 'َ';

        KeyArray[73].KeyCode := 'ه';
        KeyArray[73].ShiftedKeyCode := 'ّ';

        KeyArray[79].KeyCode := 'خ';
        KeyArray[79].ShiftedKeyCode := ']';

        KeyArray[80].KeyCode := 'ح';
        KeyArray[80].ShiftedKeyCode := '[';

        KeyArray[219].KeyCode := 'ج';
        KeyArray[219].ShiftedKeyCode := '}';

        KeyArray[221].KeyCode := 'چ';
        KeyArray[221].ShiftedKeyCode := '{';

        KeyArray[192].KeyCode := '‍';
        KeyArray[192].ShiftedKeyCode := '÷';

        KeyArray[49].KeyCode := '1';
        KeyArray[49].ShiftedKeyCode := '!';

        KeyArray[50].KeyCode := '2';
        KeyArray[50].ShiftedKeyCode := '٬';

        KeyArray[51].KeyCode := '3';
        KeyArray[51].ShiftedKeyCode := '٫';

        KeyArray[52].KeyCode := '4';
        KeyArray[52].ShiftedKeyCode := '﷼';

        KeyArray[53].KeyCode := '5';
        KeyArray[53].ShiftedKeyCode := '٪';

        KeyArray[54].KeyCode := '6';
        KeyArray[54].ShiftedKeyCode := '×';

        KeyArray[55].KeyCode := '7';
        KeyArray[55].ShiftedKeyCode := '،';

        KeyArray[56].KeyCode := '8';
        KeyArray[56].ShiftedKeyCode := '*';

        KeyArray[57].KeyCode := '9';
        KeyArray[57].ShiftedKeyCode := ')';

        KeyArray[48].KeyCode := '0';
        KeyArray[48].ShiftedKeyCode := '(';

        KeyArray[189].KeyCode := '-';
        KeyArray[189].ShiftedKeyCode := 'ـ';

        KeyArray[187].KeyCode := '=';
        KeyArray[187].ShiftedKeyCode := '+';

        KeyArray[220].KeyCode := '\';
        KeyArray[220].ShiftedKeyCode := '|';
      end;
  end;
end;

procedure TKeyboard.Update(Subject: TObject);
begin
  KeyboardLayout := TTutor(Subject).KeyboardLayout;
  Init;
end;

procedure TKeyboard.FillNextKey(Letter: Char);
var
  AnsiChr: Ansistring;
begin
  RestoreKeyColor;

  if Letter = VIRTUAL_SPACE then // Correct virtual space
  begin
    KeyIndex := 32;
    ShiftPressed := KeyboardLayout = klPersian;
  end
  else
  begin
    AnsiChr := Letter;
    KeyIndex := GetCharacterCode(AnsiChr[1], ShiftPressed);
  end;

  if KeyIndex = 0 then
    Exit;

  if Letter = NULL_CHAR then
    Exit;

  if KeyArray[KeyIndex].KeyboardKey = nil then
    Exit;
  KeyArray[KeyIndex].KeyboardKey.Brush.Color := ACTIVE_KEY_CLR;

  if ShiftPressed then
    if KeyArray[KeyIndex].KeyboardKey.FingerNo in [1 .. 5] then
      kkRightShift.Brush.Color := ACTIVE_KEY_CLR
    else
      kkLeftShift.Brush.Color := ACTIVE_KEY_CLR;
end;

end.
