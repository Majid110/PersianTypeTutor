unit UKeyboardHelper;

interface

uses
  System.Types,
  Winapi.Windows,
  UKeyboardKey;

type

  KeyRecord = record
    KeyCode: AnsiChar;
    ShiftedKeyCode: AnsiChar;
    KeyboardKey : TKeyboardKey;
  end;

  TKeyArray = array[0..256] of KeyRecord;

  TKeyboardHelper = class
  const
    FINGER_POS: array [1 .. 10] of TPoint = (
      (X: 482; Y: 73),
      (X: 457; Y: 104),
      (X: 452; Y: 138),
      (X: 458; Y: 173),
      (X: 0; Y: 0),
      (X: 507; Y: 319),
      (X: 458; Y: 361),
      (X: 452; Y: 396),
      (X: 457; Y: 431),
      (X: 482; Y: 462)
      );
  public
    class procedure FillKeyArray(KeyArray: TKeyArray; KafKeyNumber: Byte);
    class function GetCharacterCode(Ch: AnsiChar): byte;
  end;

implementation

{ TKeyboardHelper }

class procedure TKeyboardHelper.FillKeyArray(KeyArray: TKeyArray; KafKeyNumber: Byte);
var
  KeyNumber    : Integer;
  idx          : Integer;
  keyChar      : AnsiChar;
  keyboardState: TKeyboardState;
begin

  GetKeyboardState(keyboardState);

  // ............ Ignore Caps Lock state ............
  keyboardState[VK_CAPITAL] := 0;

  for KeyNumber := 0 to 256 do
  begin
    ToAscii(KeyNumber, MapVirtualKeyA(KeyNumber, 1), keyboardState, @keyChar, 0);

    KeyArray[KeyNumber].KeyCode := keyChar;

    if keyChar in ['ß', '˜'] then
        KafKeyNumber := KeyNumber;
  end;

  // ............... simulate Shift key is pressed ..............
  keyboardState[16] := 128;

  for KeyNumber := 0 to 256 do
  begin
    ToAscii(KeyNumber, MapVirtualKey(KeyNumber, 0), keyboardState, @keyChar, 0);
    KeyArray[KeyNumber].ShiftedKeyCode := keyChar;
  end;
end;

class function TKeyboardHelper.GetCharacterCode(Ch: AnsiChar): byte;
var
  i, lo, hi: Integer;

begin
  i := VkKeyScanA(Ch);
  lo := LoByte(i);
  hi := HiByte(i);

  if Ch = 'í' then
    hi := 0; //Ignore Shift for 'í'

  if (lo >= 96) and (lo <= 106) then
    lo := lo - 48;
  if hi = 1 then
    ShiftPress := True
  else
    begin
      ShiftPress := False;
      RestoreShiftKeyColor;
    end;

  if ch in ['ß', '˜'] then
    Arrayidx := Kaf_idx
  else
    Arrayidx := lo;

  Result := keyarray[Arrayidx].CompIndex;
end;

end.
