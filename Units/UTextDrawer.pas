unit UTextDrawer;

interface

uses
  Winapi.Windows,
  Vcl.ExtCtrls,
  Vcl.Controls,
  System.Classes,
  Vcl.Graphics,
  System.Math,
  Winapi.Messages,
  System.SysUtils,
  UGlobalUnit,
  UTutor,
  UObserver;

type

  PIntArray = array of Integer;
  PwcArray  = array of widechar;

  WideCharInfo = Record
    lpDx: PIntArray;
    lpGlyphs: PwcArray;
    lpOrder: PIntArray;
  End;

  TTextDrawer = class(TPaintBox, IObserver)
  private
    FTextLines       : TStrings;
    FCurrentChar     : Char;
    FCurrentLineIndex: Integer;
    fCurrentCharIndex: Integer;
    FStartLineIndex  : Integer;
    FFinishLineIndex : Integer;
    FRightIndent     : Integer;
    FTopIndent       : Integer;
    FLineHeight      : Integer;
    FTextLineCharInfo: array of WideCharInfo;
    FCaretPosition   : TPoint;
    FShowCaret       : Boolean;
    FCaretBitmap     : TBitmap;
    FCaretTimer      : TTimer;
    FTutorState      : TTutorState;
    FKeyboardLayout  : TKeyboardLayout;
    procedure DrawVisibleTextLine;
    function GetVisibleLineCount: Integer;
    procedure SetRightIndent(const Value: Integer);
    procedure SetTopIndent(const Value: Integer);
    procedure UpdateLineIndexes;
    procedure DrawPersianText(Canvas: TCanvas; sText: string; LineIndex: Integer; yPos: Integer);
    procedure DrawEnglishText(Canvas: TCanvas; sText: string; LineIndex: Integer; yPos: Integer);
    procedure ChangeCaretPosition;
    procedure TimerCaretTimer(Sender: TObject);
    procedure GoCaretNextLine(CurrentLineIndex: Integer);
    procedure GetNextCharIndex(var NextLineIndex, NextCharIndex: Integer);
    procedure Update(Subject: TObject);
    procedure SetKeyboardLayout(const Value: TKeyboardLayout);
  protected
    procedure Paint; override;
    procedure FONTCHANGED(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTextLine(Text: string);
    procedure FillNextChar;
    function CurrentChar: Char;
    function NextChar: Char;
    procedure Init;
    procedure Reset;
    function TutorState: TTutorState;

  published
    property RightIndent   : Integer read FRightIndent write SetRightIndent;
    property TopIndent     : Integer read FTopIndent write SetTopIndent;
    property KeyboardLayout: TKeyboardLayout read FKeyboardLayout write SetKeyboardLayout;

  const
    CARET_COLOR = ClYellow;
    CARET_WIDTH = 1;
  end;

  TTextDrawHelper = class
  const
    ALLAH_LA_TEXT    = '«··Â ·”·„«‰Â';
    ALLAH_INDEX      = 1;
    FIRST_LAM_INDEX  = 5;
    MIDDLE_LAM_INDEX = 7;
    END_ALEF_INDEX   = 9;
    END_HA_INDEX     = 11;
  private
  public
    class function GetCharacterPlace(DC: HDC; sText: WideString): WideCharInfo;
    class function GetALLH_LACharInfo(DC: HDC): WideCharInfo;
    class function GetFreeGlyphArrayIndex(GlyphArray: PwcArray): Integer;
  end;

implementation

{ TextDrawer }

constructor TTextDrawer.Create(AOwner: TComponent);
begin
  inherited;
  FTextLines := TStringList.Create;
  FKeyboardLayout := klPersian;
  FTutorState := tsNon;

  FCaretBitmap := TBitmap.Create;
  FCaretBitmap.Height := 2;

  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Interval := 250;
  FCaretTimer.OnTimer := TimerCaretTimer;

  Reset;
end;

function TTextDrawer.CurrentChar: Char;
begin
  Result := FCurrentChar;
end;

destructor TTextDrawer.Destroy;
begin
  FCaretTimer.Free;
  FCaretBitmap.Free;
  FTextLines.Free;
  inherited;
end;

procedure TTextDrawer.DrawPersianText(Canvas: TCanvas; sText: string; LineIndex: Integer; yPos: Integer);
const
  FIRST_LAM  = #$00b7;
  MIDDLE_LAM = #$00b8;
  ALEF       = #$005a;
  FIRST_LA   = #$00d3;
  MIDDLE_LA  = #$00d4;
  ALLAH      = #$00d7;
  END_HA     = #$00C4;

var
  aRect     : TRect;
  CharInfo  : WideCharInfo;
  LACharInfo: WideCharInfo;
  Options   : LongInt;
  wChar     : widechar;
  I         : Integer;
  xPos      : Integer;
  lehWidth  : Integer;
begin

  if LineIndex < FCurrentLineIndex then
    Canvas.Font.Color := ClYellow
  else
    Canvas.Font.Color := clBlack;

  aRect := Canvas.ClipRect;
  CharInfo := FTextLineCharInfo[LineIndex];

  xPos := aRect.Right - FRightIndent;
  Options := ETO_GLYPH_INDEX;

  I := 0;
  while I < Length(sText) do
  begin

    if LineIndex = FCurrentLineIndex then
      if I < fCurrentCharIndex then
        Canvas.Font.Color := ClYellow
      else
        Canvas.Font.Color := clBlack;

    wChar := CharInfo.lpGlyphs[CharInfo.lpOrder[I]];

    if (wChar = MIDDLE_LA) or (wChar = FIRST_LA) or (wChar = ALLAH) then
    begin
      LACharInfo := TTextDrawHelper.GetALLH_LACharInfo(Canvas.Handle);
      if wChar = ALLAH then
      begin

        CharInfo.lpOrder[I + 1] := TTextDrawHelper.GetFreeGlyphArrayIndex(CharInfo.lpGlyphs);
        CharInfo.lpOrder[I + 2] := CharInfo.lpOrder[I + 1] - 1;

        wChar := FIRST_LAM;
        CharInfo.lpGlyphs[CharInfo.lpOrder[I]] := FIRST_LAM;
        CharInfo.lpDx[CharInfo.lpOrder[I]] := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.FIRST_LAM_INDEX]];

        CharInfo.lpGlyphs[CharInfo.lpOrder[I + 1]] := MIDDLE_LAM;
        CharInfo.lpDx[CharInfo.lpOrder[I + 1]] := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.MIDDLE_LAM_INDEX]];

        CharInfo.lpGlyphs[CharInfo.lpOrder[I + 2]] := END_HA;
        CharInfo.lpDx[CharInfo.lpOrder[I + 2]] := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.END_HA_INDEX]];
      end
      else
      begin
        if wChar = FIRST_LA then
        begin
          wChar := FIRST_LAM;
          lehWidth := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.FIRST_LAM_INDEX]]
        end
        else
        begin
          wChar := MIDDLE_LAM;
          lehWidth := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.MIDDLE_LAM_INDEX]]
        end;

        CharInfo.lpGlyphs[CharInfo.lpOrder[I]] := wChar;
        CharInfo.lpDx[CharInfo.lpOrder[I]] := lehWidth;

        CharInfo.lpOrder[I + 1] := TTextDrawHelper.GetFreeGlyphArrayIndex(CharInfo.lpGlyphs);
        CharInfo.lpGlyphs[CharInfo.lpOrder[I + 1]] := ALEF;
        CharInfo.lpDx[CharInfo.lpOrder[I + 1]] := LACharInfo.lpDx[LACharInfo.lpOrder[TTextDrawHelper.END_ALEF_INDEX]];
      end;
    end;

    xPos := xPos - CharInfo.lpDx[CharInfo.lpOrder[I]];
    ExtTextOutW(Canvas.Handle, xPos, yPos, Options, @aRect, @wChar, 1, nil);
    Inc(I);
  end;
end;

procedure TTextDrawer.DrawEnglishText(Canvas: TCanvas; sText: string; LineIndex: Integer; yPos: Integer);
var
  aRect: TRect;
  I    : Integer;
  xPos : Integer;
begin

  if LineIndex < FCurrentLineIndex then
    Canvas.Font.Color := ClYellow
  else
    Canvas.Font.Color := clBlack;

  Canvas.Brush.Style := bsClear;
  // SetBkMode(Canvas.Handle, TRANSPARENT);

  aRect := Canvas.ClipRect;

  xPos := aRect.Left;

  I := 1;
  while I <= Length(sText) do
  begin

    if LineIndex = FCurrentLineIndex then
      if I <= fCurrentCharIndex then
        Canvas.Font.Color := ClYellow
      else
        Canvas.Font.Color := clBlack;

    Canvas.TextOut(xPos, yPos, sText[I]);
    xPos := xPos + Canvas.TextWidth(sText[I]);
    Inc(I);
  end;
end;

procedure TTextDrawer.DrawVisibleTextLine;
var
  I      : Integer;
  yPos   : Integer;
  aRect  : TRect;
  aBitMap: TBitmap;
begin
  Canvas.lock;
  try
    aRect := Rect(
      Canvas.ClipRect.Left + FRightIndent,
      Canvas.ClipRect.Top + FTopIndent,
      Canvas.ClipRect.Right - FTopIndent,
      Canvas.ClipRect.Bottom - FTopIndent
    );

    aBitMap := TBitmap.Create;
    try
      aBitMap.Canvas.lock;
      try
        aBitMap.Canvas.Brush.Color := Color;
        aBitMap.Height := aRect.Height;
        aBitMap.Width := aRect.Width;
        // aBitMap.Canvas.Brush := Canvas.Brush;
        aBitMap.Canvas.Font := Canvas.Font;
        aBitMap.Canvas.Brush.Style := bsClear;

        yPos := aRect.Top;

        for I := FStartLineIndex to FFinishLineIndex do
        Begin
          case FKeyboardLayout of
            klEnglish: DrawEnglishText(aBitMap.Canvas, FTextLines[I - 1], I - 1, yPos)
          else
            DrawPersianText(aBitMap.Canvas, FTextLines[I - 1], I - 1, yPos);
          end;
          yPos := yPos + FLineHeight;
        End;
        Canvas.CopyRect(aRect, aBitMap.Canvas, aBitMap.Canvas.ClipRect);
        // Canvas.Draw(0, 0, aBitMap);
      finally
        aBitMap.Canvas.Unlock;
      end;
    finally
      aBitMap.Free;
    end;
  finally
    Canvas.Unlock;
  end;
end;

function TTextDrawer.TutorState: TTutorState;
begin
  Result := FTutorState;
end;

procedure TTextDrawer.GetNextCharIndex(var NextLineIndex, NextCharIndex: Integer);
begin
  NextLineIndex := FCurrentLineIndex;
  NextCharIndex := fCurrentCharIndex;
  if fCurrentCharIndex = Length(FTextLines[FCurrentLineIndex]) then
  begin
    if FCurrentLineIndex = FTextLines.Count - 1 then
      NextCharIndex := NULL_CHAR_INDEX
    else
    begin
      NextLineIndex := FCurrentLineIndex + 1;
      NextCharIndex := IfThen(FKeyboardLayout = klEnglish, 0, 1);
    end;
  end
  else
    NextCharIndex := fCurrentCharIndex + 1;
end;

procedure TTextDrawer.FillNextChar;
begin
  if FTutorState = tsFinished then
    Exit;

  if (FCurrentLineIndex = FTextLines.Count - 1) and
  (fCurrentCharIndex = Length(FTextLines[FCurrentLineIndex]) - 1) then
  begin
    FCaretTimer.Enabled := False;
    FTutorState := tsFinished;
  end;

  if fCurrentCharIndex = Length(FTextLines[FCurrentLineIndex]) - 1 then
  begin
    if FCurrentLineIndex = FFinishLineIndex - 1 then
      Inc(FStartLineIndex);
    if FTutorState = tsFinished then
      Inc(fCurrentCharIndex)
    else
    begin
      fCurrentCharIndex := 0;
      Inc(FCurrentLineIndex);
    end;
    UpdateLineIndexes;
    GoCaretNextLine(FCurrentLineIndex);
  end
  else
    Inc(fCurrentCharIndex);

  FCurrentChar := FTextLines[FCurrentLineIndex][fCurrentCharIndex];

  ChangeCaretPosition;
  DrawVisibleTextLine;
end;

procedure TTextDrawer.FONTCHANGED(var Message: TMessage);
begin
  Canvas.Font := Font;
end;

function TTextDrawer.GetVisibleLineCount: Integer;
begin
  Result := (Height - FTopIndent) Div FLineHeight;
end;

procedure TTextDrawer.Init;
begin
  if Parent = nil then
    Exit;
  FTutorState := tsReady;

  case FKeyboardLayout of
    klEnglish: FLineHeight := Canvas.TextHeight('Ay')
  else
    FLineHeight := Canvas.TextHeight('¬Õ')
  end;

  FCaretPosition.Y := 0;
  GoCaretNextLine(FCurrentLineIndex);

  UpdateLineIndexes;
  DrawVisibleTextLine;
  ChangeCaretPosition;
  FCaretTimer.Enabled := true;

  FCurrentChar := FTextLines[FCurrentLineIndex][fCurrentCharIndex];
end;

function TTextDrawer.NextChar: Char;
var
  NextLineIndex: Integer;
  NextCharIndex: Integer;
begin
  if TutorState = tsFinished then
    Exit(NULL_CHAR);

  GetNextCharIndex(NextLineIndex, NextCharIndex);
  if NextCharIndex = NULL_CHAR_INDEX then
    Result := NULL_CHAR
  else
    Result := FTextLines[NextLineIndex][NextCharIndex];
end;

procedure TTextDrawer.Paint;
begin
  inherited;
  if TutorState = tsReady then
    DrawVisibleTextLine;
end;

procedure TTextDrawer.Reset;
begin
  FTextLines.Clear;
  FCurrentLineIndex := 0;
  fCurrentCharIndex := 0;
  FStartLineIndex := 1;
  FFinishLineIndex := 1;
  SetLength(FTextLineCharInfo, 0);
  FCaretTimer.Enabled := False;
  FCaretPosition.Y := 0;
  GoCaretNextLine(0);
  FTutorState := tsFinished;
  Repaint;
end;

procedure TTextDrawer.SetKeyboardLayout(const Value: TKeyboardLayout);
begin
  FKeyboardLayout := Value;
end;

procedure TTextDrawer.SetRightIndent(const Value: Integer);
begin
  FRightIndent := Value;
end;

procedure TTextDrawer.AddTextLine(Text: String);
var
  Txt: string;
begin
  Txt := Trim(Text);
  FTextLines.Add(Txt);
  SetLength(FTextLineCharInfo, Length(FTextLineCharInfo) + 1);
  FTextLineCharInfo[High(FTextLineCharInfo)] := TTextDrawHelper.GetCharacterPlace(Canvas.Handle, Txt);
end;

procedure TTextDrawer.SetTopIndent(const Value: Integer);
begin
  FTopIndent := Value;
end;

procedure TTextDrawer.Update(Subject: TObject);
var
  I     : Integer;
  Tutors: TStrings;
begin
  Reset;

  case TTutor(Subject).KeyboardLayout of
    klEnglish:
      begin
        Font.Name := 'Arial';
        Font.Size := 21;
        Font.Style := [];
      end
  else
    begin
      Font.Name := 'B Nazanin';
      Font.Size := 18;
      Font.Style := [fsbold];
    end;
  end;
  KeyboardLayout := TTutor(Subject).KeyboardLayout;

  Tutors := TTutor(Subject).CurrentTutor;
  for I := 1 to Tutors.Count - 1 do
    AddTextLine(Tutors[I]);
  FTutorState := tsReady;
  Init;
end;

procedure TTextDrawer.UpdateLineIndexes;
var
  VisibleLineCount: Integer;
begin
  VisibleLineCount := GetVisibleLineCount;
  FFinishLineIndex := IfThen(FStartLineIndex + VisibleLineCount <= FTextLines.Count,
    FStartLineIndex + VisibleLineCount - 1, FTextLines.Count);
end;

procedure TTextDrawer.ChangeCaretPosition;
var
  CharacterWide: Integer;
  NextLineIndex: Integer;
  NextCharIndex: Integer;

begin
  CharacterWide := 0;
  GetNextCharIndex(NextLineIndex, NextCharIndex);
  if NextCharIndex = NULL_CHAR_INDEX then
    Exit;

  if NextLineIndex = FCurrentLineIndex + 1 then
    GoCaretNextLine(FCurrentLineIndex + 1);

  case FKeyboardLayout of
    klEnglish:
      CharacterWide := Canvas.TextWidth(FTextLines[NextLineIndex][NextCharIndex])
  else
    CharacterWide := FTextLineCharInfo[NextLineIndex]
    .lpDx[FTextLineCharInfo[NextLineIndex].lpOrder[NextCharIndex - 1]];
  end;

  FCaretPosition.X := FCaretPosition.X + CharacterWide;
  FCaretBitmap.Width := CharacterWide;
end;

procedure TTextDrawer.TimerCaretTimer(Sender: TObject);
var
  Colour: TColor;
begin
  FShowCaret := not FShowCaret;

  FCaretBitmap.Canvas.lock;
  try
    if FShowCaret then
      Colour := Color
    else
      Colour := CARET_COLOR;

    FCaretBitmap.Canvas.Brush.Color := Colour;

    FCaretBitmap.Canvas.FloodFill(0, 0, Colour, fsBorder);
  finally
    FCaretBitmap.Canvas.Unlock;
  end;

  case FKeyboardLayout of
    klEnglish: Canvas.Draw(ClientRect.Left + FCaretPosition.X - FCaretBitmap.Width,
        FCaretPosition.Y, FCaretBitmap)
  else
    Canvas.Draw(ClientRect.Right - FCaretPosition.X,
      FCaretPosition.Y, FCaretBitmap);
  end;
end;

procedure TTextDrawer.GoCaretNextLine(CurrentLineIndex: Integer);
begin
  FCaretPosition.X := FRightIndent;
  FCaretPosition.Y := ((CurrentLineIndex + 2 - FStartLineIndex) * FLineHeight);
end;

{ TTextDrawHelper }

class
function TTextDrawHelper.GetCharacterPlace(DC: HDC;
  sText:
  WideString): WideCharInfo;
var
  GCP       : TGCPResults;
  TextLength: Integer;
begin

  TextLength := Length(sText);

  SetLength(Result.lpOrder, TextLength);
  SetLength(Result.lpDx, TextLength);
  SetLength(Result.lpGlyphs, TextLength);

  GCP.lStructSize := sizeof(TGCPResults);
  GCP.lpOutString := nil; // pointer(buffer);
  GCP.lpOrder := Pointer(Result.lpOrder);
  GCP.lpDx := Pointer(Result.lpDx);
  GCP.lpGlyphs := Pointer(Result.lpGlyphs);
  GCP.lpCaretPos := nil;
  GCP.lpClass := nil;
  GCP.nGlyphs := TextLength;
  GCP.nMaxFit := 0;

  GetCharacterPlacementW(DC, Pointer(sText), TextLength, 0,
    GCP, GetFontLanguageInfo(DC))

end;

class
function TTextDrawHelper.GetFreeGlyphArrayIndex(GlyphArray: PwcArray): Integer;
var
  I: Integer;
begin
  for I := High(GlyphArray) Downto 0 do
    if GlyphArray[I] = #0 then
      Exit(I);
  Exit(NULL_CHAR_INDEX);
end;

class
function TTextDrawHelper.GetALLH_LACharInfo(DC: HDC): WideCharInfo;
begin
  Result := TTextDrawHelper.GetCharacterPlace(DC, ALLAH_LA_TEXT);
end;

end.
