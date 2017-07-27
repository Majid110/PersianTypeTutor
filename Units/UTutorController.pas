unit UTutorController;

interface

uses
  Winapi.MMSystem,
  System.Classes,
  System.SysUtils,
  UTutor,
  UTextDrawer,
  UKeyboard,
  UHand,
  UGlobalUnit,
  UObserver;

type
  TTutorController = class(TInterfacedObject, IObserver)
  private
    FTutor     : TTutor;
    FTextDrawer: TTextDrawer;
    FKeyboard  : TKeyboard;
    FHand      : THand;
    FKeySound  : String;
    FWrongSound: String;
    FSpaceSound: String;
    function LoadSoundFile(const FileName: TFileName): string;
    procedure PlayKeySound;
    procedure PlayWrongSound;
    procedure PlaySpaceSound;
    procedure SetTextDrawer(const Value: TTextDrawer);
    procedure SetKeyboard(const Value: TKeyboard);
    procedure SetHand(const Value: THand);
    procedure SetTutor(const Value: TTutor);
    procedure Update(Subject: TObject);
  public
    FLastTypeWrong: Boolean;
    constructor Create;
    procedure LoadSoundFiles;
    procedure CheckKey(Key: Word; Shift: TShiftState);
  published
    property TextDrawer: TTextDrawer read FTextDrawer write SetTextDrawer;
    property Keyboard  : TKeyboard read FKeyboard write SetKeyboard;
    property Hand      : THand read FHand write SetHand;
    property Tutor     : TTutor read FTutor write SetTutor;
  end;

implementation

{ TTutorController }

procedure TTutorController.CheckKey(Key: Word; Shift: TShiftState);
var
  AnsiChr    : AnsiString;
  KeyChar    : AnsiChar;
  RandomSound: Integer;
begin

  if FTutor.TutorState <> tsStarted then
    Exit;

  if FTextDrawer.TutorState = tsFinished then
  begin
    FTutor.Finish;
    Exit;
  end;

  KeyChar := FKeyboard.TranslateKey(Key, Shift);

  if KeyChar = 'ß' then
      KeyChar := '˜';

  AnsiChr := FTextDrawer.NextChar;

  // Change virtual space to basic space when keyboard layout is Microsoft Persian
  if (AnsiChr = VIRTUAL_SPACE) and (FTutor.KeyboardLayout = klMicrosoftPersian) then
    AnsiChr := ' ';

  if KeyChar <> AnsiChr[1] then
  begin
    PlayWrongSound;
    if not FLastTypeWrong then
    begin
      FTutor.IncrementWrong;
      FLastTypeWrong := True;
    end;
  end
  else
  begin
    FTutor.IncrementTypedCount;
    FTextDrawer.FillNextChar;
    FKeyboard.FillNextKey(FTextDrawer.NextChar);
    FLastTypeWrong := False;
    FHand.ShowFinger(FKeyboard.CurrentKey.FingerNo, TextDrawer.NextChar, FKeyboard.SheftNeeded);
    if not FTutor.SilentMode then
    begin
      if KeyChar = #32 then
          PlaySpaceSound
      else
      begin
        RandomSound := Random(2);
        if RandomSound = 1 then
            PlaySpaceSound
        else
            PlayKeySound;
      end;
    end;
  end;
end;

constructor TTutorController.Create;
begin
  LoadSoundFiles;
end;

procedure TTutorController.LoadSoundFiles;
begin
  FKeySound := LoadSoundFile(ApplicationPath + 'Data\type3.wav');
  FWrongSound := LoadSoundFile(ApplicationPath + 'Data\BEEP.WAV');
  FSpaceSound := LoadSoundFile(ApplicationPath + 'Data\type2.wav');
end;

procedure TTutorController.PlayWrongSound;
begin
  sndPlaySound(Pointer(FWrongSound), SND_MEMORY or SND_NODEFAULT or SND_ASYNC);
end;

procedure TTutorController.SetHand(const Value: THand);
begin
  FHand := Value;
end;

procedure TTutorController.SetKeyboard(const Value: TKeyboard);
begin
  FKeyboard := Value;
end;

procedure TTutorController.SetTextDrawer(const Value: TTextDrawer);
begin
  FTextDrawer := Value;
end;

procedure TTutorController.SetTutor(const Value: TTutor);
begin
  FTutor := Value;
end;

procedure TTutorController.Update(Subject: TObject);
begin
  FKeyboard.FillNextKey(FTextDrawer.NextChar);
  FHand.ShowFinger(FKeyboard.CurrentKey.FingerNo, FKeyboard.CurrentKey.Caption, FKeyboard.SheftNeeded);
end;

procedure TTutorController.PlaySpaceSound;
begin
  sndPlaySound(Pointer(FSpaceSound), SND_MEMORY or SND_NODEFAULT or SND_ASYNC)
end;

procedure TTutorController.PlayKeySound;
begin
  sndPlaySound(Pointer(FKeySound), SND_MEMORY or SND_NODEFAULT or SND_ASYNC);
end;

function TTutorController.LoadSoundFile(const FileName: TFileName): string;
begin
  with TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    except
      Result := ''; // Deallocates memory
      Free;
      raise;
    end;
    Free;
  end;
end;

end.
