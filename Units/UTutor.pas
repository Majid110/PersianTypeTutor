unit UTutor;

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
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  System.StrUtils,
  UGlobalUnit,
  UObserver,
  PngFunctions;

type
  TTutor = class(TFrame, ISubject)
    imgBack: TImage;
    gbMode: TGroupBox;
    rbPersian: TRadioButton;
    rbEnglish: TRadioButton;
    imgPrior: TImage;
    imgNext: TImage;
    lblRecord: TLabel;
    Label4: TLabel;
    lblWrongCount: TLabel;
    Label2: TLabel;
    lblTypedCount: TLabel;
    Label3: TLabel;
    lblPassedTime: TLabel;
    Label1: TLabel;
    cbToturNo: TComboBox;
    LbLesson: TLabel;
    imgOSK: TImage;
    imgAbout: TImage;
    imgSound: TImage;
    rbMicrosoftPersian: TRadioButton;
    procedure rbPersianClick(Sender: TObject);
    procedure rbEnglishClick(Sender: TObject);
    procedure cbToturNoChange(Sender: TObject);
    procedure imgNextClick(Sender: TObject);
    procedure imgPriorClick(Sender: TObject);
    procedure cbToturNoCloseUp(Sender: TObject);
    procedure imgSoundClick(Sender: TObject);
    procedure rbMicrosoftPersianClick(Sender: TObject);
  private
    FTimeSec         : Integer;
    FStartTime       : Extended;
    FElapsedTime     : Extended;
    FWrongCount      : Integer;
    FTypedCount      : Integer;
    FTutorState      : TTutorState;
    FTypeTimer       : TTimer;
    FTutorNo         : Integer;
    FCurrentTutorText: TStrings;
    FObservers       : TInterfaceList;
    FSilentMode      : Boolean;
    FKeyboardLayout  : TKeyboardLayout;
    procedure TypeTimerOnTimer(Sender: TObject);
    procedure LoadTutorFromFile;
    procedure SetSilentMode(const Value: Boolean);
    procedure SetKeyboardLayout(const Value: TKeyboardLayout);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
    procedure Init;
    procedure Start;
    procedure Finish;
    function IncrementWrong: Integer;
    function IncrementTypedCount: Integer;
    function TutorState: TTutorState;
    function CurrentTutor: TStrings;
  published
    property SilentMode    : Boolean read FSilentMode write SetSilentMode;
    property KeyboardLayout: TKeyboardLayout read FKeyboardLayout write SetKeyboardLayout;
  end;

implementation

{$R *.dfm}


procedure TTutor.Attach(Observer: IObserver);
begin
  if FObservers = nil then
    FObservers := TInterfaceList.Create;
  if FObservers.IndexOf(Observer) < 0 then
    FObservers.Add(Observer);
end;

procedure TTutor.cbToturNoChange(Sender: TObject);
begin
  Init;
  FTutorNo := cbToturNo.ItemIndex + 1;
  LoadTutorFromFile;
end;

procedure TTutor.cbToturNoCloseUp(Sender: TObject);
begin
  SetFocus;
end;

constructor TTutor.Create(AOwner: TComponent);
begin
  inherited;
  imgBack.Canvas.Lock;
  FTypeTimer := TTimer.Create(Self);
  FTypeTimer.OnTimer := TypeTimerOnTimer;
  FTypeTimer.Enabled := False;
  FCurrentTutorText := TStringList.Create;
  FKeyboardLayout := klPersian;
  FTutorNo := 0;
  Init;
end;

destructor TTutor.Destroy;
begin
  FCurrentTutorText.Free;
  if FObservers <> nil then
    FreeAndNil(FObservers);
  inherited;
end;

procedure TTutor.Detach(Observer: IObserver);
begin
  if FObservers <> nil then
  begin
    FObservers.Remove(Observer);
    if FObservers.Count = 0 then
    begin
      FObservers.Free;
      FObservers := nil;
    end;
  end;
end;

procedure TTutor.Finish;
begin
  FTypeTimer.Enabled := False;
  FTutorState := tsFinished;
end;

procedure TTutor.imgNextClick(Sender: TObject);
begin
  if FTutorNo < 18 then
  begin
    Init;
    Inc(FTutorNo);
    cbToturNo.ItemIndex := FTutorNo - 1;
    LoadTutorFromFile;
  end;
end;

procedure TTutor.imgPriorClick(Sender: TObject);
begin
  if FTutorNo > 1 then
  begin
    Init;
    Dec(FTutorNo);
    cbToturNo.ItemIndex := FTutorNo - 1;
    LoadTutorFromFile;
  end;
end;

procedure TTutor.imgSoundClick(Sender: TObject);
begin
  SilentMode := Not SilentMode;
end;

function TTutor.IncrementWrong: Integer;
begin
  Inc(FWrongCount);
  lblWrongCount.Caption := IntToStr(FWrongCount);
  Result := FWrongCount;
end;

function TTutor.IncrementTypedCount: Integer;
begin
  Inc(FTypedCount);
  lblTypedCount.Caption := IntToStr(FTypedCount);
  if FTimeSec <> 0 then
    lblRecord.Caption := IntToStr(Trunc((60 * FTypedCount) / FTimeSec));
  Result := FTypedCount;
end;

procedure TTutor.Init;
begin
  FTypedCount := 0;
  FWrongCount := 0;
  FTimeSec := 0;
  FStartTime := 0;
  FElapsedTime := 0;
  lblRecord.Caption := '0';
  lblWrongCount.Caption := '0';
  lblTypedCount.Caption := '0';
  lblPassedTime.Caption := '0';
  lblPassedTime.Caption := '00:00';
  FTutorState := tsReady;
  FTypeTimer.Enabled := False;
end;

procedure TTutor.LoadTutorFromFile;
var
  FileName: String;
  Title : string;
begin
  if FTutorNo = 0 then
    Exit;
  FCurrentTutorText.Clear;
  FileName := ifThen(FKeyboardLayout = klEnglish, 'E', 'F');
  FCurrentTutorText.LoadFromFile(ApplicationPath + 'Data\Tutors\' + FileName + IntToStr(FTutorNo) + TUTOR_EXT);

  // Some change based on persian keyboard layout
  if (KeyboardLayout = klMicrosoftPersian) and (FTutorNo <= 8) and (FTutorNo <> 6) then
  begin
    Title := FCurrentTutorText[0]; //Prevent tutor title change
    FCurrentTutorText.Text := ReplaceStr(FCurrentTutorText.Text, 'Å', '∆');
    FCurrentTutorText[0] := Title;
  end;
  Notify;
end;

procedure TTutor.Notify;
var
  i: Integer;
begin
  if FObservers <> nil then
    for i := 0 to Pred(FObservers.Count) do
      IObserver(FObservers[i]).Update(Self);
end;

procedure TTutor.rbEnglishClick(Sender: TObject);
begin
  Init;
  FKeyboardLayout := klEnglish;
  LoadTutorFromFile;
end;

procedure TTutor.rbMicrosoftPersianClick(Sender: TObject);
begin
  Init;
  FKeyboardLayout := klMicrosoftPersian;
  LoadTutorFromFile;
end;

procedure TTutor.rbPersianClick(Sender: TObject);
begin
  Init;
  FKeyboardLayout := klPersian;
  LoadTutorFromFile;
end;

procedure TTutor.SetKeyboardLayout(const Value: TKeyboardLayout);
begin
  FKeyboardLayout := Value;
end;

procedure TTutor.SetSilentMode(const Value: Boolean);
begin
  FSilentMode := Value;
end;

procedure TTutor.Start;
begin
  FElapsedTime := Now;
  FStartTime := FElapsedTime;
  FTypeTimer.Enabled := True;
  FTutorState := tsStarted;
end;

procedure TTutor.TypeTimerOnTimer(Sender: TObject);
begin
  Inc(FTimeSec);
  FElapsedTime := Now - FStartTime;
  lblPassedTime.Caption := FormatDateTime('nn:ss', FElapsedTime);
end;

function TTutor.TutorState: TTutorState;
begin
  Result := FTutorState;
end;

function TTutor.CurrentTutor: TStrings;
begin
  Result := FCurrentTutorText;
end;

end.
