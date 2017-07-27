unit UfrmMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  UTextDrawer,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  UGlobalUnit,
  UKeyboard,
  UHand,
  UTutor,
  UObserver,
  UTutorController,
  UfrmAbout;

type

  TfrmMain = class(TForm, IObserver)
    Keyboard1: TKeyboard;
    Hand1: THand;
    imgBack: TImage;
    Tutor1: TTutor;
    pnlClient: TPanel;
    lblTutorTitle: TLabel;
    pnlBack: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Tutor1imgOSKClick(Sender: TObject);
    procedure Tutor1imgAboutClick(Sender: TObject);
  private
    { Private declarations }
    FTextDrawer     : TTextDrawer;
    FTutorController: TTutorController;
    procedure Update(Subject: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LockWindowUpdate(Self.Handle);
  Self.WindowState := wsMaximized;
  LockWindowUpdate(0);

  FTextDrawer := TTextDrawer.Create(Self);
  FTextDrawer.SetBounds(8, 64, 1005, 186);
  FTextDrawer.RightIndent := 10;
  FTextDrawer.Color := RGB(100, 149, 237);
  FTextDrawer.Parent := pnlBack;

  FTutorController := TTutorController.Create;
  FTutorController.Tutor := Tutor1;
  FTutorController.TextDrawer := FTextDrawer;
  FTutorController.Keyboard := Keyboard1;
  FTutorController.Hand := Hand1;

  Tutor1.Attach(Self);
  Tutor1.Attach(FTextDrawer);
  Tutor1.Attach(Keyboard1);
  Tutor1.Attach(FTutorController);
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ActiveControl = Tutor1.cbToturNo then
      Exit;

  if Tutor1.TutorState = tsFinished then
      Exit
  else if (FTextDrawer.TutorState = tsReady) and (Tutor1.TutorState <> tsStarted) then
      Tutor1.Start;

  if (Key = VK_SHIFT) or (Key = VK_TAB) then
      Exit;

  FTutorController.CheckKey(Key, Shift);
end;

procedure TfrmMain.Tutor1imgAboutClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Application);
  try
    frmAbout.ShowModal;
  finally
    frmAbout.Free;
  end;
end;

procedure TfrmMain.Tutor1imgOSKClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'Open', 'osk', nil, nil, SW_NORMAL);
end;

procedure TfrmMain.Update(Subject: TObject);
begin
  lblTutorTitle.Caption := TTutor(Subject).CurrentTutor[0];
end;

end.
