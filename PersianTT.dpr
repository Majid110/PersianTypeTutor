program PersianTT;

{$R *.dres}


uses
  Vcl.Forms,
  System.SysUtils,
  UfrmMain in 'Units\UfrmMain.pas' {frmMain} ,
  UTextDrawer in 'Units\UTextDrawer.pas',
  UKeyboard in 'Units\UKeyboard.pas' {Keyboard: TFrame} ,
  UKeyboardKey in 'Units\UKeyboardKey.pas',
  UGlobalUnit in 'Units\UGlobalUnit.pas',
  UFingerCaption in 'Units\UFingerCaption.pas',
  UHand in 'Units\UHand.pas' {Hand: TFrame} ,
  UObserver in 'Units\UObserver.pas',
  UTutor in 'Units\UTutor.pas' {Tutor: TFrame} ,
  UTutorController in 'Units\UTutorController.pas',
  UfrmSplash in 'Units\UfrmSplash.pas' {frmSplash} ,
  UfrmAbout in 'Units\UfrmAbout.pas' {frmAbout};

{$R *.res}

var
  Version: string;

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  frmSplash := TfrmSplash.Create(Application);
  frmSplash.Show;
  frmSplash.Update;
  GetLatestVersion;
  if LatestVersion = '' then
    Sleep(1500);
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  frmSplash.Hide;
  frmSplash.Free;
  frmSplash := nil;
  Application.Run;

end.
