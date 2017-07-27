unit UfrmSplash;

interface

uses
  Winapi.Windows,
  Vcl.Forms,
  Vcl.StdCtrls,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  UGlobalUnit;

type
  TfrmSplash = class(TForm)
    imgBack: TImage;
    lblWebsite: TLabel;
    lblVersion: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure imgBackClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    filepng: TPngImage;
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.dfm}


function CaptureScreenRect(ARect: TRect): TBitmap;
var
  ScreenDC: HDC;
begin
  Result := TBitmap.Create;
  Result.Width := ARect.Right - ARect.Left;
  Result.Height := ARect.Bottom - ARect.Top;
  ScreenDC := GetDC(0);
  try
    BitBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, ScreenDC,
      frmSplash.Left, frmSplash.Top, SRCCOPY);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

function CaptureScreen: TBitmap;
begin
  Result := CaptureScreenRect(frmSplash.ClientRect);
end;

procedure TfrmSplash.FormActivate(Sender: TObject);
var
  BitMap: TBitmap;
begin
  BitMap := CaptureScreen;
  try
    imgBack.Picture.BitMap := BitMap;
  finally
    BitMap.Free;
  end;
  filepng := TPngImage.Create;
  filepng.LoadFromFile(ApplicationPath + 'Data\Spl.SPL');
  filepng.Draw(imgBack.Canvas, imgBack.ClientRect);
end;

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := 'Version ' + APPLICATION_VERSION;
  BringWindowToTop(Application.Handle);
end;

procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
  filepng.Free;
end;

procedure TfrmSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := CaFree;
end;

procedure TfrmSplash.imgBackClick(Sender: TObject);
begin
  if tag = 1 then
      Close;
end;

procedure TfrmSplash.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (tag = 1) and (Key = VK_ESCAPE) then
      Close;
end;

end.
