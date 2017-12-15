unit UfrmAbout;

interface

uses
  WinApi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  UGlobalUnit;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    WebSite: TLabel;
    OKButton: TButton;
    procedure WebSiteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Version.Caption := 'Version: ' + APPLICATION_VERSION;
end;

procedure TfrmAbout.WebSiteClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'https://github.com/Majid110/PersianTypeTutor', nil, nil, SW_NORMAL);
end;

end.
