unit UFingerCaption;

interface

uses
  Windows,
  // SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  pngimage,
  Graphics,
  Dialogs;

type
  TFingerCaption = class(TPaintbox)
  private
    FElementName: string;
    FPngImage   : TPngImage;
    { Private declarations }
    procedure SetPngImage(const Value: TPngImage);
  protected
    { Protected declarations }
  public
    { Public declarations }
    destructor Destroy; override;
    procedure Paint; override;
  published
    { Published declarations }
    property pngimage: TPngImage read FPngImage write SetPngImage;
    property Autosize;
    property Caption;
    property Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PersianTypeTutor', [TFingerCaption]);
end;

{ TPsPNGPicture }

destructor TFingerCaption.Destroy;
begin
  if FPngImage <> nil then
  begin
    FPngImage.Free;
    FPngImage := nil;
  end;
  inherited;
end;

procedure TFingerCaption.SetPngImage(const Value: TPngImage);
begin
  if Value <> nil then
  begin
    FPngImage := Value;
    SetBounds(left, Top, FPngImage.Width, FPngImage.Height);
    Paint;
  end;
end;

procedure TFingerCaption.Paint;
var
  Rc: TRect;
begin
  inherited;
  Canvas.Lock;
  try
    if FPngImage <> nil then
        FPngImage.Draw(Canvas, ClientRect);
    if Caption <> '' then
    begin
      Canvas.Font := Font;
      Canvas.Brush.Style := bsClear;
      Rc := ClientRect;
      DrawText(Canvas.Handle, Pchar(Caption), Length(Caption), Rc, DT_RTLREADING or DT_SINGLELINE or DT_CENTER or
        DT_VCENTER);
    end;
  finally
    Canvas.Unlock;
  end;
end;

end.
