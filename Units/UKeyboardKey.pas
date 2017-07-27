unit UKeyboardKey;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  windows,
  graphics,
  Messages;

type
  TKeyboardKey = class(TShape)
  private
    FPicture : TPicture;
    FFingerNo: integer;
    FKeyNumber: Integer;
    { Private declarations }
    procedure SetPicture(Value: TPicture);
    procedure DrawPicture;
    procedure SetKeyNumber(const Value: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure FONTCHANGED(var Message: TMessage); message CM_FONTCHANGED;
    procedure TEXTCHANGED(var Message: TMessage); message CM_TEXTCHANGED;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Caption;
    property Font;
    property Picture : TPicture read FPicture write SetPicture;
    property FingerNo: integer read FFingerNo write FFingerNo;
    property KeyNumber: Integer read FKeyNumber write SetKeyNumber;
    property ParentFont;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PersianTypeTutor', [TKeyboardKey]);
end;

{ TPsCaptionShape }

constructor TKeyboardKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
end;

destructor TKeyboardKey.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TKeyboardKey.DrawPicture;
var
  t, l: integer;
  G   : TGraphic;
begin
  G := FPicture.Graphic;
  t := (Height - G.Height) div 2;
  l := (Width - G.Width) div 2;
  G.Transparent := True;
  Canvas.Draw(t, l, G);
end;

procedure TKeyboardKey.FONTCHANGED(var Message: TMessage);
begin
  Repaint;
end;

procedure TKeyboardKey.Paint;
var
  Rc: TRect;
begin
  inherited;
  Rc := ClientRect;
  Canvas.Font := Font;
  Canvas.Brush.Style := bsClear;
  if FPicture.Bitmap <> nil then
      DrawPicture;
  DrawText(Canvas.Handle, pchar(Caption), length(Caption), Rc, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
end;

procedure TKeyboardKey.SetKeyNumber(const Value: Integer);
begin
  FKeyNumber := Value;
end;

procedure TKeyboardKey.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  Repaint;
end;

procedure TKeyboardKey.TEXTCHANGED(var Message: TMessage);
begin
  Repaint;
end;

end.
