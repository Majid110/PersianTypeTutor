unit UHand;

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
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  UFingerCaption;

type
  THand = class(TFrame)
    imgHand: TImage;
    fcFingerCaption: TFingerCaption;
    fcShift: TFingerCaption;
    shp1: TShape;
    shp2: TShape;
    shp3: TShape;
    shp4: TShape;
    shp5: TShape;
    shp6: TShape;
    shp7: TShape;
    shp8: TShape;
    shp9: TShape;
    shp10: TShape;
  private
    FingerPosition: array [1 .. 10] of TPoint;
    procedure ChangeFingerCaptionPosition(FingerNo: Byte);
    function ReadPngImageFromResource: TPngImage;
    procedure ChangeShiftCaptionPosition(FingerNo: Byte);
    procedure SetFingerPositions;
  public
    Constructor Create(AOwner: TComponent); override;
    procedure ShowFinger(FingerNo: Integer; Letter: String; ShiftPressed: Boolean);
  end;

implementation

{$R *.dfm}


constructor THand.Create(AOwner: TComponent);
var
  png: TPngImage;
begin
  inherited;
  //png := ReadPngImageFromResource;
  fcFingerCaption.pngimage := ReadPngImageFromResource;
  fcFingerCaption.Font := Font;
  fcShift.pngimage := ReadPngImageFromResource;
  fcShift.Font := Font;
  SetFingerPositions;
end;

function THand.ReadPngImageFromResource: TPngImage;
var
  RS : TResourceStream;
  png: TPngImage;
begin
  RS := TResourceStream.Create(hInstance, 'FingerCaptionImage', RT_RCDATA);
  try
    png := TPngImage.Create;
    png.LoadFromStream(RS);
  finally
    RS.Free;
  end;
  Result := png;
end;

procedure THand.SetFingerPositions;
  function GetShapeXY(Shape: TShape): TPoint;
  begin
    Result.X := Trunc((Shape.Top + (Shape.Height / 2)) - (fcShift.Height / 2));
    Result.Y := Trunc((Shape.Left + (Shape.Width / 2)) - (fcShift.Width / 2));
  end;

begin
  FingerPosition[1] := GetShapeXY(shp1);
  FingerPosition[2] := GetShapeXY(shp2);
  FingerPosition[3] := GetShapeXY(shp3);
  FingerPosition[4] := GetShapeXY(shp4);
  FingerPosition[5] := GetShapeXY(shp5);
  FingerPosition[6] := GetShapeXY(shp6);
  FingerPosition[7] := GetShapeXY(shp7);
  FingerPosition[8] := GetShapeXY(shp8);
  FingerPosition[9] := GetShapeXY(shp9);
  FingerPosition[10] := GetShapeXY(shp10);
end;

Procedure THand.ShowFinger(FingerNo: Integer; Letter: String; ShiftPressed: Boolean);
begin
  fcFingerCaption.Visible := True;
  if FingerNo = 6 then
      fcFingerCaption.Caption := 'Space'
  else
      fcFingerCaption.Caption := Letter;
  ChangeFingerCaptionPosition(FingerNo);
  if ShiftPressed then
      ChangeShiftCaptionPosition(FingerNo)
  else
      fcShift.Visible := False;
end;

procedure THand.ChangeFingerCaptionPosition(FingerNo: Byte);
begin
  fcFingerCaption.Left := FingerPosition[FingerNo].Y;
  fcFingerCaption.Top := FingerPosition[FingerNo].X;
  fcFingerCaption.Repaint;
end;

procedure THand.ChangeShiftCaptionPosition(FingerNo: Byte);
begin

  fcShift.Visible := True;
  if FingerNo > 5 then
  begin
    fcShift.Left := FingerPosition[1].Y;
    fcShift.Top := FingerPosition[1].X;
  end
  else
  begin
    fcShift.Left := FingerPosition[10].Y;
    fcShift.Top := FingerPosition[10].X;
  end;
end;

end.
