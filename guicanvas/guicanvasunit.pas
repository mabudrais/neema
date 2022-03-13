unit GuiCanvasUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, neemacanvasUnit, FPCanvas, StdCtrls;

type

  { TGUICanvas }

  TGUICanvas = class(INeemaCanvas)
    constructor Create(c: TCanvas; M: TMemo);
  private
    canvas: TCanvas;
    Memo1: TMemo;
    XTransForm, YTransForm: integer;
    BitmapW, BitmapH: integer;
  public
    procedure SetCanvas(c: TObject);
    procedure SetBitmapSize(W, H: integer);
    function GetWidth(): integer;
    function GetHeight(): integer;
    procedure TextOut(X, Y: integer; const Text: string);
    procedure GetTextWidtrhHeight(Text: string; out W, H: integer);
    procedure setPenWidth(w: integer);
    procedure setBrushColor(w: integer);
    procedure Rectangle(X1, Y1, X2, Y2: integer);
    procedure Line(x1, y1, x2, y2: integer);
    function getPenWidth: integer;
    function getBrushColor: integer;
    procedure FillRect(X1, Y1, X2, Y2: integer);
    procedure EllipseC(x, y: integer; rx, ry: longword);
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: integer);
    procedure setFontColor(c: TNeemaColour);
    function getFontColor: TNeemaColour;
    procedure setPenColor(c: TNeemaColour);
    function getPenColor: TNeemaColour;
    procedure Ellipse(x1, y1, x2, y2: integer);
    procedure MoveTo(X, Y: integer);
    procedure TransFormPoint(var X, Y: integer);
    procedure WriteToMemo(Text: string);
    procedure Clear;
    procedure SetTransform(CXT, CYT: integer);
    procedure AngleArc(X, Y: integer; Radius: longword; StartAngle, SweepAngle: single);
  end;

implementation

{ TGUICanvas }

constructor TGUICanvas.Create(c: TCanvas; M: TMemo);
begin
  canvas := c;
  Memo1 := m;
  XTransForm := 0;
  YTransForm := 0;
end;

procedure TGUICanvas.EllipseC(x, y: integer; rx, ry: longword);
begin
  canvas.EllipseC(x + XTransForm, y + YTransForm, rx, ry);
end;

procedure TGUICanvas.FillRect(X1, Y1, X2, Y2: integer);
begin
  canvas.FillRect(X1 + XTransForm, Y1 + YTransForm, X2 + XTransForm, Y2 + YTransForm);
end;

function TGUICanvas.getBrushColor: integer;
begin
  Result := canvas.Brush.Color;
end;

function TGUICanvas.getPenWidth: integer;
begin
  Result := canvas.Pen.Width;
end;

procedure TGUICanvas.Line(x1, y1, x2, y2: integer);
begin
  canvas.Line(x1 + XTransForm, y1 + YTransForm, x2 + XTransForm, y2 + YTransForm);
end;

procedure TGUICanvas.Rectangle(X1, Y1, X2, Y2: integer);
begin
  canvas.Rectangle(x1 + XTransForm, y1 + YTransForm, x2 + xTransForm, y2 + YTransForm);
end;

procedure TGUICanvas.setBrushColor(w: integer);
begin
  canvas.Brush.Color := w;
end;

procedure TGUICanvas.setPenWidth(w: integer);
begin
  canvas.Pen.Width := w;
end;

procedure TGUICanvas.TextOut(X, Y: integer; const Text: string);
var
  OldBrushStyle: TFPBrushStyle;
begin
  OldBrushStyle := canvas.Brush.Style;
  canvas.Brush.Style := bsClear;
  canvas.TextOut(X + XTransForm, Y + YTransForm, Text);
  canvas.Brush.Style := OldBrushStyle;
end;

procedure TGUICanvas.GetTextWidtrhHeight(Text: string; out W, H: integer);
begin
  H := canvas.TextHeight(Text);
  W := canvas.TextWidth(Text);
end;

procedure TGUICanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg,
  Angle16DegLength: integer);
begin
  canvas.Arc(ALeft + XTransForm, ATop + YTransForm, ARight + XTransForm,
    ABottom + YTransForm,
    Angle16Deg, Angle16DegLength);
end;

procedure TGUICanvas.Ellipse(x1, y1, x2, y2: integer);
begin
  canvas.Ellipse(x1 + XTransForm, y1 + YTransForm, x2 + XTransForm, y2 + YTransForm);
end;

function TGUICanvas.getFontColor: TNeemaColour;
begin
  Result := canvas.Font.Color;
end;

procedure TGUICanvas.setFontColor(c: TNeemaColour);
begin
  canvas.Font.Color := c;
end;

procedure TGUICanvas.Clear;
begin
  canvas.Clear;
end;

procedure TGUICanvas.WriteToMemo(Text: string);
begin
  Memo1.Append(Text);
end;

procedure TGUICanvas.SetTransform(CXT, CYT: integer);
begin
  XTransForm := CXT;
  YTransForm := CYT;
end;

function TGUICanvas.getPenColor: TNeemaColour;
begin
  Result := canvas.Pen.Color;
end;

procedure TGUICanvas.setPenColor(c: TNeemaColour);
begin
  canvas.Pen.Color := c;
end;

procedure TGUICanvas.MoveTo(X, Y: integer);
begin
  canvas.MoveTo(X + XTransForm, Y + YTransForm);
end;

procedure TGUICanvas.AngleArc(X, Y: integer; Radius: longword;
  StartAngle, SweepAngle: single);
begin
  canvas.AngleArc(X + XTransForm, Y + YTransForm, Radius, StartAngle, SweepAngle);
end;

procedure TGUICanvas.TransFormPoint(var X, Y: integer);
begin
  x := x + XTransForm;
  y := Y + YTransForm;
end;

procedure TGUICanvas.SetCanvas(c: TObject);
begin
  canvas := c as TCanvas;
end;

function TGUICanvas.GetHeight(): integer;
begin
  Result := BitmapH;
end;

function TGUICanvas.GetWidth(): integer;
begin
  Result := BitmapW;
end;

procedure TGUICanvas.SetBitmapSize(W, H: integer);
begin
  BitmapW := W;
  BitmapH := H;
end;

end.
