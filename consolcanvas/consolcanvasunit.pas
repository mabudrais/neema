unit consolcanvasunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,neemacanvasUnit;
type

   { Tconsolcanvasunit }

   TConsolCanvas=class(INeemaCanvas)
   public
     constructor Create;
     procedure GetTextWidtrhHeight(Text: string; out W, H: integer);
     procedure WriteToMemo(Text: string);
     procedure TextOut(X, Y: integer; const Text: string);
     procedure SetTransform(CXT, CYT: Integer);
     procedure setPenWidth(w: integer);
     procedure setFontColor(c: TNeemaColour);
     procedure setBrushColor(w: integer);
     procedure Rectangle(X1, Y1, X2, Y2: integer);
     procedure Line(x1, y1, x2, y2: integer);
     function getPenWidth: integer;
     function getFontColor: TNeemaColour;
     function getBrushColor: integer;
     procedure FillRect(X1, Y1, X2, Y2: integer);
     procedure EllipseC(x, y: integer; rx, ry: longword);
     procedure Ellipse(x1, y1, x2, y2: integer);
     procedure Clear;
     procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: integer);
     procedure TransFormPoint(var X, Y: integer);
     procedure setPenColor(c: TNeemaColour);
     procedure SetCanvas(c: TObject);
     procedure SetBitmapSize(W, H: Integer);
     procedure MoveTo(X, Y: integer);
     function GetWidth(): integer;
     function getPenColor: TNeemaColour;
     function GetHeight(): integer;
     procedure AngleArc(X, Y: integer; Radius: longword; StartAngle, SweepAngle: single);
   end;

implementation

{ Tconsolcanvas }

procedure TConsolCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: integer);
begin

end;

procedure TConsolCanvas.Clear;
begin

end;

procedure TConsolCanvas.Ellipse(x1, y1, x2, y2: integer);
begin

end;

procedure TConsolCanvas.EllipseC(x, y: integer; rx, ry: longword);
begin

end;

procedure TConsolCanvas.FillRect(X1, Y1, X2, Y2: integer);
begin

end;

function TConsolCanvas.getBrushColor: integer;
begin
      Result:=-1;
end;

function TConsolCanvas.getFontColor: TNeemaColour;
begin
      Result:=-1;
end;

function TConsolCanvas.getPenWidth: integer;
begin
     Result:=-1;
end;

procedure TConsolCanvas.Line(x1, y1, x2, y2: integer);
begin

end;

procedure TConsolCanvas.Rectangle(X1, Y1, X2, Y2: integer);
begin

end;

procedure TConsolCanvas.setBrushColor(w: integer);
begin

end;

procedure TConsolCanvas.setFontColor(c: TNeemaColour);
begin

end;

procedure TConsolCanvas.setPenWidth(w: integer);
begin

end;

procedure TConsolCanvas.SetTransform(CXT, CYT: Integer);
begin

end;

procedure TConsolCanvas.TextOut(X, Y: integer; const Text: string);
begin

end;

procedure TConsolCanvas.WriteToMemo(Text: string);
begin

end;

constructor TConsolCanvas.Create;
begin

end;

procedure TConsolCanvas.GetTextWidtrhHeight(Text: string; out W, H: integer);
begin

end;

procedure TConsolCanvas.AngleArc(X, Y: integer; Radius: longword; StartAngle, SweepAngle: single);
begin

end;

function TConsolCanvas.GetHeight(): integer;
begin
        Result:=-1;
end;

function TConsolCanvas.getPenColor: TNeemaColour;
begin
                 Result:=-1;
end;

function TConsolCanvas.GetWidth(): integer;
begin
   Result:=-1;
end;

procedure TConsolCanvas.MoveTo(X, Y: integer);
begin

end;

procedure TConsolCanvas.SetBitmapSize(W, H: Integer);
begin

end;

procedure TConsolCanvas.SetCanvas(c: TObject);
begin

end;

procedure TConsolCanvas.setPenColor(c: TNeemaColour);
begin

end;

procedure TConsolCanvas.TransFormPoint(var X, Y: integer);
begin

end;

end.

