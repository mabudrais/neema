unit drawingutilunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, neemacanvasUnit,UtilUnit;

const
  CIRCLERADUIS = 20;
  CBHALFWIDTH = 10;

procedure DrawCB(C: INeemaCanvas; X, Y: integer; IsClosed: boolean; Colour: integer);
procedure DrawTC(C: INeemaCanvas; X, Y: integer; Colour: integer);
procedure DrawRC(C: INeemaCanvas; X, Y: integer; Colour: integer);
procedure DrawTSC(C: INeemaCanvas; X, Y: integer; Colour: integer);
procedure T3WConectionPos(ConIndex,XPos,YPos:Integer;out ConX, ConY:Integer);
implementation

procedure DrawCB(C: INeemaCanvas; X, Y: integer; IsClosed: boolean; Colour: integer);
var
  OldColour: integer;
  OldPenColour: TNeemaColour;
begin
  OldColour := C.getBrushColor;
  OldPenColour := c.getPenColor;
  c.setPenColor(Colour);
  c.setBrushColor(clNemWhite);
  if IsClosed then
  begin
    C.setBrushColor(Colour);
    c.setPenColor(Colour);
  end;
  c.Rectangle(X - CBHALFWIDTH, Y - CBHALFWIDTH, X + CBHALFWIDTH,
    Y + CBHALFWIDTH);
  c.setPenColor(OldPenColour);
  c.setBrushColor(OldColour);
end;

procedure DrawTC(C: INeemaCanvas; X, Y: integer; Colour: integer);
var
  OldColour: integer;
  OldPenColour: TNeemaColour;
begin
  OldColour := C.getBrushColor;
  OldPenColour := c.getPenColor;
  c.setPenColor(Colour);
  C.setBrushColor(clNemSilver);
  c.EllipseC(X, Y, CIRCLERADUIS, CIRCLERADUIS);
  c.setPenColor(OldPenColour);
  c.setBrushColor(OldColour);
end;

procedure DrawTSC(C: INeemaCanvas; X, Y: integer; Colour: integer);
var
  OldBrushColor: integer;
  OldPenColour: TNeemaColour;
begin
  OldBrushColor := c.getBrushColor;
  OldPenColour := c.getPenColor;
  c.setBrushColor(clYellow);
  c.setPenColor(clYellow);
  c.EllipseC(X, Y, CIRCLERADUIS + 2, CIRCLERADUIS + 2);
  c.setPenColor(OldPenColour);
  c.setBrushColor(OldBrushColor);
end;

procedure T3WConectionPos(ConIndex,XPos,YPos:Integer;out ConX, ConY:Integer);
begin
   if ConIndex = 0 then
      SetTwoVal(XPos, YPos - 2 * CIRCLERADUIS, ConX, ConY)
    else if ConIndex = 1 then
      SetTwoVal(XPos - round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS, ConX, ConY)
    else if ConIndex = 2 then
      SetTwoVal(XPos + round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS, ConX, ConY);
end;

procedure DrawRC(C: INeemaCanvas; X, Y: integer; Colour: integer);
var
  GroundLength: integer;
  ArcRadais, OldWidth: integer;
  OldColour: TNeemaColour;
begin
  OldColour := C.getPenColor;
  OldWidth := c.getPenWidth;
  c.setPenColor(Colour);
  c.setPenWidth(2);
  ArcRadais := 10;
  GroundLength := 10;
  c.MoveTo(x, y);
  c.AngleArc(x, y + ArcRadais, ArcRadais, 90, 270);
  c.Line(x, y + ArcRadais, x + ArcRadais, y + ArcRadais);
  c.Line(x, y + ArcRadais, x, y + 3 * ArcRadais);
  c.Line(x - round(GroundLength / 2), y + 3 * ArcRadais, x +
    round(GroundLength / 2), y + 3 * ArcRadais);
  c.Line(x - round(GroundLength / 4), y + round(3.5 * ArcRadais), x +
    round(GroundLength / 4),
    y + round(3.6 * ArcRadais));
  c.setPenColor(OldColour);
  c.setPenWidth(OldWidth);
end;

end.
