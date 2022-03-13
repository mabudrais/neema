unit neemacanvasUnit;

{$mode objfpc}{$H+}
 {$Interfaces CORBA}
interface

uses
  Classes, SysUtils;

type
  TNeemaColour = integer;
  INeemaImageList = interface
  end;

  { INeemaCanvas }

  INeemaCanvas = interface
    ['{76D7C3B3-C189-4ECF-BFBA-57AADCE8B916}']
    procedure SetCanvas(c: TObject);
    function GetWidth(): integer;
    function GetHeight(): integer;
    procedure SetBitmapSize(W, H: integer);
    procedure EllipseC(x, y: integer; rx, ry: longword);
    procedure Ellipse(x1, y1, x2, y2: integer);
    procedure TextOut(X, Y: integer; const Text: string);
    procedure GetTextWidtrhHeight(Text: string; out W, H: integer);
    procedure Rectangle(X1, Y1, X2, Y2: integer);
    procedure FillRect(X1, Y1, X2, Y2: integer);
    procedure Line(x1, y1, x2, y2: integer);
    procedure setPenWidth(w: integer);
    function getPenWidth: integer;
    procedure setBrushColor(w: integer);
    function getBrushColor: integer;
    procedure setFontColor(c: TNeemaColour);
    function getFontColor: TNeemaColour;
    procedure setPenColor(c: TNeemaColour);
    function getPenColor: TNeemaColour;
    procedure MoveTo(X, Y: integer);
    procedure TransFormPoint(var X, Y: integer);
    procedure AngleArc(X, Y: integer; Radius: longword; StartAngle, SweepAngle: single);
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: integer);
    procedure WriteToMemo(Text: string);
    procedure Clear;
    procedure SetTransform(CXT, CYT: integer);
  end;

const
  clNemAqua = TNeemaColour($FFFF00);
  clYellow = TNeemaColour($00FFFF);
  clNemBlue = TNeemaColour($FF0000);
  clNemRed = TNeemaColour($0000FF);
  clNemBlack = TNeemaColour($000000);
  clNemWhite = TNeemaColour($FFFFFF);
  ClNemSilver = TNeemaColour($C0C0C0);
  clNemPurple = TNeemaColour($800080);
  clNemFuchsia = TNeemaColour($FF00FF);

implementation

end.
