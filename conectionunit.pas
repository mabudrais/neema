unit ConectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit,neemacanvasUnit;

type

  { TConection }

  TConection = class(TElicment)
  private
    FID1: integer;
    FID2: integer;
    FX1: integer;
    FY1: integer;
    FX2: integer;
    FY2: integer;
  public
    procedure Ini(cID1, cID2, cx1, cy1, cx2, cy2: integer);
    procedure Draw(const c: INeemaCanvas;Tick:Boolean); override;
  published
    property ID1: integer read FID1 write FID1;
    property ID2: integer read FID2 write FID2;
    property X1: integer read FX1 write FX1;
    property Y1: integer read FY1 write FY1;
    property X2: integer read FX2 write FX2;
    property Y2: integer read FY2 write FY2;
  end;

implementation

procedure TConection.Ini(cID1, cID2, cx1, cy1, cx2, cy2: integer);
begin
  ID1 := cID1;
  ID2 := cID2;
  X1 := cx1;
  Y1 := cy1;
  X2 := cx2;
  Y2 := cy2;
end;

procedure TConection.Draw(const c: INeemaCanvas;Tick:Boolean);
begin
  inherited Draw(c,Tick);
  c.setPenColor(clNemBlack);
  c.Line(X1,Y1,X2,Y2);
end;

{ TConection }


end.
