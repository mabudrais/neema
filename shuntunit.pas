unit ShuntUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UtilUnit, UComplex, SEMeasurementUnit
  , neemacanvasUnit, drawingutilunit;

type

  { TShunt }

  TShunt = class(TElicment)
  private
    FCBClose: boolean;
    FShuntVAR: string;
  public
    M: TShuntMeasurmen;
    Color: integer;
    constructor Create; override;
    destructor Destroy; override;
    function GetMaxConectioNum(): integer; override;
    function GetXPerUnit(): complex;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
  published
    property ShuntVAR: string read FShuntVAR write FShuntVAR;
    property CBClose: boolean read FCBClose write FCBClose;
  end;

implementation

{ TShunt }

constructor TShunt.Create;
begin
  inherited Create;
  M := TShuntMeasurmen.Create('');
  BaseVA := '100e6';
  BaseVolt := '220e6';
  ShuntVAR := '1';
  CBClose := True;
  Color := clNemBlack;
end;

destructor TShunt.Destroy;
begin
  M.Free;
  inherited Destroy;
end;

function TShunt.GetMaxConectioNum(): integer;
begin
  Result := 1;
end;

function TShunt.GetXPerUnit(): complex;
var
  BVA, cVar, BV, BaseImp: extended;
begin
 BVA:= StringToVA(BaseVA);
  StringToVolt(BaseVolt, BV);
  StringToVar(ShuntVAR, cVar);
  BaseImp := BV * BV / BVA;
  Result := (BV / (cong(cVar * i / BV))) / BaseImp;
end;

procedure TShunt.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldPenColor: TNeemaColour;
  OldBrushColour: integer;
begin
  OldPenColor := C.getPenColor;
  OldBrushColour := c.getBrushColor;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(XPos - round(CIRCLERADUIS), YPos - round(CIRCLERADUIS / 2),
      XPos + round(CIRCLERADUIS / 2),
      YPos + CBHALFWIDTH * 4);
  end;
  DrawCB(c, XPos - round(CIRCLERADUIS / 2), YPos, CBClose, Color);
  DrawRC(c, XPos - round(CIRCLERADUIS / 2), YPos + CBHALFWIDTH * 2, Color);
  c.setPenColor(Color);
  c.Line(XPos - CBHALFWIDTH, YPos + CBHALFWIDTH, XPos - CBHALFWIDTH,
    YPos + CBHALFWIDTH * 2);
  C.setPenColor(OldPenColor);
  c.setBrushColor(OldBrushColour);
end;

function TShunt.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
  if isINSR(X, Y, XPos - round(CIRCLERADUIS), YPos - round(CIRCLERADUIS / 2),
    XPos + round(CIRCLERADUIS / 2), YPos + CBHALFWIDTH * 4) then
  begin
    IsSelected := True;
    Result := True;
  end;
end;

end.
