unit SVCUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, neemacanvasUnit,
  UtilUnit, drawingutilunit;

type

  { TSVC }

  TSVC = class(TElicment)
  private
    FTransformerPerUnitR, FTransformerPerUnitX: string;
    FCB1Close, FCB2Close: boolean;
    FTransformerMinTap, FTransformerMAXTap, FTransformerMedTap, FTransformerTap: integer;
    FTransformerTapStep: string;
    FTransformerPrimeryBaseVolt, FTransformerSecendryBaseVolt: string;
    FHasTransformer: boolean;
    FMaxCompensationVar: string;
    FMinCompensationVar: string;
    FFixedVar: string;
    FTargetVolt: string;
    FVoltEpsilon: string;
    procedure INIDefaultValues();
    procedure SetTap(AValue: integer);
  public
    // M: TTrans2wMeasurmen;
    Circle1Color, Circle2Color, CB1Color, CB2Color: integer;
    constructor Create; override;
    procedure GetAdmitancePerUnit(out MaxY, MinY: complex);
    procedure SetTransformerData(cPerUnitR, cPerUnitX, cFPrimeryBaseVolt,
      cSecendryBaseVolt: string);
    procedure SetTransformerTapData(cTapStep: string;
      cTap, cMinTap, cMedTap, cMaxTap: integer);
    function GetMaxConectioNum(): integer; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
  published
    property TransformerPerUnitR: string read FTransformerPerUnitR
      write FTransformerPerUnitR;
    property TransformerPerUnitX: string read FTransformerPerUnitX
      write FTransformerPerUnitX;
    property TransformerPrimeryBaseolt: string
      read FTransformerPrimeryBaseVolt write FTransformerPrimeryBaseVolt;
    property TransformerSecendryBaseVolt: string
      read FTransformerSecendryBaseVolt write FTransformerSecendryBaseVolt;
    property TransformerMinTap: integer read FTransformerMinTap write FTransformerMinTap;
    property TransformerMAXTap: integer read FTransformerMAXTap write FTransformerMAXTap;
    property TransformerMedTap: integer read FTransformerMedTap write FTransformerMedTap;
    property TransformerTapStep: string read FTransformerTapStep
      write FTransformerTapStep;
    property TransformerTap: integer read FTransformerTap write SetTap;
    property CB1Close: boolean read FCB1Close write FCB1Close;
    property CB2Close: boolean read FCB2Close write FCB2Close;
    property HasTransformer: boolean read FHasTransformer write FHasTransformer;
    property MaxCompensationVar: string read FMaxCompensationVar
      write FMaxCompensationVar;
    property MinCompensationVar: string read FMinCompensationVar
      write FMinCompensationVar;
    property FixedVar: string read FFixedVar write FFixedVar;
    property TargetVolt: string read FTargetVolt write FTargetVolt;
    property VoltEpsilon: string read FVoltEpsilon write FVoltEpsilon;
  end;

implementation

{ TSVC }
procedure TSVC.SetTap(AValue: integer);
begin
  if FTransformerTap = AValue then
    Exit;
  FTransformerTap := AValue;
end;

procedure TSVC.GetAdmitancePerUnit(out MaxY, MinY: complex);//V S oz all in perunit
var
  Leg1, Leg2, RD, XD, dv, VPrimery, a: double;
  oz1, oz2, oz3, SVSDynamicImp: complex;
  RequerdQ, ABSV: real;
begin
  //this function need more error checking

  Oz1 := 1e13;
  Oz2 := 1e-13;
  Oz3 := 1e13;
  if HasTransformer then
  begin
    if not ((TransformerTap >= TransformerMinTap) and
      (TransformerTap <= TransformerMAXTap) and (TransformerMAXTap <>
      TransformerMinTap)) then
      raise Exception.Create('Error in tap setting');
    dv := TransformerTapStep.ToDouble * (TransformerTap - TransformerMedTap);
    VPrimery := StringToVolt(TransformerPrimeryBaseolt);
    a := VPrimery / (VPrimery + dv);
    Leg1 := infto13(a, (a - 1));
    Leg2 := infto13((a * a), (1 - a));
    RD := TransformerPerUnitR.ToDouble;
    XD := TransformerPerUnitX.ToDouble;
    Oz1 := cinit(RD, XD) * Leg1;
    Oz2 := cinit(RD, XD) * a;
    Oz3 := cinit(RD, XD) * Leg2;
  end;
  MaxY := cong(StringToVar(Self.MaxCompensationVar) * i / StringToVA(BaseVA));
  MaxY := MaxY + 1 / oz3;
  MaxY := SeriesAdmitance(MaxY, 1 / oz2);
  MaxY := MaxY + 1 / oz1;
  MinY := cong(StringToVar(Self.MinCompensationVar) * i / StringToVA(BaseVA));
  MinY := MinY + 1 / oz3;
  MinY := SeriesAdmitance(MinY, 1 / oz2);
  MinY := MinY + 1 / oz1;
end;

procedure TSVC.INIDefaultValues();
begin

  CB1Close := True;
  CB2Close := True;
  VoltEpsilon := '0.005';
  FTransformerPrimeryBaseVolt := '1';
  FTransformerSecendryBaseVolt := '2';
end;

constructor TSVC.Create;
begin
  inherited Create;
  INIDefaultValues();
  //M := TTrans2wMeasurmen.Create(Self.Name);
end;

procedure TSVC.SetTransformerData(cPerUnitR, cPerUnitX, cFPrimeryBaseVolt,
  cSecendryBaseVolt: string);
begin
  TransformerPerUnitR := cPerUnitR;
  TransformerPerUnitX := cPerUnitX;
  TransformerPrimeryBaseolt := cFPrimeryBaseVolt;
  TransformerSecendryBaseVolt := cSecendryBaseVolt;
end;

procedure TSVC.SetTransformerTapData(cTapStep: string;
  cTap, cMinTap, cMedTap, cMaxTap: integer);
begin
  HasTransformer := True;
  TransformerTapStep := cTapStep;
  TransformerTap := cTap;
  TransformerMinTap := cMinTap;
  TransformerMedTap := cMedTap;
  TransformerMAXTap := cMaxTap;
end;

function TSVC.GetMaxConectioNum(): integer;
begin
  Result := 1;
end;

procedure TSVC.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldColour, OldBrushColour: integer;
  OldPenColour: TNeemaColour;
begin
  if (IsSelected) and (Tick) then
  begin
    DrawTSC(c, XPos, YPos - round(CIRCLERADUIS / 2), clNemBlack);
    DrawTSC(c, XPos, YPos + round(CIRCLERADUIS / 2), clNemBlack);
  end;
  DrawCB(c, XPos, YPos - 2 * CIRCLERADUIS, CB1Close, CB1Color);
  HasTransformer := True;
  if HasTransformer then
  begin
    DrawCB(c, XPos, YPos + 2 * CIRCLERADUIS, CB2Close, CB2Color);
    DrawTC(c, XPos, YPos - round(CIRCLERADUIS / 2), Circle1Color);
    DrawTC(c, XPos, YPos + round(CIRCLERADUIS / 2), Circle2Color);
  end;
  OldBrushColour := c.getBrushColor;
  OldPenColour := c.getPenColor;
  c.setPenColor(CB2Color);
  c.setBrushColor(clNemBlack);
  if HasTransformer then
    c.Line(XPos, YPos + 2 * CIRCLERADUIS + 10, XPos, YPos + 2 * CIRCLERADUIS + 20)
  else
    c.Line(XPos, YPos - round(CIRCLERADUIS) * 2, XPos, YPos + 2 *
      CIRCLERADUIS + 20);
  c.Line(XPos - 20, YPos + 2 * CIRCLERADUIS + 20, XPos + 20, YPos +
    2 * CIRCLERADUIS + 20);
  c.Line(XPos - 20, YPos + 2 * CIRCLERADUIS + 40, XPos + 20, YPos +
    2 * CIRCLERADUIS + 40);
  c.Line(XPos - 5, YPos + 2 * CIRCLERADUIS + 20, XPos - 10, YPos +
    2 * CIRCLERADUIS + 40);
  c.Line(XPos - 20, YPos + 2 * CIRCLERADUIS + 20, XPos - 10, YPos +
    2 * CIRCLERADUIS + 40);
  c.Line(XPos + 10, YPos + 2 * CIRCLERADUIS + 20, XPos + 5, YPos +
    2 * CIRCLERADUIS + 40);
  c.Line(XPos + 10, YPos + 2 * CIRCLERADUIS + 20, XPos + 20, YPos +
    2 * CIRCLERADUIS + 40);
  // if Tick then
  //c.Rectangle(XPos-CIRCLERADUIS,YPos-2*CIRCLERADUIS,XPos+CIRCLERADUIS,YPos+4*CIRCLERADUIS);
  c.setBrushColor(OldBrushColour);
  c.setPenColor(OldPenColour);
end;

function TSVC.SetSelected(X, Y: integer): boolean;
begin
  IsSelected := isINSR(X, Y, XPos - CIRCLERADUIS, YPos - 2 * CIRCLERADUIS,
    XPos + CIRCLERADUIS, YPos + 4 * CIRCLERADUIS);
  Result := IsSelected;
end;

function TSVC.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    if Conections.Count = 0 then
      SetTwoVal(XPos, YPos - 2 * CIRCLERADUIS, ConX, ConY)
    else if Conections.Count = 1 then
      SetTwoVal(XPos, YPos + 2 * CIRCLERADUIS, ConX, ConY);
  end;
end;


end.
