unit TransFormer3WShuntUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, UtilUnit, SEMeasurementUnit,
  neemacanvasUnit
  , drawingutilunit;

type
  TBA = array of boolean;
  TIA = array of integer;
  { TTransFormer3WShunt }

  TTransFormer3WShunt = class Sealed(TElicment)
  private
    FCB1Close, FCB2Close, FShuntCB: boolean;
    FMinTap, FMAXTap, FMedTap, FTap: integer;
    FTapStep: string;
    FPrimeryBaseVolt, FSecendryBaseVolt, FTertiaryBaseVolt: string;
    FPerUnitPrimeryR, FPerUnitPrimeryX, FPerUnitSecondaryR, FPerUnitSecondaryX: string;
    FPerUnitTertiaryR, FPerUnitTertiaryX: string;
    FShuntVA: string;
    procedure SetTap(AValue: integer);
  public
    M: TTrans3wMeasurmen;
    Circle1Color, Circle2Color, Circle3Color, CB1Color, CB2Color,
    CB3Color: TNeemaColour;
    constructor Create; override;
    destructor Destroy; override;
    procedure GetPiModelImpPerUnit(out oz1, oz2, oz3: complex);
    procedure GetSPerUnit(V1PerUnit, V2PerUnit: complex; out S1, S2, S3: complex);
    procedure SetData(cPPR, cPPX, cPSR, cPSX, cPTR, cPTX, cPBV, cSBV, cTBV: string);
    procedure SetTapData(cTapStep: string; cTap, cMinTap, cMedTap, cMaxTap: integer);
    function GetMaxConectioNum(): integer; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
    function GetMeasurment(): TMeasurmen; override;
  published
    property PerUnitPrimeryR: string read FPerUnitPrimeryR write FPerUnitPrimeryR;
    property PerUnitPrimeryX: string read FPerUnitPrimeryX write FPerUnitPrimeryX;
    property PerUnitSecondaryR: string read FPerUnitSecondaryR write FPerUnitSecondaryR;
    property PerUnitSecondaryX: string read FPerUnitSecondaryX write FPerUnitSecondaryX;
    property PerUnitTertiaryR: string read FPerUnitTertiaryR write FPerUnitTertiaryR;
    property PerUnitTertiaryX: string read FPerUnitTertiaryX write FPerUnitTertiaryX;
    property PrimeryBaseolt: string read FPrimeryBaseVolt write FPrimeryBaseVolt;
    property SecendryBaseVolt: string read FSecendryBaseVolt write FSecendryBaseVolt;
    property TertiaryBaseVolt: string read FTertiaryBaseVolt write FTertiaryBaseVolt;
    property MinTap: integer read FMinTap write FMinTap;
    property MAXTap: integer read FMAXTap write FMAXTap;
    property MedTap: integer read FMedTap write FMedTap;
    property TapStep: string read FTapStep write FTapStep;
    property Tap: integer read FTap write SetTap;
    property CB1Close: boolean read FCB1Close write FCB1Close;
    property CB2Close: boolean read FCB2Close write FCB2Close;
    property ShuntCB: boolean read FShuntCB write FShuntCB;
    property ShuntVA: string read FShuntVA write FShuntVA;//VA not MVA
  end;



implementation

{ TTransFormer3WShunt }

procedure TTransFormer3WShunt.SetTap(AValue: integer);
begin
  if FTap = AValue then
    Exit;
  FTap := AValue;
end;

procedure TTransFormer3WShunt.GetPiModelImpPerUnit(out oz1, oz2, oz3: complex);
var
  Leg1, Leg2, RD, XD, dv, VPrimery, a, ShuntImp, ShuntBaseImp: double;
  ShuntImpPerunit: extended;
  Limp, St3, St1, St2, da, db, dc: complex;
begin
  if not ((Tap >= MinTap) and (Tap <= MAXTap) and (MAXTap <> MinTap)) then
    raise Exception.Create('Error in tap setting');
  if (not CB1Close) or (not CB2Close) then
    raise Exception.Create('(not Transformer.CB1Close) or (not Transformer.CB1Close)');
  dv := TapStep.ToDouble * (Tap - MedTap);
  VPrimery := StringToVolt(PrimeryBaseolt);
  a := VPrimery / (VPrimery + dv);
  Leg1 := infto13(a, (a - 1));
  Leg2 := infto13((a * a), (1 - a));
  RD := PerUnitPrimeryR.ToDouble;
  XD := PerUnitPrimeryX.ToDouble;
  Oz1 := cinit(RD, XD) * Leg1;
  Oz2 := cinit(RD, XD) * a;
  Oz3 := cinit(RD, XD) * Leg2;
  //reactor  star delta convertion
  ShuntImp := (StringToVolt(TertiaryBaseVolt) * StringToVolt(TertiaryBaseVolt)) /
    StringToVA(ShuntVA);
  ShuntBaseImp := (StringToVolt(TertiaryBaseVolt) * StringToVolt(TertiaryBaseVolt)) /
    StringToVA(BaseVA);
  St3 := oz2;
  St1 := oz3;
  St2 := cinit(PerUnitSecondaryR.ToDouble, PerUnitSecondaryX.ToDouble);
  Limp := cinit(PerUnitTertiaryR.ToDouble, PerUnitTertiaryX.ToDouble);
  ShuntImpPerunit := ShuntImp / ShuntBaseImp;
  if ShuntCB then
    St1 := 1 / (1 / St1 + 1 / (Limp + ShuntImpPerunit * i));
  StarToDelta(St1, st2, st3, da, db, dc);
  db := 1 / (1 / db + 1 / oz1);
  oz1 := db;
  oz2 := da;
  oz3 := dc;
end;

constructor TTransFormer3WShunt.Create;
begin
  inherited Create;
  M := TTrans3wMeasurmen.Create('');
  IniServerDataString();
  CB1Close := True;
  CB2Close := True;
  ShuntVA := '0.5';
  PrimeryBaseolt := '1';
  SecendryBaseVolt := '1';
  TertiaryBaseVolt := '1';
  Self.PerUnitPrimeryR := '0';
  Self.PerUnitPrimeryX := '0.5';
  Self.PerUnitSecondaryR := '0';
  Self.PerUnitSecondaryX := '0.1';
  Self.PerUnitTertiaryR := '0';
  Self.PerUnitTertiaryX := '0.5';
  TapStep := '0.001';
  Tap := 9;
  MAXTap := 19;
  MinTap := 1;
  MedTap := 9;
end;

destructor TTransFormer3WShunt.Destroy;
begin
  m.Free;
  inherited Destroy;
end;

procedure TTransFormer3WShunt.GetSPerUnit(V1PerUnit, V2PerUnit: complex;
  out S1, S2, S3: complex);
var
  V1, Del1, V2, Del2, Z1, Lamda1, Z2, Lamda2, Z3, Lamda3: double;
  oz3, oz2, oz1, VALeg1, VALeg2, TempS12: complex;
  Q12, P12: double;
  P21, Q21: real;
begin
  V1 := cmod(V1PerUnit);
  Del1 := carg(V1PerUnit);
  V2 := cmod(V2PerUnit);
  Del2 := carg(V2PerUnit);
  GetPiModelImpPerUnit(oz1, oz2, oz3);
  Z1 := cmod(oz1);
  Lamda1 := carg(oz1);
  Z2 := cmod(oz2);
  Lamda2 := carg(oz2);
  Z3 := cmod(oz3);
  Lamda3 := carg(oz3);
  TempS12 := 1 / oz2;
  TempS12 := V1PerUnit * cong((V1PerUnit - V2PerUnit) / oz2);
  P12 := V1 * V1 / Z2 * cos(Lamda2) - V1 * V2 / Z2 * cos(Lamda2 + Del1 - Del2);
  Q12 := V1 * V1 / Z2 * sin(Lamda2) - V1 * V2 / Z2 * sin(Lamda2 + Del1 - Del2);
  VALeg1 := V1PerUnit * cong(V1PerUnit / oz1);
  VALeg2 := V2PerUnit * cong(V2PerUnit / oz3);
  P12 := P12 + VALeg1.re;
  Q12 := Q12 + VALeg1.im;
  S1 := P12 + i * Q12;
  P21 := V2 * V2 / Z2 * cos(Lamda2) - V2 * V1 / Z2 * cos(Lamda2 + Del2 - Del1);
  Q21 := V2 * V2 / Z2 * sin(Lamda2) - V2 * V1 / Z2 * sin(Lamda2 + Del2 - Del1);
  TempS12 := V2 * V2 / Z2 * sin(Lamda2);
  TempS12 := -V2 * V1 / Z2 * sin(Lamda2 + Del2 - Del1);
  P21 := P21 + VALeg2.re;
  Q21 := Q21 + VALeg2.im;
  S2 := P21 + i * Q21;
end;

procedure TTransFormer3WShunt.SetData(cPPR, cPPX, cPSR, cPSX, cPTR,
  cPTX, cPBV, cSBV, cTBV: string);
begin
  PerUnitPrimeryR := cPPR;
  PerUnitPrimeryX := cPPX;
  PerUnitSecondaryR := cPSR;
  PerUnitSecondaryX := cPSX;
  PerUnitTertiaryR := cPTR;
  PerUnitTertiaryX := cPTX;
  PrimeryBaseolt := cPBV;
  SecendryBaseVolt := cSBV;
  TertiaryBaseVolt := cTBV;
end;

procedure TTransFormer3WShunt.SetTapData(cTapStep: string;
  cTap, cMinTap, cMedTap, cMaxTap: integer);
begin
  TapStep := cTapStep;
  Tap := cTap;
  MinTap := cMinTap;
  MedTap := cMedTap;
  MAXTap := cMaxTap;
end;

function TTransFormer3WShunt.GetMaxConectioNum(): integer;
begin
  Result := 2;
end;

procedure TTransFormer3WShunt.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldColour: integer;
begin
  DrawCB(c, XPos, YPos - 2 * CIRCLERADUIS, CB1Close, CB1Color);
  DrawCB(c, XPos + round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS,
    CB2Close, CB2Color);
  DrawCB(c, XPos - round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS,
    ShuntCB, CB3Color);
  if (IsSelected) and (Tick) then
  begin
    DrawTSC(c, XPos, YPos - round(CIRCLERADUIS / 2), clNemBlack);
    DrawTSC(c, XPos + round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
      clNemBlack);
    DrawTSC(c, XPos - round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
      clNemBlack);
  end;
  DrawTC(c, XPos, YPos - round(CIRCLERADUIS / 2), Circle1Color);
  DrawTC(c, XPos + round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2), Circle2Color);
  DrawTC(c, XPos - round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2), Circle3Color);
  DrawRC(c, XPos - round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS +
    CBHALFWIDTH * 2, CB3Color);
  c.Line(XPos - round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS +
    CBHALFWIDTH, XPos - round(CIRCLERADUIS / 2), YPos + 2 * CIRCLERADUIS +
    CBHALFWIDTH * 2);
end;


function TTransFormer3WShunt.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    T3WConectionPos(Conections.Count, XPos, YPos, ConX, ConY);
  end;
end;

function TTransFormer3WShunt.GetConectionPos(cID: integer;
  out ConX, ConY: integer): boolean;
var
  Index: integer;
begin
  Result := False;
  Index := Conections.IndexOf(cID);
  if Index > -1 then
  begin
    T3WConectionPos(Index, XPos, YPos, ConX, ConY);
    Result := True;
  end;
end;

function TTransFormer3WShunt.SetSelected(X, Y: integer): boolean;
begin
  IsSelected := IsINCircle(X, Y, XPos, YPos - round(CIRCLERADUIS / 2), CIRCLERADUIS) or
    IsINCircle(X, Y, XPos + round(CIRCLERADUIS / 2), YPos +
    round(CIRCLERADUIS / 2), CIRCLERADUIS) or IsINCircle(X, Y, XPos +
    round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2), CIRCLERADUIS);
  Result := IsSelected;
end;

function TTransFormer3WShunt.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

end.
