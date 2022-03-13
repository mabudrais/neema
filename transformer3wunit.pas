unit TransFormer3WUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, UtilUnit, SEMeasurementUnit,
  neemacanvasUnit, drawingutilunit;

type

  { TTransFormer3W }

  TTransFormer3W = class Sealed(TElicment)
  private
    FCB1Close, FCB2Close, FCB3Close: boolean;
    FMinTap, FMAXTap, FMedTap, FTap: integer;
    FTapStep: string;
    FPrimeryBaseVolt, FSecendryBaseVolt, FTertiaryBaseVolt: string;
    FPerUnitPrimeryR, FPerUnitPrimeryX, FPerUnitSecondaryR, FPerUnitSecondaryX: string;
    FPerUnitTertiaryR, FPerUnitTertiaryX: string;
    FShuntVA: string;
    procedure SetTap(AValue: integer);
  public
    M: TTrans3wMeasurmen;
    Circle1Color, Circle2Color, Circle3Color, CB1Color, CB2Color, CB3Color: TNeemaColour;
    constructor Create; override;
    destructor Destroy; override;
    procedure GetPiModelImpPerUnit(out oz1, oz2, oz3: complex);
    procedure GetPrimeryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
    procedure GetSecondryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
    procedure GetTertiaryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
    function isTwoSideOpen(): boolean;
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
    //x or i ?
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
    property CB3Close: boolean read FCB3Close write FCB3Close;
    property ShuntVA: string read FShuntVA write FShuntVA;//VA not MVA
  end;

implementation

procedure TTransFormer3W.SetTap(AValue: integer);
begin
  if FTap = AValue then
    Exit;
  FTap := AValue;
end;

function TTransFormer3W.isTwoSideOpen(): boolean;
begin
  Result := ((not CB1Close) and (not CB2Close)) or ((not CB1Close) and (not CB3Close)) or
    ((not CB2Close) and (not CB3Close));
end;

procedure TTransFormer3W.GetPrimeryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
var
  Leg1, Leg2, RD, XD, dv, VPrimery, a: double;
begin
  if not ((Tap >= MinTap) and (Tap <= MAXTap) and (MAXTap <> MinTap)) then
    raise Exception.Create('Error in tap setting');
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
end;

procedure TTransFormer3W.GetSecondryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
var
  Leg1, Leg2, RD, XD, dv, a: double;
begin
  a := 1;
  Leg1 := infto13(a, (a - 1));
  Leg2 := infto13((a * a), (1 - a));
  RD := PerUnitSecondaryR.ToDouble;
  XD := PerUnitSecondaryX.ToDouble;
  Oz1 := cinit(RD, XD) * Leg1;
  Oz2 := cinit(RD, XD) * a;
  Oz3 := cinit(RD, XD) * Leg2;
end;

procedure TTransFormer3W.GetTertiaryWindingPiModelImpPerUnit(out oz1, oz2, oz3: complex);
var
  Leg1, Leg2, RD, XD, dv, a: double;
begin
  a := 1;
  Leg1 := infto13(a, (a - 1));
  Leg2 := infto13((a * a), (1 - a));
  RD := PerUnitTertiaryR.ToDouble;
  XD := PerUnitTertiaryX.ToDouble;
  Oz1 := cinit(RD, XD) * Leg1;
  Oz2 := cinit(RD, XD) * a;
  Oz3 := cinit(RD, XD) * Leg2;
end;

procedure TTransFormer3W.GetPiModelImpPerUnit(out oz1, oz2, oz3: complex);
var
  Leg1, Leg2, RD, XD, dv, VPrimery, a: double;
  St3, St1, St2, da, db, dc: complex;
begin
  if not ((Tap >= MinTap) and (Tap <= MAXTap) and (MAXTap <> MinTap)) then
    raise Exception.Create('Error in tap setting');
  if (not CB1Close) then
    raise Exception.Create('transformer ' + Name + ' CB1 open');
  if ((not CB2Close) and (not CB3Close)) then
    raise Exception.Create(' GetPiModelImpPerUnit() can not be used in transformer ' +
      Name + 'only cb1 breaker are close');
  if ((CB2Close) and (CB3Close)) then
    raise Exception.Create(' GetPiModelImpPerUnit() can not be used in transformer ' +
      Name + 'all breaker are close');
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
  //ShuntImp := (TertiaryBaseVolt.ToDouble * TertiaryBaseVolt.ToDouble) / ShuntVA.ToDouble;
  //ShuntBaseImp := (TertiaryBaseVolt.ToDouble * TertiaryBaseVolt.ToDouble)  / BaseMVA.ToDouble;
  St3 := oz2;
  St1 := oz3;
  if CB2Close then
    St2 := cinit(PerUnitSecondaryR.ToDouble, PerUnitSecondaryX.ToDouble)
  else
    St2 := cinit(PerUnitTertiaryR.ToDouble, PerUnitTertiaryX.ToDouble);
  // ShuntImpPerunit := ShuntImp / ShuntBaseImp;
  //if ShuntCB then
  //St1 := 1 / (1 / St1 + 1 / (Limp + ShuntImpPerunit * i));
  StarToDelta(St1, st2, st3, da, db, dc);
  db := 1 / (1 / db + 1 / oz1);
  oz1 := db;
  oz2 := da;
  oz3 := dc;
end;

constructor TTransFormer3W.Create;
begin
  inherited Create;
  m := TTrans3wMeasurmen.Create('');
  IniServerDataString();
  CB1Close := True;
  CB2Close := True;
  CB3Close := True;
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

destructor TTransFormer3W.Destroy;
begin
  inherited Destroy;
  m.Free;
end;

procedure TTransFormer3W.SetData(cPPR, cPPX, cPSR, cPSX, cPTR, cPTX,
  cPBV, cSBV, cTBV: string);
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

procedure TTransFormer3W.SetTapData(cTapStep: string;
  cTap, cMinTap, cMedTap, cMaxTap: integer);
begin
  TapStep := cTapStep;
  Tap := cTap;
  MinTap := cMinTap;
  MedTap := cMedTap;
  MAXTap := cMaxTap;
end;

function TTransFormer3W.GetMaxConectioNum(): integer;
begin
  Result := 3;
end;

procedure TTransFormer3W.Draw(const c: INeemaCanvas; Tick: boolean);
begin
  DrawCB(c, XPos, YPos - 2 * CIRCLERADUIS, CB1Close, CB1Color);
  DrawCB(c, XPos + round(CIRCLERADUIS / 2) + 2, YPos + 2 * CIRCLERADUIS,
    CB2Close, CB2Color);
  DrawCB(c, XPos - round(CIRCLERADUIS / 2) - 2, YPos + 2 * CIRCLERADUIS,
    CB3Close, CB3Color);
  if (IsSelected) and (Tick) then
  begin
    DrawTSC(c, XPos, YPos - round(CIRCLERADUIS / 2), clNemBlack);
    DrawTSC(c, XPos + round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
      clNemBlack);
    DrawTSC(c, XPos - round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
      clNemBlack);
  end;
  DrawTC(c, XPos, YPos - round(CIRCLERADUIS / 2), Circle1Color);
  DrawTC(c, XPos + round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
    Circle2Color);
  DrawTC(c, XPos - round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2),
    Circle3Color);
end;

function TTransFormer3W.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    T3WConectionPos(Conections.Count, XPos, YPos, ConX, ConY);
  end;
end;

function TTransFormer3W.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;

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

function TTransFormer3W.SetSelected(X, Y: integer): boolean;
begin
  IsSelected := IsINCircle(X, Y, XPos, YPos - round(CIRCLERADUIS / 2), CIRCLERADUIS) or
    IsINCircle(X, Y, XPos + round(CIRCLERADUIS / 2), YPos +
    round(CIRCLERADUIS / 2), CIRCLERADUIS) or IsINCircle(X, Y, XPos +
    round(CIRCLERADUIS / 2), YPos + round(CIRCLERADUIS / 2), CIRCLERADUIS);
  Result := IsSelected;
end;

function TTransFormer3W.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

end.
