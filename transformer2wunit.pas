unit TransFormer2WUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, elementUnit, ElectricElentUnit, UComplex, UtilUnit,
  SEMeasurementUnit, neemacanvasUnit, drawingutilunit,PiInterfaceUnit;

type

  { TTransFormer2W }

  TTransFormer2W = class(TElicment, IPI)
  private
    FPerUnitR, FPerUnitX: string;
    FCB1Close, FCB2Close: boolean;
    FMinTap, FMAXTap, FMedTap, FTap: integer;
    FTapStep: string;
    FPrimeryBaseVolt, FSecendryBaseVolt: string;
    procedure GetConectionPos2(Index: integer; out ConX, ConY: integer);
    procedure SetTap(AValue: integer);
  public
    M: TTrans2wMeasurmen;
    Circle1Color, Circle2Color, CB1Color, CB2Color: integer;
    procedure BussIDs(out aID1, aID2: integer);
    procedure CBSatatus(out CB1, CB2: boolean);
    constructor Create; override;
    destructor Destroy; override;
    procedure GetImpPerUnit(out oz1, oz2, oz3: complex);
    function PISelf: TObject;
    procedure SetData(cPerUnitR, cPerUnitX, cFPrimeryBaseVolt,
      cSecendryBaseVolt: string);
    procedure SetTapData(cTapStep: string; cTap, cMinTap, cMedTap, cMaxTap: integer);
    function GetMaxConectioNum(): integer; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
      override;
    procedure OpenAllCB(); override;
    function GetMeasurment(): TMeasurmen; override;
  published
    property PerUnitR: string read FPerUnitR write FPerUnitR;
    property PerUnitX: string read FPerUnitX write FPerUnitX;
    property PrimeryBaseolt: string read FPrimeryBaseVolt write FPrimeryBaseVolt;
    property SecendryBaseVolt: string read FSecendryBaseVolt write FSecendryBaseVolt;
    property MinTap: integer read FMinTap write FMinTap;
    property MAXTap: integer read FMAXTap write FMAXTap;
    property MedTap: integer read FMedTap write FMedTap;
    property TapStep: string read FTapStep write FTapStep;
    property Tap: integer read FTap write SetTap;
    property CB1Close: boolean read FCB1Close write FCB1Close;
    property CB2Close: boolean read FCB2Close write FCB2Close;
  end;

implementation

{ TTransFormer2W }

procedure TTransFormer2W.SetTap(AValue: integer);
begin
  if FTap = AValue then
    Exit;
  FTap := AValue;
end;

procedure TTransFormer2W.GetImpPerUnit(out oz1, oz2, oz3: complex);
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
  RD := PerUnitR.ToDouble;
  XD := PerUnitX.ToDouble;
  Oz1 := cinit(RD, XD) * Leg1;
  Oz2 := cinit(RD, XD) * a;
  Oz3 := cinit(RD, XD) * Leg2;
end;

function TTransFormer2W.PISelf: TObject;
begin
 Result:=Self;
end;

constructor TTransFormer2W.Create;
begin
  inherited Create;
  CB1Close := True;
  CB2Close := True;
  M := TTrans2wMeasurmen.Create(Self.Name);
  IniServerDataString();
  Circle1Color := clNemBlack;
  Circle2Color := clNemBlack;
  PrimeryBaseolt := '1';
  SecendryBaseVolt := '1';
  PerUnitR := '0';
  PerUnitX := '0.5';
  TapStep := '0.001';
  Tap := 9;
  MAXTap := 19;
  MinTap := 1;
  MedTap := 9;
end;

procedure TTransFormer2W.BussIDs(out aID1, aID2: integer);
begin
    aID1 := -1;
    aID2 := -1;
    if Conections.Count > 0 then
      aID1 := Conections[0];
    if Conections.Count > 1 then
      aID2 := Conections[1];
end;

procedure TTransFormer2W.CBSatatus(out CB1, CB2: boolean);
begin
  CB1 := CB1Close;
  CB2 := CB2Close;
end;

destructor TTransFormer2W.Destroy;
begin
  M.Free;
  inherited Destroy;
end;

procedure TTransFormer2W.SetData(cPerUnitR, cPerUnitX, cFPrimeryBaseVolt,
  cSecendryBaseVolt: string);
begin
  PerUnitR := cPerUnitR;
  PerUnitX := cPerUnitX;
  PrimeryBaseolt := cFPrimeryBaseVolt;
  SecendryBaseVolt := cSecendryBaseVolt;
end;

procedure TTransFormer2W.SetTapData(cTapStep: string;
  cTap, cMinTap, cMedTap, cMaxTap: integer);
begin
  TapStep := cTapStep;
  Tap := cTap;
  MinTap := cMinTap;
  MedTap := cMedTap;
  MAXTap := cMaxTap;
end;

function TTransFormer2W.GetMaxConectioNum(): integer;
begin
  Result := 2;
end;

procedure TTransFormer2W.Draw(const c: INeemaCanvas; Tick: boolean);
begin
  if (IsSelected) and (Tick) then
  begin
    DrawTSC(c, XPos, YPos - round(CIRCLERADUIS / 2), clNemBlack);
    DrawTSC(c, XPos, YPos + round(CIRCLERADUIS / 2), clNemBlack);
  end;
  DrawCB(c, XPos, YPos - 2 * CIRCLERADUIS, CB1Close, CB1Color);
  DrawCB(c, XPos, YPos + 2 * CIRCLERADUIS, CB2Close, CB2Color);
  DrawTC(c, XPos, YPos - round(CIRCLERADUIS / 2), Circle1Color);
  DrawTC(c, XPos, YPos + round(CIRCLERADUIS / 2), Circle2Color);
end;

function TTransFormer2W.SetSelected(X, Y: integer): boolean;
begin
  IsSelected := isINSR(X, Y, XPos - CIRCLERADUIS, YPos - 2 * CIRCLERADUIS,
    XPos + CIRCLERADUIS, YPos + 2 * CIRCLERADUIS);
  Result := IsSelected;
end;

function TTransFormer2W.GetNextConectionPos(out ConX, ConY: integer): boolean;
var
  ConectionPosIndex: integer;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    ConectionPosIndex := Conections.Count;
    GetConectionPos2(ConectionPosIndex, ConX, ConY);
  end;
end;

procedure TTransFormer2W.GetConectionPos2(Index: integer; out ConX, ConY: integer);
begin
  if (Index > 1) or (Index < 0) then
    raise Exception.Create('TTransFormer2W wrong conection index ');
  if Index = 0 then
    SetTwoVal(XPos, YPos - 2 * CIRCLERADUIS, ConX, ConY)
  else if Index = 1 then
    SetTwoVal(XPos, YPos + 2 * CIRCLERADUIS, ConX, ConY);
end;

function TTransFormer2W.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
var
  Index: integer;
begin
  Result := False;
  Index := Conections.IndexOf(cID);
  if Index > -1 then
  begin
    GetConectionPos2(Index, ConX, ConY);
    Result := True;
  end;
end;

procedure TTransFormer2W.OpenAllCB();
begin
  CB1Close := False;
  CB2Close := False;
end;

function TTransFormer2W.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

end.
