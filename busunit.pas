unit BusUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, neemacanvasUnit,
  SEMeasurementUnit, UtilUnit, busCalculatedVoltageintefaceunit;

type
  TBusType = (slackbus, regulatingbus, loadbus);

  { TBus }

  TBus = class(TElicment, IBCVI)
  private
    FBusType: Tbustype;
    FBusVoltageR, FBusVoltagei, FInjectedP, FInjectedQ: string;
    FCalculatedVoltage: complex;
    FMaxRegulationQ, FMinRegulationQ: string;
    function GetPerUnitVolt: complex;
    function GetPerUnitPQ: complex;
    procedure SetCalculatedVoltage(V: complex);
  public
    M: TBusmeasurmen;
    BusColor: TNeemaColour;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function GCVPerUnit(aID: integer): complex;
    function GCVByIndex(aIndex: integer; out V: complex): boolean;
    function GIName(): string;
    procedure SetData(V, PQ: complex; VB, MVAB: double; T: TBusType);
    procedure SetPerUnitPQ(AValue: complex);
    function GetMaxConectioNum(): integer; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
      override;
    function SetSelected(X, Y: integer): boolean; override;
    property PerUnitVolt: complex read GetPerUnitVolt;
    property PerUnitPQ: complex read GetPerUnitPQ write SetPerUnitPQ;
    property CalculatedVoltage: complex read FCalculatedVoltage
      write SetCalculatedVoltage;
    function GetMeasurment(): TMeasurmen; override;
  published
    property BusType: Tbustype read FBusType write FBusType;
    property BusVoltageR: string read FBusVoltageR write FBusVoltageR;
    property BusVoltageI: string read FBusVoltagei write FBusVoltagei;
    property InjectedP: string read FInjectedP write FInjectedP;
    property InjectedQ: string read FInjectedQ write FInjectedQ;
    property MaxRegulationQ: string read FMaxRegulationQ write FMaxRegulationQ;
    property MinRegulationQ: string read FMinRegulationQ write FMinRegulationQ;
  end;

const
  BayDistansc = 50;
  MinBusLength = 50;

implementation

{ TBus }

function TBus.GetPerUnitVolt: complex;
begin
  Result := cinit(StringToVolt(BusVoltageR), StringToVolt(BusVoltageI)) /
    StringToVolt(BaseVolt);
end;

function TBus.GetPerUnitPQ: complex;
begin
  Result := cinit(StringToWat(InjectedP), StringToVar(InjectedQ)) / (StringToVA(BaseVA));
end;

function TBus.GetMaxConectioNum(): integer;
begin
  Result := 50;
end;

procedure TBus.SetData(V, PQ: complex; VB, MVAB: double; T: TBusType);
begin
  BusVoltageR := FloatToStr(v.re);
  BusVoltageI := FloatToStr(v.im);
  InjectedP := FloatToStr(PQ.re);
  InjectedQ := FloatToStr(PQ.im);
  BaseVolt := VB.ToString();
  BaseVA := MVAB.ToString();
  BusType := T;
  CalculatedVoltage := cinit(StringToVolt(BusVoltageR), StringToVolt(BusVoltageI));
end;

procedure TBus.SetCalculatedVoltage(V: complex);
begin
  FCalculatedVoltage := V;
end;

constructor TBus.Create;
begin
  inherited Create;
  M := TBusmeasurmen.Create('');
  IniServerDataString();
  BusType := loadbus;
  BusVoltageR := '1';
  BusVoltageI := '0';
  InjectedP := '0';
  InjectedQ := '0';
  MaxRegulationQ := '0';
  MinRegulationQ := '0';
end;

destructor TBus.Destroy;
begin
  m.Free;
  inherited Destroy;
end;

procedure TBus.Draw(const c: INeemaCanvas; Tick: boolean);
var
  BusLength, k, OldBrushColour: integer;
  OldPenColour: TNeemaColour;
begin
  inherited Draw(c, Tick);
  OldBrushColour := c.getBrushColor;
  OldPenColour := c.getPenColor;
  c.setPenColor(clNemBlack);
  BusLength := BayDistansc * Conections.Count;
  if Conections.Count < 1 then
    BusLength := MinBusLength;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(Self.XPos - 3, YPos - 3, XPos + BusLength + 3, YPos + 6);
  end;
  c.setBrushColor(BusColor);
  c.FillRect(Self.XPos, YPos, XPos + BusLength, YPos + 3);
  for k := 0 to Conections.Count - 1 do
    c.EllipseC(XPos + k * BayDistansc, YPos, 5, 5);
  c.setBrushColor(OldBrushColour);
  c.TextOut(XPos + BusLength, YPos, VoltToReadableString(CalculatedVoltage));
  c.setPenColor(OldPenColour);
end;

function TBus.GCVPerUnit(aID: integer): complex;
begin
  if Conections.IndexOf(aID) < 0 then
    raise Exception.Create('not conected to bus');
  Result := CalculatedVoltage / StringToVolt(BaseVolt);
end;

function TBus.GCVByIndex(aIndex: integer; out V: complex): boolean;
begin
  if aIndex <> 0 then Exit(False);
  V := CalculatedVoltage;
  Result := True;
end;

function TBus.GIName: string;
begin
  Result := Name;
end;

function TBus.SetSelected(X, Y: integer): boolean;
var
  BusLength: integer;
begin
  Result := False;
  BusLength := BayDistansc * Conections.Count;
  if Conections.Count < 1 then
    BusLength := MinBusLength;
  if ((X > XPos) and (X < XPos + BusLength) and (Y < YPos + 5) and
    (Y > YPos - 5)) then
  begin
    IsSelected := True;
    Result := True;
  end;
end;

function TBus.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

procedure TBus.SetPerUnitPQ(AValue: complex);
begin
  Injectedp := FloatToStr(AValue.re * StringToVA(BaseVA));
  InjectedQ := FloatToStr(AValue.im * StringToVA(BaseVA));
end;

function TBus.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    ConX := XPos + BayDistansc * Conections.Count;
    ConY := YPos;
  end;
end;

function TBus.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
var
  ConectionIndex: integer;
begin
  ConectionIndex := Conections.IndexOf(cID);
  if ConectionIndex < 0 then
    Exit(False);
  ConX := XPos + BayDistansc * ConectionIndex;
  ConY := YPos;
  Result := True;
end;

end.
