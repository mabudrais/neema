unit multibusunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenircDataUnit, ElectricElentUnit, BusUnit,
  UComplex, SEMeasurementUnit, busCalculatedvoltageintefaceunit,
  neemacanvasUnit, drawingutilunit, UtilUnit;

type
  { TBus }

  { TMultiBus }

  TMultiBus = class(TElicment, IBCVI)
  private
    FBus1Type, FBus2Type: Tbustype;
    FBus1VoltageR, FBus1Voltagei, FInjected1P, FInjected1Q: string;
    FBus2VoltageR, FBus2Voltagei, FInjected2P, FInjected2Q: string;
    FBC12Closed: boolean;
    CalculatedVoltages: array of complex;
    FMaxRegulation1Q: string;
    FMaxRegulation2Q: string;
    FMinRegulation1Q: string;
    FMinRegulation2Q: string;
    FSubBussNames: TStringList;
    SubBusIndexs: TIntegerList;
    function GetSubBusIndexsStr: string;
    procedure SetSubBusIndexsStr(AValue: string);
    function getCalculatedVoltage(index: integer): complex;
    procedure SetCalculatedVoltage(index: integer; AValue: complex);
    function GetBusLength: integer;
  public
    SubBusDead: array of boolean;
    IsSubOByEE: array of boolean;
    M: TMultiBusmeasurmen;
    Bus1Color, Bus2Color: TNeemaColour;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function GCVPerUnit(aID: integer): complex;
    function GCVByIndex(aIndex: integer; out V: complex): boolean;
    function GIName(): String;
    function SetSelected(X, Y: integer): boolean; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean; override;
    procedure AddConection(ConID: integer); override;
    procedure RemoveConection(ConID: integer); override;
    function GetMaxConectioNum(): integer; override;
    function GetSubBusIndex(EID: integer): integer;
    function GetSubBusIndexByIndex(Index: integer): integer;
    procedure SetSubBusIndex(EID, Index: integer);
    procedure SetSubBusIndexByIndex(IsoIndex, Index: integer);
    function ActiveSubBusNum(): integer;
    procedure GetActiveBusDataPerUnit(index: integer; out T: TBusType;
      out PerUnitV, PerUnitPQ: complex; out SubBusName: string);
    procedure GetRegulationQLimitPerUnit(index: integer;
      out MaxRegulationQ, MinRegulationQ: double);
    procedure SetActiveBusData(index: integer; T: TBusType; V, PQ: complex;
      SubBusName: string);
    procedure SetActiveBusPerUnitPQ(index: integer; PQ: complex);
    property CalculatedVoltage[index: integer]: complex
      read getCalculatedVoltage write SetCalculatedVoltage;
    function GetMeasurment(): TMeasurmen; override;
  published
    property Bus1Type: Tbustype read FBus1Type write FBus1Type;
    property Bus1VoltageR: string read FBus1VoltageR write FBus1VoltageR;
    property Bus1VoltageI: string read FBus1Voltagei write FBus1Voltagei;
    property Injected1P: string read FInjected1P write FInjected1P;
    property Injected1Q: string read FInjected1Q write FInjected1Q;
    property MaxRegulation1Q: string read FMaxRegulation1Q write FMaxRegulation1Q;
    property MinRegulation1Q: string read FMinRegulation1Q write FMinRegulation1Q;
    //bus2
    property Bus2Type: Tbustype read FBus2Type write FBus2Type;
    property Bus2VoltageR: string read FBus2VoltageR write FBus2VoltageR;
    property Bus2VoltageI: string read FBus2Voltagei write FBus2Voltagei;
    property Injected2P: string read FInjected2P write FInjected2P;
    property Injected2Q: string read FInjected2Q write FInjected2Q;
    property BC12Closed: boolean read FBC12Closed write FBC12Closed;
    property MaxRegulation2Q: string read FMaxRegulation2Q write FMaxRegulation2Q;
    property MinRegulation2Q: string read FMinRegulation2Q write FMinRegulation2Q;

    property SubBusIndexsStr: string read GetSubBusIndexsStr write SetSubBusIndexsStr;
    property SubBussNames: TStringList read FSubBussNames write FSubBussNames;
  end;

const
  MaxSubBusNum = 2;
  MinBusLength = 50;
  BayDistansc = 50;
  BayHeigth = 20;
  BusThiknes = 3;
  SubBusDistance = 20;

implementation

{ TMultiBus }

constructor TMultiBus.Create;
var
  k: integer;
begin
  inherited Create;
  M := TMultiBusmeasurmen.Create('');
  IniServerDataString();
  SubBusIndexs := TIntegerList.Create;
  SubBussNames := TStringList.Create;
  for k := 0 to MaxSubBusNum - 1 do
    SubBussNames.Add('tempSubus,' + k.ToString);
  SetLength(CalculatedVoltages, MaxSubBusNum);
  setlength(SubBusDead, MaxSubBusNum);
  SetLength(IsSubOByEE, MaxSubBusNum);
  Bus1Type := loadbus;
  Bus2Type := loadbus;
  Bus1VoltageR := '1';
  Bus1VoltageI := '0';
  Injected1P := '0';
  Injected1Q := '0';
  MaxRegulation1Q := '0';
  MinRegulation1Q := '0';
  Bus2VoltageR := '1';
  Bus2VoltageI := '0';
  Injected2P := '0';
  Injected2Q := '0';
  MaxRegulation2Q := '0';
  MinRegulation2Q := '0';
end;

destructor TMultiBus.Destroy;
begin
  SubBusIndexs.Free;
  SubBussNames.Free;
  inherited Destroy;
end;

function TMultiBus.GetSubBusIndexsStr: string;
var
  Comma: string;
  k: integer;
begin
  Comma := '';
  Result := '';
  for k := 0 to SubBusIndexs.Count - 1 do
  begin
    Result := Result + Comma + IntToStr(SubBusIndexs[k]);
    Comma := ',';
  end;
end;

procedure TMultiBus.SetSubBusIndexsStr(AValue: string);
var
  Data: TStringArray;
  k: integer;
begin
  Data := AValue.Split(',');
  for k := 0 to Length(Data) - 1 do
    SubBusIndexs.Add(StrToInt(Data[k]));
end;

function TMultiBus.ActiveSubBusNum(): integer;
begin
  if BC12Closed then
    Result := 1
  else
    Result := 2;
end;

function TMultiBus.getCalculatedVoltage(index: integer): complex;
begin
  if index >= MaxSubBusNum then
    raise Exception.Create('index>=MaxSubBusNum');
  Result := CalculatedVoltages[index];
end;

procedure TMultiBus.SetCalculatedVoltage(index: integer; AValue: complex);
begin
  if index >= MaxSubBusNum then
    raise Exception.Create('index>=MaxSubBusNum');
  CalculatedVoltages[index] := AValue;
  if BC12Closed then
    CalculatedVoltages[1] := AValue;
end;

procedure TMultiBus.GetActiveBusDataPerUnit(index: integer; out T: TBusType;
  out PerUnitV, PerUnitPQ: complex; out SubBusName: string);
var
  cVA, cP, cVR, cVI, cQ, cBV: extended;
begin
  if SubBussNames.Count <= index then
    raise Exception.Create('SubBussNames.Count <= index');
  SubBusName := SubBussNames[index];
  if index = 0 then
  begin
    T := Bus1Type;
    if not ((StringToVA(BaseVA, cVA)) and (StringToWat(Injected1P, cP)) and
      (StringToVar(Injected1q, cQ)) and (StringToVolt(Bus1VoltageR, cVR)) and
      (StringToVolt(Bus1VoltageI, cVI))) then
      raise Exception.Create('wrong multi bus data ' + Name);
    if not (StringToVolt(BaseVolt, cBV)) then
      raise Exception.Create('wrong multi bus data ' + Name);
    PerUnitPQ := cinit(cP, cQ) / cVA;
    PerUnitV := cinit(cVR, cVI) / cBV;
  end
  else if index = 1 then
  begin
    T := Bus2Type;
    if not ((StringToVA(BaseVA, cVA)) and (StringToWat(Injected2P, cP)) and
      (StringToVar(Injected2q, cQ)) and (StringToVolt(Bus2VoltageR, cVR)) and
      (StringToVolt(Bus2VoltageI, cVI)) and (StringToVolt(BaseVolt, cBV))) then
      raise Exception.Create('wrong multi bus data ' + Name);
    PerUnitPQ := cinit(cP, cQ) / cVA;
    PerUnitV := cinit(cVR, cVI) / cBV;
  end
  else
    raise Exception.Create('no subBus whit index ' + index.ToString);
end;

procedure TMultiBus.GetRegulationQLimitPerUnit(index: integer;
  out MaxRegulationQ, MinRegulationQ: double);
begin
  if SubBussNames.Count <= index then
    raise Exception.Create('SubBussNames.Count <= index');
  if index = 0 then
  begin
    MaxRegulationQ := (StringToVar(MaxRegulation1Q)) / StringToVA(BaseVA);
    MinRegulationQ := (StringToVar(MinRegulation1Q)) / StringToVA(BaseVA);
  end
  else if index = 1 then
  begin
    MaxRegulationQ := (StringToVar(MaxRegulation2Q)) / StringToVA(BaseVA);
    MinRegulationQ := (StringToVar(MinRegulation2Q)) / StringToVA(BaseVA);
  end
  else
    raise Exception.Create('no subBus whit index ' + index.ToString);
end;

procedure TMultiBus.SetActiveBusPerUnitPQ(index: integer; PQ: complex);
begin
  if index = 0 then
  begin
    Injected1P := FloatToStr(PQ.re * StringToVA(BaseVA));
    Injected1Q := FloatToStr(pq.im * StringToVA(BaseVA));
  end
  else
  if index = 1 then
  begin
    Injected2P := FloatToStr(PQ.re * StringToVA(BaseVA));
    Injected2Q := FloatToStr(pq.im * StringToVA(BaseVA));
  end
  else
    raise Exception.Create('no subBus whit index ' + index.ToString);
end;

function TMultiBus.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

procedure TMultiBus.SetActiveBusData(index: integer; T: TBusType;
  V, PQ: complex; SubBusName: string);
begin
  if SubBussNames.Count <= index then
    raise Exception.Create('SubBussNames.Count <= index');

  SubBussNames[index] := SubBusName;
  if index = 0 then
  begin
    Bus1Type := T;
    Injected1P := FloatToStr(PQ.re);
    Injected1Q := FloatToStr(pq.im);
    Bus1VoltageR := FloatToStr(V.re);
    Bus1VoltageI := FloatToStr(V.im);
  end
  else if index = 1 then
  begin
    Bus2Type := T;
    Injected2P := FloatToStr(PQ.re);
    Injected2Q := FloatToStr(pq.im);
    Bus2VoltageR := FloatToStr(V.re);
    Bus2VoltageI := FloatToStr(V.im);
  end
  else
    raise Exception.Create('no subBus whit index ' + index.ToString);
end;

procedure TMultiBus.SetSubBusIndexByIndex(IsoIndex, Index: integer);
begin
  SubBusIndexs[IsoIndex] := Index;
end;

procedure TMultiBus.SetSubBusIndex(EID, Index: integer);
var
  ConectionIndex: longint;
begin
  ConectionIndex := Conections.IndexOf(EID);
  if ConectionIndex < -1 then
    raise Exception.Create('Element is not conected to this bus');
  if SubBusIndexs.Count <> Conections.Count then
    raise Exception.Create('SubBusIndexs.Count<>Conections.Count');
  SubBusIndexs[ConectionIndex] := Index;
end;

function TMultiBus.GetSubBusIndex(EID: integer): integer;
var
  Index, SubBusIndex: integer;
begin
  Index := Conections.IndexOf(EID);
  if Index < 0 then
    Exit(-1);
  SubBusIndex := SubBusIndexs[Index];
  if SubBusIndex = 0 then
    Exit(0)
  else if SubBusIndex = 1 then
    Exit(1)
  else
    raise Exception.Create('wrong SubBusIndex');
end;

procedure TMultiBus.AddConection(ConID: integer);
begin
  inherited AddConection(ConID);
  if SubBusIndexs.Count <> Conections.Count - 1 then
    raise Exception.Create('SubBusIndexs.Count<>Conections.Count-1');
  SubBusIndexs.Add(0);
end;

procedure TMultiBus.RemoveConection(ConID: integer);
var
  ConectionIndex: integer;
begin
  ConectionIndex := Conections.IndexOf(ConID);
  inherited RemoveConection(ConID);
  SubBusIndexs.Delete(ConectionIndex);
end;

function TMultiBus.GetMaxConectioNum(): integer;
begin
  Result := 50;
end;

procedure TMultiBus.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldBrushColour, BusLength, BayStart, k: integer;
  OldPenColor: TNeemaColour;
begin
  inherited Draw(c, Tick);
  OldBrushColour := c.getBrushColor;
  OldPenColor := c.getPenColor;
  BusLength := GetBusLength;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(Self.XPos - 3, YPos - 3, XPos + BusLength + 3, YPos + 26);
  end;
  c.setBrushColor(Bus1Color);
  ;
  c.FillRect(Self.XPos, YPos, XPos + BusLength, YPos + BusThiknes);
  c.setBrushColor(Bus2Color);
  c.FillRect(Self.XPos, YPos + SubBusDistance, XPos + BusLength,
    YPos + SubBusDistance + BusThiknes);
  c.setBrushColor(clNemWhite);
  c.TextOut(XPos + BusLength + 10, YPos, UtilUnit.VoltToReadableString(
    CalculatedVoltage[0]));
  c.TextOut(XPos + BusLength + 10, YPos + SubBusDistance,
    UtilUnit.VoltToReadableString(CalculatedVoltage[1]));
  c.setBrushColor(clNemBlack);
  DrawCB(c, XPos - CBHALFWIDTH, YPos + round(SubBusDistance / 2),
    BC12Closed, clNemBlack);
  c.setPenColor(clNemBlack);
  for k := 0 to Conections.Count - 1 do
  begin
    BayStart := YPos + SubBusDistance;
    if SubBusIndexs[k] = 0 then
    begin
      BayStart := YPos;
      C.setPenColor(Bus1Color);
      C.setBrushColor(Bus1Color);
    end
    else
    begin
      C.setPenColor(Bus2Color);
      C.setBrushColor(Bus2Color);
    end;
    c.Line(xpos + (k + 1) * BayDistansc, BayStart, xpos + (k + 1) *
      BayDistansc, YPos + SubBusDistance + BayHeigth);
    C.EllipseC(xpos + (k + 1) * BayDistansc, BayStart, 5, 5);
  end;
  c.setBrushColor(OldBrushColour);
  c.setPenColor(OldPenColor);
end;

function TMultiBus.GCVPerUnit(aID: integer): complex;
var
  SBI: integer;
begin
  SBI := GetSubBusIndex(aID);
  if SBI < 0 then
    raise Exception.Create('not conected to bus');
  Result := CalculatedVoltage[SBI] / StringToVolt(BaseVolt);
end;

function TMultiBus.GCVByIndex(aIndex: integer;out V: complex): boolean;
begin
  if aindex >= MaxSubBusNum then
    Exit(False);
  V := CalculatedVoltages[aIndex];
  Result := True;
end;

function TMultiBus.GIName: String;
begin
  Result:=Name;
end;

function TMultiBus.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
  if ((X > XPos) and (X < XPos + GetBusLength()) and (Y < YPos + 23) and (Y > YPos)) then
  begin
    IsSelected := True;
    Result := True;
  end;
end;

function TMultiBus.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    ConX := XPos + BayDistansc * (Conections.Count + 1);
    ConY := YPos + SubBusDistance + BayHeigth;
  end;
end;

function TMultiBus.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
var
  Index: integer;
begin
  Result := False;
  Index := Conections.IndexOf(cID);
  if Index > -1 then
  begin
    Result := True;
    ConX := XPos + BayDistansc * (Index + 1);
    ConY := YPos + SubBusDistance + BayHeigth;
  end;
end;

function TMultiBus.GetBusLength: integer;
var
  BusLength: integer;
begin
  BusLength := BayDistansc * Conections.Count;
  if Conections.Count < 1 then
    BusLength := MinBusLength;
  Result := BusLength;
end;

function TMultiBus.GetSubBusIndexByIndex(Index: integer): integer;
begin
  Result := SubBusIndexs[Index];
end;

end.
