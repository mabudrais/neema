unit LoadFlowSolverUnit;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, SolverUnit, GenircDataUnit, BusUnit, multibusunit,
  TransmissionLineUnit, TransFormer2WUnit, TransFormer3WShuntUnit, contnrs,
  UComplex, WritlnUnit, electricElentUnit, ConectionUnit, TransFormer3WUnit,
  SVCUnit, FFunit, ImpedanceUnit, StringSplitPatchUnit, UtilUnit, LabelUnit
  , ShuntUnit, PiImpedanceUnit, SEMeasurementUnit;

type
  TBreakerStatusArray = array of boolean;

  { TLoadFlowBus }

  TLoadFlowBus = class
    Name: string;
    DrawingItemID, SubMultiPlex: integer;
    BusType: TBusType;
    Volt, PQ: complex;
    IsFictional: boolean;
    constructor Create(cName: string; cDrawinItemID: integer;
      cBusType: TBusType; cV, cPQ: complex; cIsFictional: boolean);
    constructor Create(cName: string; cDrawinItemID: integer;
      cBusType: TBusType; cV, cPQ: complex; cSubMultiPlex: integer);
    procedure SetResultVolt(V: complex; cDrawing: TObject);
    function Compare(LFB: TLoadFlowBus): boolean;
  end;

  TLoadFlowSolverOption = (LFSOprintPowerError, LFSOprintTol,
    LFSOprintLinePower, LFSOprintVoltage);
  TLoadFlowSolverOptionSet = set of TLoadFlowSolverOption;
  TStartType = (STStringVolt, STCalCulatedVoltge, STFlatStart, STDC);
  { TLoadFlowSolver }

  TLoadFlowSolver = class(TSolver)
  private
    cDrawing: TObject;
    FOption: TLoadFlowSolverOptionSet;
    PrintVoltageEnabled: boolean;
    procedure AddShuntImp(LoadFlowBuss: TObjectList; Shunt: TShunt;
      const EL: TIntegerObjectMap);
    procedure AddTSVC(LoadFlowBuss, VariableElements: TObjectList;
      SVC: TSVC; const EL: TIntegerObjectMap);
    procedure ExtractTBusBuss(LoadFlowBuss, VariableElements: TObjectList; Bus: Tbus);
    procedure ExtractTLineBuss(LoadFlowBuss: TObjectList; Line: TLine);
    procedure ExtractBuses(const LoadFlowBuss, VariableElements: TObjectList;
      const EL: TIntegerObjectMap);
    procedure AddTLineImp(LoadFlowBuss: TObjectList; Line: TLine;
      const EL: TIntegerObjectMap);
    procedure AddPiImp(LoadFlowBuss: TObjectList; PiImp: TPiImpedance;
      const EL: TIntegerObjectMap);
    procedure AddToYBus(x, y: integer; imp: complex);
    procedure FillMatrixs(const LoadFlowBuss, VariableElements: TObjectList;
      const EL: TIntegerObjectMap);
    procedure AddTBussInfo(LoadFlowBuss: TObjectList);
    procedure AddTRansformer2w(LoadFlowBuss: TObjectList;
      Transformer: TTransFormer2W; const EL: TIntegerObjectMap);
    procedure ExtractTMultiBusBuss(LoadFlowBuss, VariableElements: TObjectList;
      MBus: TMultiBus);
    procedure GetSubBusData(k: integer; MBus: TMultiBus;
      out cPPq, cPV, cV: complex; out T: TBusType; out MaxRQ, MinRQ: double;
      out cName: string);
    procedure AddTRansformer3wShunt(LoadFlowBuss: TObjectList;
      Transformer: TTransFormer3WShunt; const EL: TIntegerObjectMap);
    procedure FindDeadElements(EL: TIntegerObjectMap);
    procedure FindElementsConectedToSlack(EL: TIntegerObjectMap);
    function CheckEquelBaseVA(EL: TIntegerObjectMap): boolean;
    procedure TopologyCheck(LoadFlowBuss: TObjectList; AdditionData: TObject);
    procedure UpdateElementDeadState(cE: TElicment; EL: TIntegerObjectMap;
      BSA: TBreakerStatusArray);
    procedure Extract3WTransformerBuss(LoadFlowBuss: TObjectList; T: TTransFormer3W);
    procedure AddTRansformer3w(LoadFlowBuss: TObjectList;
      Transformer: TTransFormer3W; const EL: TIntegerObjectMap);
    procedure AddBetweenIMpToYBus(var Bus2Index: integer; const Bus1Index: integer;
      const oz2: complex);
    procedure PrintMatrixs();
    procedure AddTImpedanceImp(LoadFlowBuss: TObjectList; Impedance: TImpedance;
      const EL: TIntegerObjectMap);
    procedure ParseOption(OPtion: string);
  protected
    Accelerationfactor: extended;
    VBus, PQ: array of complex;
    YBus: twodarray;
    BusType: array of Tbustype;
    STartType: TStartType;
    TreatPVasPQ, CheckTopology: boolean;
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList): boolean;
      virtual;
    procedure DeIni(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList);
      virtual;
    function Step: boolean; virtual;
    function GLFBI(DrawingItemID, DrawingBusID: integer; LoadFlowBuss: TObjectList;
      const EL: TIntegerObjectMap): integer;
    function GLFBI(DrawingItemID, LoadFlowBusIndex: integer;
      MultiBus: TMultiBus; LoadFlowBuss: TObjectList): integer;
    procedure GetLineBuss(out Bus2Index, Bus1Index: integer;
      const EL: TIntegerObjectMap; const Line: TLine; const LoadFlowBuss: TObjectList);
    function OtherSubBus(SubBusIndex: integer; M: TMultiBus): integer;
  public
    constructor Create;
    function Solve(Drawing: TObject; OptionString: string;
      AdditionData: TObject): boolean;
      override;
    function GetVBus(busIndex: integer): complex;
      virtual;//should be used by TVariableElement
    procedure SetVBus(busIndex: integer; Volt: complex);
      virtual;//should be used by TVariableElement
    function GetSBus(busIndex: integer): complex;
      virtual;//should be used by TVariableElement
    procedure SetSBus(busIndex: integer; S: complex);
      virtual;//should be used by TVariableElement
    procedure UpdateDrawing(Drawing: TObject; LoadFlowBuss: TObjectList); virtual;
    procedure UpdateVoltage; virtual;
    function PowerError: complex;
    procedure AddPIToYBus(const Bus1Index, Bus2Index: integer;
      const oz1, oz2, oz3: complex);
    property Option: TLoadFlowSolverOptionSet read FOption write FOption;
  end;

  { TVariableElement }

  TVariableElement = class
    YBusx, YBusy: integer;
    E: TElicment;
    function Correct(Solver: TLoadFlowSolver): boolean; virtual;
  end;

  { TVariableVarElement }

  TVariableVarElement = class(TVariableElement)
    TargetV, PreviousVar: extended;
    function Correct(Solver: TLoadFlowSolver): boolean; override;
    constructor Create(cYBusx, cYBusy: integer; cE: TElicment; cTargetV: extended);
    function getMaxQ(V: complex): extended; virtual;
    function getMinQ(V: complex): extended; virtual;




  public
    Epsilone: extended;
  end;

  { TPVBus }

  TPVBus = class(TVariableVarElement)
    MaxQ, MinQ: extended;
    constructor Create(cYBusx, cYBusy: integer; cE: TElicment;
      cMaxQ, cMinQ, cTargetV: extended);
    function getMaxQ(V: complex): extended; override;
    function getMinQ(V: complex): extended; override;
  end;

  { TSVC }

  { TSVCVARInject }

  TSVCVARInject = class(TVariableVarElement)
    MaxImp, MinImp: complex;
    constructor Create(cYBusx, cYBusy: integer; cE: TElicment;
      cMaxX, cMinX: complex; cTargetV, cEpsilon: extended);
    function getMaxQ(V: complex): extended; override;
    function getMinQ(V: complex): extended; override;
  end;

const
  STARTMODE = 'StartMode';
  FLATSTART = 'FlatStart';
  CALCULATEDVOLTGE = 'CalCulatedVoltge';
  PRINTVOLTAGE = 'PrintVoltage';
  OP_ACCELERATIONFACTOR = 'AccelerationFactor';
  OP_TREATPVASPQ = 'TreatPVasPQ';
  OP_TESTTOPOLGY = 'TestTopolgy';

implementation

uses DrawingUnit, elementUnit;

{ TSVC }

constructor TSVCVARInject.Create(cYBusx, cYBusy: integer; cE: TElicment;
  cMaxX, cMinX: complex; cTargetV, cEpsilon: extended);
begin
  inherited Create(cYBusx, cYBusy, cE, cTargetV);
  MaxImp := cMaxX;
  MinImp := cMinX;
  Epsilone := cEpsilon;
end;

function TSVCVARInject.getMaxQ(V: complex): extended;
begin
  Result := (V * cong(V * MaxImp)).im;
end;

function TSVCVARInject.getMinQ(V: complex): extended;
begin
  Result := (V * cong(V * MinImp)).im;
end;

{ TVariableVarElement }

function TVariableVarElement.Correct(Solver: TLoadFlowSolver): boolean;
var
  V, S, Vnew, OldS: complex;
  D, Vmod, RequirdCorrection, OtherElementVar, C: double;
begin
  Result := False;
  V := Solver.GetVBus(Self.YBusx);
  OldS := Solver.PQ[YBusx];
  OtherElementVar := OldS.im - Self.PreviousVar;
  Vmod := cmod(V);
  if abs((Vmod - TargetV) / TargetV) < Epsilone then
    Exit(True);
  D := TargetV * TargetV - v.im * v.im;
  if not (d > 0) then
    raise Exception.Create('');
  Vnew := sqrt(D) + i * v.im;
  Solver.SetVBus(YBusx, Vnew);
  S := Solver.GetSBus(Self.YBusx);
  RequirdCorrection := S.im - OtherElementVar;
  if RequirdCorrection > Self.getMaxQ(V) then
    RequirdCorrection := Self.getMaxQ(V);
  if RequirdCorrection < Self.getMinQ(V) then
    RequirdCorrection := Self.getMinQ(V);
  self.PreviousVar := RequirdCorrection * 1.0;
  S.im := Self.PreviousVar + OtherElementVar;
  Solver.SetSBus(YBusx, cinit(Solver.PQ[YBusx].re, s.im));
  //Solver.SetVBus(YBusx, V);
  // Solver.SetVBus(YBusx, Vnew);
end;

constructor TVariableVarElement.Create(cYBusx, cYBusy: integer;
  cE: TElicment; cTargetV: extended);
begin
  YBusx := cYBusx;
  cYBusy := cYBusy;
  E := cE;
  Epsilone := 0.005;
  TargetV := cTargetV;
  PreviousVar := 0;
end;

function TVariableVarElement.getMaxQ(V: complex): extended;
begin
  Result := 0;
  raise Exception.Create('getMaxQ  not implented');
end;

function TVariableVarElement.getMinQ(V: complex): extended;
begin
  Result := 0;
  raise Exception.Create('getMinQ  not implented');
end;

{ TPVBus }
constructor TPVBus.Create(cYBusx, cYBusy: integer; cE: TElicment;
  cMaxQ, cMinQ, cTargetV: extended);
begin
  inherited Create(cYBusx, cYBusy, cE, cTargetV);
  MaxQ := cMaxQ;
  MinQ := cMinQ;
end;

function TPVBus.getMaxQ(V: complex): extended;
begin
  Result := MaxQ;
end;

function TPVBus.getMinQ(V: complex): extended;
begin
  Result := MinQ;
end;

{ TVariableElement }

function TVariableElement.Correct(Solver: TLoadFlowSolver): boolean;
begin
  raise Exception.Create('abstract method');
  Result := False;
end;

procedure TLoadFlowSolver.AddTSVC(LoadFlowBuss, VariableElements: TObjectList;
  SVC: TSVC; const EL: TIntegerObjectMap);
var
  MaxY, MinY: complex;
  SVCBusIndex: integer;
  TargetVolt, SVCTargetVolt, SVCBaseVolt, TargetVPerUnit: extended;
  SVCBusType: TBusType;
begin
  if (not (SVC.CB1Close)) or (not (SVC.CB2Close)) then
  begin
    if (not (SVC.CB1Close)) and (not (SVC.CB2Close)) then
      Exit
    else
      raise Exception.Create('please close both svc breaker');
  end;
  SVC.GetAdmitancePerUnit(MaxY, MinY);
  if (abs(MaxY.re) > 1e-9) or ((abs(MinY.re) > 1e-9)) then
    raise Exception.Create('wrong svc r');
  SVCBusIndex := GLFBI(SVC.ID, SVC.GetConection(0), LoadFlowBuss, EL);
  if (not (StringToVolt(SVC.TargetVolt, SVCTargetVolt))) or
    (not (StringToVolt(SVC.BaseVolt, SVCBaseVolt))) then
    raise Exception.Create('svc wrong target volt');
  SVCBusType := (LoadFlowBuss[SVCBusIndex] as TLoadFlowBus).BusType;
  if (SVCBusType = regulatingbus) or (SVCBusType = slackbus) then
    raise Exception.Create('wrong SVC bus type');
  TargetVPerUnit := safeDiv(SVCTargetVolt, SVCBaseVolt);
  VariableElements.Add(TSVCVARInject.Create(SVCBusIndex, SVCBusIndex,
    SVC, MaxY, MinY, TargetVPerUnit, StrToFloat(SVC.VoltEpsilon)));
end;

{ TLoadFlowSolver }
procedure TLoadFlowSolver.ExtractTBusBuss(LoadFlowBuss, VariableElements: TObjectList;
  Bus: Tbus);
var
  cV: complex;
begin
  if Bus.Dead then
    Exit;
  if Bus.BusType = loadbus then
  begin
    case STartType of
      STStringVolt: cV := Bus.PerUnitVolt;
      STCalCulatedVoltge, STDC: cV := Bus.CalculatedVoltage / StringToVolt(Bus.BaseVolt);
      STFlatStart: cV := 1.0;
    end;
  end
  else if (bus.BusType = regulatingbus) and (STartType = STCalCulatedVoltge) then
    cV := Bus.CalculatedVoltage / StringToVolt(Bus.BaseVolt)
  else
    cv := Bus.PerUnitVolt;
  if (bus.BusType = regulatingbus) and (TreatPVasPQ) then
    VariableElements.Add(TPVBus.Create(LoadFlowBuss.Count, LoadFlowBuss.Count,
      Bus, Bus.MaxRegulationQ.ToDouble(), bus.MinRegulationQ.ToDouble(),
      StringToVolt(Bus.BusVoltageR)));
  LoadFlowBuss.add(TLoadFlowBus.Create(Bus.Name, bus.ID, bus.BusType,
    cv, bus.PerUnitPQ, False));
end;

procedure TLoadFlowSolver.GetSubBusData(k: integer; MBus: TMultiBus;
  out cPPq, cPV, cV: complex; out T: TBusType; out MaxRQ, MinRQ: double;
  out cName: string);
begin
  if not MBus.SubBusDead[k] then
  begin
    MBus.GetActiveBusDataPerUnit(k, T, cPV, cPPq, cName);
    if T = regulatingbus then
    begin
      MBus.GetRegulationQLimitPerUnit(k, MaxRQ, MinRQ);
      if (STartType = STCalCulatedVoltge) then
        cV := cmod(MBus.CalculatedVoltage[k]) / StringToVolt(MBus.BaseVolt)
      else      //need unit test
        cv := cPV;
    end
    else if T = loadbus then
    begin
      case STartType of
        STStringVolt: cV := cPV;
        STCalCulatedVoltge, STDC: cV :=
            MBus.CalculatedVoltage[k] / StringToVolt(MBus.BaseVolt);
        STFlatStart: cV := 1.0;
      end;
    end
    else
      cv := cPV;
  end;
end;

procedure TLoadFlowSolver.ExtractTMultiBusBuss(LoadFlowBuss, VariableElements:
  TObjectList;
  MBus: TMultiBus);
var
  SubBusNum, k: integer;
  cPPq1, cPV1, cV1, cPPq2, cPV2, cV2: complex;
  T1, T2: TBusType;
  cName1, cName2: string;
  MaxRQ1, MinRQ1, MaxRQ2, MinRQ2: double;
begin
  if MBus.Dead then
    Exit;
  if MaxSubBusNum > 2 then
    raise Exception.Create('algrothim dont suport this');
  SubBusNum := MBus.ActiveSubBusNum();
  if MBus.Name.Contains('ROS') then
    Wl('');
  GetSubBusData(0, MBus, cPPq1, cPV1, cV1, T1, MaxRQ1, MinRQ1, cName1);
  GetSubBusData(1, MBus, cPPq2, cPV2, cV2, T2, MaxRQ2, MinRQ2, cName2);
  if MBus.BC12Closed then
  begin
    //check for subbus dead
    if (T1 = slackbus) and (T2 = slackbus) then
      raise Exception.Create('two Conected slack bus')
    else if (T1 = regulatingbus) and (T2 = regulatingbus) then
      raise Exception.Create('two Conected regulation bus')
    else if ((T1 = slackbus) and (T2 = regulatingbus)) or
      ((T1 = regulatingbus) and (T2 = slackbus)) then
      raise Exception.Create('two Conected slack and reg bus')
    else if ((T1 = slackbus) and (T2 = loadbus)) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq2, 3))
    else if ((T1 = loadbus) and (T2 = slackbus)) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName2, MBus.ID, T2, cV2, cPPq1, 3))
    else if (T1 = regulatingbus) and (T2 = loadbus) then
    begin
      if (TreatPVasPQ) then   //multi thread problem
        VariableElements.Add(TPVBus.Create(LoadFlowBuss.Count,
          LoadFlowBuss.Count, MBus, MaxRQ1, MinRQ1, cPV1.re));
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq1 + cPPq2, 3));
    end
    else if (T1 = loadbus) and (T2 = regulatingbus) then
    begin
      if (TreatPVasPQ) then   //multi thread problem
        VariableElements.Add(TPVBus.Create(LoadFlowBuss.Count,
          LoadFlowBuss.Count, MBus, MaxRQ2, MinRQ2, cPV2.re));
      LoadFlowBuss.add(TLoadFlowBus.Create(cName2, MBus.ID, T2, cV2, cPPq1 + cPPq2, 3));
    end
    else if ((T1 = loadbus) and (T2 = loadbus)) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq1 + cPPq2, 3));
  end
  else
  begin
    if (T1 = slackbus) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq1, 1))
    else if (T1 = regulatingbus) and (not MBus.SubBusDead[0]) then
    begin
      if (TreatPVasPQ) then   //multi thread problem
        VariableElements.Add(TPVBus.Create(LoadFlowBuss.Count,
          LoadFlowBuss.Count, MBus, MaxRQ1, MinRQ1, cPV1.re));
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq1, 1));
    end
    else if (T1 = loadbus) and (not MBus.SubBusDead[0]) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T1, cV1, cPPq1, 1));
    if (T2 = slackbus) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName1, MBus.ID, T2, cV2, cPPq2, 2))
    else if (T2 = regulatingbus) and (not MBus.SubBusDead[1]) then
    begin
      if (TreatPVasPQ) then   //multi thread problem
        VariableElements.Add(TPVBus.Create(LoadFlowBuss.Count,
          LoadFlowBuss.Count, MBus, MaxRQ2, MinRQ2, cPV2.re));
      LoadFlowBuss.add(TLoadFlowBus.Create(cName2, MBus.ID, T2, cV2, cPPq2, 2));
    end
    else if (T2 = loadbus) and (not MBus.SubBusDead[1]) then
      LoadFlowBuss.add(TLoadFlowBus.Create(cName2, MBus.ID, T2, cV2, cPPq2, 2));
  end;
end;

procedure TLoadFlowSolver.ExtractTLineBuss(LoadFlowBuss: TObjectList; Line: TLine);
begin
  if Line.Dead then
    Exit;
  if (not Line.CB1Close) and (not Line.CB2Close) and (not Line.Dead) then
    raise Exception.Create('');
  if ((Line.CB1Close) and (not Line.CB2Close)) then
    LoadFlowBuss.add(TLoadFlowBus.Create(Line.Name + 'CB2', Line.ID,
      loadbus, 1, 0, True));
  if ((not Line.CB1Close) and (Line.CB2Close)) then
    LoadFlowBuss.add(TLoadFlowBus.Create(Line.Name + 'CB1', Line.ID,
      loadbus, 1, 0, True));
end;

procedure TLoadFlowSolver.Extract3WTransformerBuss(LoadFlowBuss: TObjectList;
  T: TTransFormer3W);
begin
  if T.Dead then
    Exit;
  if (not T.CB1Close) and (not T.CB2Close) and (not T.CB3Close) and (not T.Dead) then
    raise Exception.Create('3w Trans should be dead');
  if ((T.CB1Close) and (T.CB2Close) and (T.CB3Close)) then
    LoadFlowBuss.add(TLoadFlowBus.Create(T.Name + 'CenterPoint', T.ID,
      loadbus, 1, 0, True));
end;

procedure TLoadFlowSolver.AddToYBus(x, y: integer; imp: complex);
begin
  // if (x=0) and (y=0)then
  // WriteLn(x,y,' ',(1/imp).re,'+',(1/imp).im,'j');
  YBus[x, y] := YBus[x, y] + 1 / imp;
end;

procedure TLoadFlowSolver.AddTBussInfo(LoadFlowBuss: TObjectList);
var
  k: integer;
  LFB: TLoadFlowBus;
begin
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFB := LoadFlowBuss[k] as TLoadFlowBus;
    VBus[k] := LFB.Volt;
    if (STartType = STFlatStart) and (LFB.BusType = loadbus) then
      //you shooud remove and (LFB.BusType = loadbus)
      VBus[k] := 1.0;
    BusType[k] := LFB.BusType;
    PQ[k] := LFB.PQ;
  end;
end;

procedure TLoadFlowSolver.AddTLineImp(LoadFlowBuss: TObjectList;
  Line: TLine; const EL: TIntegerObjectMap);
var
  oz1, oz2, oz3: complex;
  Bus1Index, Bus2Index: integer;
begin
  if Line.Dead then
    Exit;
  if (not Line.CB1Close) and (not Line.CB2Close) then
    raise Exception.Create('internal error two side line open ');
  line.GetImpPerUnit(oz1, oz2, oz3);
  GetLineBuss(Bus2Index, Bus1Index, EL, Line, LoadFlowBuss);
  AddToYBus(Bus1Index, Bus1Index, oz1);
  AddBetweenIMpToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz3);
end;

procedure TLoadFlowSolver.AddPiImp(LoadFlowBuss: TObjectList;
  PiImp: TPiImpedance; const EL: TIntegerObjectMap);
var
  oz1, oz2, oz3: complex;
  Bus1Index, Bus2Index: integer;
begin
  if PiImp.Dead then
    Exit;
  Bus1Index := GLFBI(PiImp.ID, PiImp.GetConection(0), LoadFlowBuss, EL);
  Bus2Index := GLFBI(PiImp.ID, PiImp.GetConection(1), LoadFlowBuss, EL);
  oz1 := cinit(StrToFloat(PiImp.ImpedanceR1), StrToFloat(PiImp.ImpedanceX1));
  oz2 := cinit(StrToFloat(PiImp.ImpedanceR2), StrToFloat(PiImp.ImpedanceX2));
  oz3 := cinit(StrToFloat(PiImp.ImpedanceR3), StrToFloat(PiImp.ImpedanceX3));
  AddToYBus(Bus1Index, Bus1Index, oz1);
  AddBetweenIMpToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz3);
end;

procedure TLoadFlowSolver.AddShuntImp(LoadFlowBuss: TObjectList;
  Shunt: TShunt; const EL: TIntegerObjectMap);
var
  oz: complex;
  BusIndex: integer;
begin
  if Shunt.Dead then
    Exit;
  if not (Shunt.CBClose) then
    raise Exception.Create('internal error shunt is open but is not dead');
  oz := Shunt.GetXPerUnit();
  BusIndex := GLFBI(Shunt.ID, Shunt.GetConection(0), LoadFlowBuss, EL);
  AddToYBus(BusIndex, BusIndex, oz);
end;

procedure TLoadFlowSolver.AddTImpedanceImp(LoadFlowBuss: TObjectList;
  Impedance: TImpedance; const EL: TIntegerObjectMap);
var
  Bus1Index, Bus2Index: integer;
begin
  if Impedance.Dead then
    Exit;
  Bus1Index := GLFBI(Impedance.ID, Impedance.GetConection(0), LoadFlowBuss, EL);
  Bus2Index := GLFBI(Impedance.ID, Impedance.GetConection(1), LoadFlowBuss, EL);
  AddBetweenIMpToYBus(Bus2Index, Bus1Index, Impedance.Impedance);
end;

procedure TLoadFlowSolver.AddTRansformer2w(LoadFlowBuss: TObjectList;
  Transformer: TTransFormer2W; const EL: TIntegerObjectMap);
var
  oz1, oz2, oz3: complex;
  Bus1Index, Bus2Index: integer;
begin
  if Transformer.Dead then
    Exit;
  if (not Transformer.CB1Close) and (not Transformer.CB1Close) then
    raise Exception.Create('');
  //you have to chech for option: ignor transformer if opened by one side
  if (not Transformer.CB1Close) or (not Transformer.CB1Close) then
    Exit;
  Transformer.GetImpPerUnit(oz1, oz2, oz3);
  if Transformer.CB1Close then
    Bus1Index := GLFBI(Transformer.ID, Transformer.GetConection(0), LoadFlowBuss, EL)
  else
    Bus1Index := GLFBI(Transformer.ID, Transformer.ID, LoadFlowBuss, EL);
  if Transformer.CB2Close then
    Bus2Index := GLFBI(Transformer.ID, Transformer.GetConection(1), LoadFlowBuss, EL)
  else
    Bus2Index := GLFBI(Transformer.ID, Transformer.ID, LoadFlowBuss, EL);
  AddToYBus(Bus1Index, Bus1Index, oz1);
  AddToYBus(Bus1Index, Bus1Index, oz2);
  AddToYBus(Bus1Index, Bus2Index, oz2);

  AddToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz3);
  AddToYBus(Bus2Index, Bus2Index, oz2);
end;

procedure TLoadFlowSolver.AddTRansformer3wShunt(LoadFlowBuss: TObjectList;
  Transformer: TTransFormer3WShunt; const EL: TIntegerObjectMap);
var
  oz1, oz2, oz3: complex;
  Bus1Index, Bus2Index: integer;
begin
  if Transformer.Dead then
    Exit;
  if (not Transformer.CB1Close) and (not Transformer.CB1Close) then
    raise Exception.Create('');
  //you have to chech for option: ignor transformer if opened by one side
  if (not Transformer.CB1Close) or (not Transformer.CB2Close) then
    Exit;
  Transformer.GetPiModelImpPerUnit(oz1, oz2, oz3);
  if Transformer.CB1Close then
    Bus1Index := GLFBI(Transformer.ID, Transformer.GetConection(0), LoadFlowBuss, EL)
  else
    Bus1Index := GLFBI(Transformer.ID, Transformer.ID, LoadFlowBuss, EL);
  if Transformer.CB2Close then
    Bus2Index := GLFBI(Transformer.ID, Transformer.GetConection(1), LoadFlowBuss, EL)
  else
    Bus2Index := GLFBI(Transformer.ID, Transformer.ID, LoadFlowBuss, EL);
  AddToYBus(Bus1Index, Bus1Index, oz1);
  AddToYBus(Bus1Index, Bus1Index, oz2);
  AddToYBus(Bus1Index, Bus2Index, oz2);

  AddToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz3);
  AddToYBus(Bus2Index, Bus2Index, oz2);
end;

procedure TLoadFlowSolver.AddTRansformer3w(LoadFlowBuss: TObjectList;
  Transformer: TTransFormer3W; const EL: TIntegerObjectMap);
var
  oz1, oz2, oz3, SeconderyImp, TerImp: complex;
  Bus1Index, Bus2Index, Bus3Index, CenterBusIndex: integer;
begin
  if Transformer.Dead then
    Exit;
  if (not Transformer.CB1Close) and (Transformer.CB2Close) and
    (Transformer.CB3Close) then
    raise Exception.Create('transformer ' + Transformer.Name + ' CB1 is open');
  //you have to chech for option: ignor transformer if opened by one side
  if Transformer.isTwoSideOpen() then
    Exit;
  if Transformer.ConectionCount < 3 then
    raise Exception.Create('3 winding transformer must be conected to 3 bus');
  if Transformer.CB1Close then
    Bus1Index := GLFBI(Transformer.ID, Transformer.GetConection(0), LoadFlowBuss, EL);
  if Transformer.CB2Close then
    Bus2Index := GLFBI(Transformer.ID, Transformer.GetConection(1), LoadFlowBuss, EL);
  if Transformer.CB3Close then
    Bus3Index := GLFBI(Transformer.ID, Transformer.GetConection(2), LoadFlowBuss, EL);
  if (Transformer.CB1Close) and (Transformer.CB2Close) and (Transformer.CB3Close) then
  begin
    CenterBusIndex := GLFBI(Transformer.ID, Transformer.ID, LoadFlowBuss, EL);
    Transformer.GetPrimeryWindingPiModelImpPerUnit(oz1, oz2, oz3);
    AddPIToYBus(Bus1Index, CenterBusIndex, oz1, oz2, oz3);
    //StrToFloat() may cause exception
    SeconderyImp := cinit(StrToFloat(Transformer.PerUnitSecondaryR),
      StrToFloat(Transformer.PerUnitSecondaryX));
    TerImp := cinit(StrToFloat(Transformer.PerUnitTertiaryR),
      StrToFloat(Transformer.PerUnitTertiaryX));
    AddBetweenIMpToYBus(CenterBusIndex, Bus2Index, SeconderyImp);
    AddBetweenIMpToYBus(CenterBusIndex, Bus3Index, TerImp);
  end
  else
  begin
    Transformer.GetPiModelImpPerUnit(oz1, oz2, oz3);
    if Transformer.CB2Close then
      AddPIToYBus(Bus1Index, Bus2Index, oz1, oz2, oz3)
    else
      AddPIToYBus(Bus1Index, Bus3Index, oz1, oz2, oz3);
  end;
end;

function TLoadFlowSolver.GLFBI(DrawingItemID, LoadFlowBusIndex: integer;
  MultiBus: TMultiBus; LoadFlowBuss: TObjectList): integer;
var
  SBI: integer;
  LFB: TLoadFlowBus;
begin
  Result := -1;
  SBI := MultiBus.GetSubBusIndex(DrawingItemID);
  LFB := (LoadFlowBuss[LoadFlowBusIndex] as TLoadFlowBus);
  if (SBI = 0) and ((LFB.SubMultiPlex and 1) = 1) then
    Exit(LoadFlowBusIndex)
  else if (SBI = 1) and ((LFB.SubMultiPlex and 2) = 2) then
    Exit(LoadFlowBusIndex);

end;
//GetLoadFlowBusIndex
function TLoadFlowSolver.GLFBI(DrawingItemID, DrawingBusID: integer;
  LoadFlowBuss: TObjectList; const EL: TIntegerObjectMap): integer;
var
  k: integer;
  LFB: TLoadFlowBus;
begin
  Result := -1;
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFB := (LoadFlowBuss[k] as TLoadFlowBus);
    if LFB.DrawingItemID = DrawingBusID then
    begin
      if LFB.SubMultiPlex = 0 then
        Exit(k)
      else
      if GLFBI(DrawingItemID, k, EL[DrawingBusID] as TMultiBus, LoadFlowBuss) > -1 then
        Exit(k);
    end;
  end;
  raise Exception.Create('LoadFlowBus not found ,ID ' + DrawingItemID.ToString());
end;

constructor TLoadFlowBus.Create(cName: string; cDrawinItemID: integer;
  cBusType: TBusType; cV, cPQ: complex; cIsFictional: boolean);
begin
  Name := cName;
  SubMultiPlex := 0;
  DrawingItemID := cDrawinItemID;
  BusType := cBusType;
  IsFictional := cIsFictional;
  Volt := cV;
  PQ := cPQ;
end;

procedure TLoadFlowBus.SetResultVolt(V: complex; cDrawing: TObject);
var
  EL: TIntegerObjectMap;
  E: TElement;
  MBus: TMultiBus;
begin
  EL := (cDrawing as TDrawing).GetElementList;
  E := El[DrawingItemID] as TElement;
  if E is TBus then
    (E as TBus).CalculatedVoltage := V * StringToVolt((E as tbus).BaseVolt)
  else if E is TMultiBus then
  begin
    MBus := (E as TMultiBus);
    if (Self.SubMultiPlex and 1) = 1 then
      MBus.CalculatedVoltage[0] := V * StringToVolt((E as TMultiBus).BaseVolt);
    if (Self.SubMultiPlex and 2) = 2 then
      MBus.CalculatedVoltage[1] := V * StringToVolt((E as TMultiBus).BaseVolt);
  end
  else if E is TLine then
  begin
    if (E as TLine).CB1Close then
      (E as TLine).CalculatedV2 := V * StringToVolt((E as TLine).BaseVolt)
    else
      (E as TLine).CalculatedV1 := V * StringToVolt((E as TLine).BaseVolt);
  end
  else if E is TTransFormer3W then
  else
    raise Exception.Create('LoadFlowBus conected to wrong element type');
end;

function TLoadFlowBus.Compare(LFB: TLoadFlowBus): boolean;
begin
  if Name <> LFB.Name then
    Exit(False);
  if DrawingItemID <> LFB.DrawingItemID then
    Exit(False);
  if SubMultiPlex <> LFB.SubMultiPlex then
    Exit(False);
  if BusType <> LFB.BusType then
    Exit(False);
  if Volt <> LFB.Volt then
    Exit(False);
  if pq <> LFB.PQ then
    Exit(False);
  if IsFictional <> LFB.IsFictional then
    Exit(False);
end;

constructor TLoadFlowBus.Create(cName: string; cDrawinItemID: integer;
  cBusType: TBusType; cV, cPQ: complex; cSubMultiPlex: integer);
begin
  Create(cName, cDrawinItemID, cBusType, cV, cPQ, False);
  SubMultiPlex := cSubMultiPlex;
end;

function TLoadFlowSolver.OtherSubBus(SubBusIndex: integer; M: TMultiBus): integer;
begin
  if M.ActiveSubBusNum() > 2 then
    raise Exception.Create('');
  if SubBusIndex = 0 then
    Result := 1
  else if SubBusIndex = 1 then
    Result := 0
  else
    raise Exception.Create('');
end;

procedure TLoadFlowSolver.UpdateElementDeadState(cE: TElicment;
  EL: TIntegerObjectMap; BSA: TBreakerStatusArray);
var
  k, SubBusIndex: integer;
  ElicE: TElicment;
  cMulti: TMultiBus;
begin
  if cE.ConectionCount <> Length(BSA) then
    raise Exception.Create('wrong conection');
  for k := 0 to Length(BSA) - 1 do
  begin
    if BSA[k] = True then
    begin
      ElicE := EL[cE.GetConection(k)] as TElicment;
      if ElicE is TBus then
      begin
        if not ElicE.Dead then
          cE.Dead := False;
        if not cE.Dead then
          ElicE.Dead := False;
      end
      else if ElicE is TMultiBus then
      begin
        cMulti := ElicE as TMultiBus;
        if not cMulti.SubBusDead[cMulti.GetSubBusIndex(ce.ID)] then
          cE.Dead := False;
        if not cE.Dead then
        begin
          SubBusIndex := cMulti.GetSubBusIndex(ce.ID);
          cMulti.SubBusDead[SubBusIndex] := False;
          if cMulti.BC12Closed then
            cMulti.SubBusDead[OtherSubBus(SubBusIndex, cMulti)] := False;
        end;
      end
      else
        raise Exception.Create(ce.ClassName +
          ' is connected to element which is not bus or multibus');
    end;
  end;
end;

procedure TLoadFlowSolver.FindElementsConectedToSlack(EL: TIntegerObjectMap);
begin
end;

function TLoadFlowSolver.CheckEquelBaseVA(EL: TIntegerObjectMap): boolean;
var
  E: TObject;
  FirstElement: boolean;
  Elic: TElicment;
  VA: extended;
begin
  Result := False;
  FirstElement := True;
  for E in El do
  begin
    if (E is TElicment) and (not (E is TConection)) then
    begin
      Elic := (E as TElicment);
      if FirstElement then
      begin
        FirstElement := False;
        VA := StringToVA(Elic.BaseVA);
      end
      else
      begin
        if abs(Va - StringToVA(Elic.BaseVA)) > 1e-6 then
        begin
          Elic.IsSelected := True;
          raise Exception.Create(Elic.ClassName+' '+Elic.Name + ' has deffrent mva' + Elic.BaseVA);
        end;
      end;
    end;
  end;
  Result := True;
end;

procedure TLoadFlowSolver.FindDeadElements(EL: TIntegerObjectMap);
var
  E: TObject;
  cMultiBus, Cmult: TMultiBus;
  k: integer;
begin
  for E in El do
  begin
    if E is TBus then
      (E as TBus).Dead := not ((E as TBus).BusType = slackbus)
    else if E is TMultiBus then
    begin
      cMultiBus := E as TMultiBus;
      cMultiBus.SubBusDead[0] := not (cMultiBus.Bus1Type = slackbus);
      cMultiBus.SubBusDead[1] := not (cMultiBus.Bus2Type = slackbus);
      if (cMultiBus.BC12Closed) and ((not cMultiBus.SubBusDead[0])) then
        cMultiBus.SubBusDead[1] := False;
      if (cMultiBus.BC12Closed) and ((not cMultiBus.SubBusDead[1])) then
        cMultiBus.SubBusDead[0] := False;
    end
    else if E is TElicment then
      (E as TElicment).Dead := True;
  end;
  for k := 0 to EL.Count do
  begin
    for E in El do
    begin
      if (E as TElement).Name.Contains('MAR') then
      begin
        if (E is TMultiBus) then
        begin
          Cmult := (E as TMultiBus);
          // Cmult.Dead;
        end;
      end;
      if E is TImpedance then
        UpdateElementDeadState(E as TImpedance, EL,
          TBreakerStatusArray.Create(True, True))
      else if E is TLine then
        UpdateElementDeadState(E as TLine, EL, TBreakerStatusArray.Create(
          (E as TLine).CB1Close, (E as TLine).CB2Close))
      else if E is TTransFormer2W then
        UpdateElementDeadState(E as TTransFormer2W, EL, TBreakerStatusArray.Create(
          (E as TTransFormer2W).CB1Close, (E as TTransFormer2W).CB2Close))
      else if E is TTransFormer3WShunt then
        UpdateElementDeadState(E as TTransFormer3WShunt, EL,
          TBreakerStatusArray.Create((E as TTransFormer3WShunt).CB1Close,
          (E as TTransFormer3WShunt).CB2Close))
      else if E is TTransFormer3W then
        UpdateElementDeadState(E as TTransFormer3W, EL, TBreakerStatusArray.Create(
          (E as TTransFormer3W).CB1Close, (E as TTransFormer3W).CB2Close,
          (E as TTransFormer3W).CB3Close))
      else if E is TShunt then
        UpdateElementDeadState(E as TShunt, EL,
          TBreakerStatusArray.Create((E as TShunt).CBClose))
      else if E is TPiImpedance then
        UpdateElementDeadState(E as TPiImpedance, EL, TBreakerStatusArray.Create(
          True, True));
    end;
  end;
end;

procedure TLoadFlowSolver.TopologyCheck(LoadFlowBuss: TObjectList;
  AdditionData: TObject);
var
  PLoadFlowsBus: TObjectList;
  k: integer;
begin
  PLoadFlowsBus := AdditionData as TObjectList;
  for k := 0 to PLoadFlowsBus.Count - 1 do
  begin
    if not (PLoadFlowsBus[k] as TLoadFlowBus).Compare(LoadFlowBuss[k] as
      TLoadFlowBus) then
      raise Exception.Create('topology error');
  end;
end;

function TLoadFlowSolver.Ini(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList): boolean;
var
  D: TDrawing;
  EL: TIntegerObjectMap;
begin
  cDrawing := Drawing;
  D := cDrawing as TDrawing;
  EL := D.GetElementList;
  if not CheckEquelBaseVA(D.GetElementList) then
    Exit(False);
  FindDeadElements(EL);
  ExtractBuses(LoadFlowBuss, VariableElements, EL);
  //you have to check if any busbar is conected to busbar
  //if any  then riase Exception
  //also if any elements is has more than one conection to the same busbar
  //check also for svc conected to pv bus
  //check also if more than one svc is conected to the same bus
  SetLength(bustype, LoadFlowBuss.Count);
  SetLength(VBus, LoadFlowBuss.Count);
  SetLength(YBus, LoadFlowBuss.Count, LoadFlowBuss.Count);
  SetLength(PQ, LoadFlowBuss.Count);
  AddTBussInfo(LoadFlowBuss);
  FillMatrixs(LoadFlowBuss, VariableElements, EL);
  Result := True;
end;

procedure TLoadFlowSolver.DeIni(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList);
begin
  SetLength(bustype, 0);
  SetLength(VBus, 0);
  SetLength(YBus, 0);
  SetLength(PQ, 0);
end;

procedure TLoadFlowSolver.ParseOption(OPtion: string);
var
  OPtionList, COption: TStringArray;
  k: integer;
begin
  OptionList := StringSplitPatch(OPtion, [';'], TStringSplitOptions.ExcludeEmpty);
  for k := 0 to Length(OptionList) - 1 do
  begin
    if OptionList[k].StartsWith(STARTMODE) then
    begin
      COption := OptionList[k].Split('=', TStringSplitOptions.ExcludeEmpty);
      if (Length(COption) <> 2) or (COption[0] <> STARTMODE) then
        raise Exception.Create('wrong optyion data ' + OPtionList[k]);
      if COption[1] = FLATSTART then
        STartType := STFlatStart
      else if COption[1] = CALCULATEDVOLTGE then
        STartType := STCalCulatedVoltge
      else
        raise Exception.Create('unknown Start mode');
    end
    else if OPtionList[k].StartsWith(PRINTVOLTAGE) then
      PrintVoltageEnabled := True
    else if OPtionList[k].StartsWith(OP_TESTTOPOLGY) then
      CheckTopology := True
    else if OPtionList[k].StartsWith(OP_TREATPVASPQ) then
      TreatPVasPQ := True
    else if OPtionList[k].StartsWith(OP_ACCELERATIONFACTOR) then
    begin
      COption := OptionList[k].Split('=', TStringSplitOptions.ExcludeEmpty);
      if (Length(COption) <> 2) or (COption[0] <> OP_ACCELERATIONFACTOR) then
        raise Exception.Create('wrong optyion data ' + OPtionList[k]);
      if not TryStrToFloat(COption[1], Accelerationfactor) then
        raise Exception.Create('wrong ACCELERATIONFACTOR ');
      SetLength(COption, 0);
    end
    else
      raise  Exception.Create('unknown option');
  end;
end;

function TLoadFlowSolver.Solve(Drawing: TObject; OptionString: string;
  AdditionData: TObject): boolean;
var
  k, m: integer;
  LoadFlowBuss, VariableElements: TObjectList;
  NeedMorCorrection: boolean;
begin
  Result := False;
  LoadFlowBuss := TObjectList.Create(True);
  VariableElements := TObjectList.Create(True);
  STartType := STStringVolt;
  CheckTopology := False;
  Accelerationfactor := 1.0;
  TreatPVasPQ := False;
  Option := [];
  ParseOption(OptionString);
  if not Ini(Drawing, LoadFlowBuss, VariableElements) then
    Exit(False);
  if CheckTopology then
    TopologyCheck(LoadFlowBuss, AdditionData);
  //PrintMatrixs();
  k := 0;
  NeedMorCorrection := False;
  if VariableElements.Count > 0 then
    NeedMorCorrection := True;
  while ((not Step()) or NeedMorCorrection) and (k < 100 / Accelerationfactor) do
  begin
    if (k > 0) and ((k mod 2) = 0) then
    begin
      NeedMorCorrection := False;
      for m := 0 to VariableElements.Count - 1 do
      begin
        if not ((VariableElements[m] as TVariableElement).Correct(Self)) then
          NeedMorCorrection := True;
      end;
    end;
    Inc(k);
    if PrintVoltageEnabled then
    begin
      for m := 0 to LoadFlowBuss.Count - 1 do
        WL([(LoadFlowBuss[m] as TLoadFlowBus).Name, ': ', FFunit.FA(GetVBus(m), 3)]);
    end;
  end;
  if k > 100 then
    raise Exception.Create('faile to solve');//memory leak
  UpdateVoltage;
  UpdateDrawing(Drawing, LoadFlowBuss);
  DeIni(Drawing, LoadFlowBuss, VariableElements);
  FreeAndNil(LoadFlowBuss);
  FreeAndNil(VariableElements);
  Result := True;
end;

procedure TLoadFlowSolver.ExtractBuses(const LoadFlowBuss, VariableElements: TObjectList;
  const EL: TIntegerObjectMap);
var
  cE: TElement;
  E: TObject;
begin
  for E in El do
  begin
    cE := E as TElement;
    if cE is TBus then
      ExtractTBusBuss(LoadFlowBuss, VariableElements, ce as TBus)
    else if cE is TImpedance then
    else if cE is TLine then
      ExtractTLineBuss(LoadFlowBuss, ce as TLine)
    else if cE is TMultiBus then
      ExtractTMultiBusBuss(LoadFlowBuss, VariableElements, Ce as TMultiBus)
    else if cE is TTransFormer2W then
    else if cE is TTransFormer3WShunt then
    else if cE is TTransFormer3W then
      Extract3WTransformerBuss(LoadFlowBuss, cE as TTransFormer3W)
    else if ce is TSVC then
    else if ce is TConection then
    else if ce is TNeemaLabel then
    else if ce is TShunt then
    else if ce is TPiImpedance then
    else
      raise Exception.Create('uknown element');
  end;
end;

procedure TLoadFlowSolver.FillMatrixs(const LoadFlowBuss, VariableElements: TObjectList;
  const EL: TIntegerObjectMap);
var
  E: TObject;
begin
  for E in El do
  begin
    if E is TBus then
    else if E is TMultiBus then
    else if E is TImpedance then
      AddTImpedanceImp(LoadFlowBuss, E as TImpedance, EL)
    else if E is TLine then
      AddTLineImp(LoadFlowBuss, E as TLine, EL)
    else if E is TTransFormer2W then
      AddTRansformer2w(LoadFlowBuss, E as TTransFormer2W, EL)
    else if E is TTransFormer3WShunt then
      AddTRansformer3wShunt(LoadFlowBuss, E as TTransFormer3WShunt, EL)
    else if E is TTransFormer3W then
      AddTRansformer3w(LoadFlowBuss, E as TTransFormer3W, EL)
    else if E is TSVC then
      AddTSVC(LoadFlowBuss, VariableElements, E as TSVC, EL)
    else if E is TConection then
    else if E is TNeemaLabel then
    else if E is TShunt then
      AddShuntImp(LoadFlowBuss, E as TShunt, EL)
    else if E is TPiImpedance then
      AddPiImp(LoadFlowBuss, E as TPiImpedance, EL)
    else
      raise Exception.Create('unsuported load flow element');
  end;
end;

function TLoadFlowSolver.Step: boolean;
begin
  Result := True;
  raise Exception.Create('not implented');
end;

constructor TLoadFlowSolver.Create;
begin
  inherited;
end;

function TLoadFlowSolver.PowerError: complex;
var
  Isum, PBus, S12: complex;
  TotalPowerErrorR, TotalPowerErrorQ, TotalPower: double;
  j, k, mult, NumOfBus: integer;
begin
  TotalPowerErrorR := 0;
  TotalPowerErrorQ := 0;
  TotalPower := 0.0;
  NumOfBus := Length(VBus);
  for j := 0 to NumOfBus - 1 do
  begin
    Isum := 0;
    if (bustype[j] <> slackbus) then
    begin
      if bustype[j] <> regulatingbus then
        TotalPower := TotalPower + PQ[j].re;
      for k := 0 to NumOfBus - 1 do
      begin
        if k = j then
          mult := 1
        else
          mult := -1;
        if abs(cmod(YBus[j, k])) < 1e-8 then
          Continue;
        S12 := VBus[j] * cong((VBus[j] - VBus[k]) * YBus[j, k]);
        Isum := Isum + vbus[k] * ybus[j, k] * mult;
      end;
      PBus := Isum * cong(vbus[j]);
      TotalPowerErrorR := TotalPowerErrorR + abs(PBus.re - PQ[j].re);
      TotalPowerErrorQ := TotalPowerErrorQ + abs(PBus.im + PQ[j].im);
    end;
  end;
  Result.re := TotalPowerErrorR;
  Result.im := TotalPowerErrorQ;
  WL('total MW ' + TotalPower.ToString());
  wL('total mw error ' + TotalPowerErrorR.ToString());
  wL('total var error ' + TotalPowerErrorQ.ToString());
end;

procedure TLoadFlowSolver.UpdateDrawing(Drawing: TObject; LoadFlowBuss: TObjectList);
var
  k, B1, B2: integer;
  E: TObject;
  EL: TIntegerObjectMap;
  cMultiBus: TMultiBus;
  Line: TLine;
begin
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    (LoadFlowBuss[k] as TLoadFlowBus).SetResultVolt(VBus[k], Drawing);
    //Wl([k.ToString, ' ', (LoadFlowBuss[k] as TLoadFlowBus).Name]);
  end;
  EL := (Drawing as TDrawing).GetElementList;
  for E in EL do
  begin
    if (E is TBus) and ((E as TBus).Dead) then
      (E as TBus).CalculatedVoltage := 0
    else if E is TMultiBus then
    begin
      //untestested code
      cMultiBus := E as TMultiBus;
      for k := 0 to MaxSubBusNum - 1 do
      begin
        if cMultiBus.SubBusDead[k] then
          cMultiBus.CalculatedVoltage[k] := 0;
      end;
    end
    else if (E is TLine) then
    begin
      Line := E as TLine;
      if (Line.Dead) then
      begin
        Line.CalculatedV1 := 0;
        Line.CalculatedV2 := 0;
      end
      else
      begin
        GetLineBuss(B2, B1, EL, Line, LoadFlowBuss);
        Line.CalculatedV1 := VBus[B1] * StringToVolt(Line.BaseVolt);
        Line.CalculatedV2 := VBus[B2] * StringToVolt(Line.BaseVolt);
        Line.UpdateCalculatedS();
      end;
    end;
  end;
end;

procedure TLoadFlowSolver.AddPIToYBus(const Bus1Index, Bus2Index: integer;
  const oz1, oz2, oz3: complex);
begin
  AddToYBus(Bus1Index, Bus1Index, oz1);
  AddToYBus(Bus1Index, Bus1Index, oz2);
  AddToYBus(Bus1Index, Bus2Index, oz2);

  AddToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz3);
end;

procedure TLoadFlowSolver.AddBetweenIMpToYBus(var Bus2Index: integer;
  const Bus1Index: integer; const oz2: complex);
begin
  AddToYBus(Bus1Index, Bus1Index, oz2);
  AddToYBus(Bus1Index, Bus2Index, oz2);

  AddToYBus(Bus2Index, Bus1Index, oz2);
  AddToYBus(Bus2Index, Bus2Index, oz2);
end;

procedure TLoadFlowSolver.PrintMatrixs();
var
  W1, H1, k, m: integer;
begin
  W1 := Length(YBus);
  H1 := Length(YBus[0]);
  for k := 0 to W1 - 1 do
  begin
    for m := 0 to H1 - 1 do
    begin
      if abs(cmod(YBus[k, m])) > 1e-6 then
        WL('(' + k.ToString() + ',' + m.ToString() + ')' + F(YBus[k, m].re, 3) +
          ',' + F(YBus[k, m].im, 3));
    end;
  end;
end;

function TLoadFlowSolver.GetVBus(busIndex: integer): complex;
begin
  Result := VBus[busIndex];
end;

procedure TLoadFlowSolver.SetVBus(busIndex: integer; Volt: complex);
begin
  VBus[busIndex] := Volt;
end;

function TLoadFlowSolver.GetSBus(busIndex: integer): complex;
begin
  Result := PQ[busIndex];
end;

procedure TLoadFlowSolver.SetSBus(busIndex: integer; S: complex);
begin
  PQ[busIndex] := S;
end;

procedure TLoadFlowSolver.GetLineBuss(out Bus2Index, Bus1Index: integer;
  const EL: TIntegerObjectMap; const Line: TLine; const LoadFlowBuss: TObjectList);
begin
  if Line.CB1Close then
    Bus1Index := GLFBI(Line.ID, Line.GetConection(0), LoadFlowBuss, EL)
  else
    Bus1Index := GLFBI(Line.ID, Line.ID, LoadFlowBuss, EL);
  if Line.CB2Close then
    Bus2Index := GLFBI(Line.ID, Line.GetConection(1), LoadFlowBuss, EL)
  else
    Bus2Index := GLFBI(Line.ID, Line.ID, LoadFlowBuss, EL);
end;

procedure TLoadFlowSolver.UpdateVoltage;
begin
  raise Exception.Create('not implemenrted UpdateVoltage');
end;

end.
