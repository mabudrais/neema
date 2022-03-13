unit StateEstimationUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LoadFlowSolverUnit, TransFormer2WUnit,
  TransFormer3WUnit, TransFormer3WShuntUnit, ConectionUnit,
  busunit, TransmissionLineUnit, ElectricElentUnit, Math, UtilUnit,
  contnrs, elementUnit, DrawingUnit, UComplex, multibusunit, ImpedanceUnit,
  GenircDataUnit, LoadUnit, UnusedVarUnit, EEMartixUnit, SEMeasurementUnit,
  ObservityUnit, PiImpedanceUnit, ShuntUnit;

type

  { TEEEquation }

  TEEEquation = class
    W: integer;//weight 0 to 100
    procedure AddError(EQlist: TStringList; index: integer); virtual;
    procedure AddVDiffren(EstimatedBusIndex: TIntegerList; EQlist: TStringList;
      index, shift: integer); virtual;
    procedure UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
      DelShift: integer); virtual;
    procedure SetIndexs(EstimatedBusIndex: TIntegerList; index: integer); virtual;
  end;

  { TBusEquation }

  TBusEquation = class(TEEEquation)
    v1: integer;
    Vvalue: double;
    constructor Create(cv1: integer; CVvalue: double);
    procedure SetIndexs(EstimatedBusIndex: TIntegerList; index: integer); override;
    procedure UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
      DelShift: integer); override;
  private
    RowIndex: integer;
    ColmonIndex: longint;
  end;

  { TPowerEquation }

  TPowerEquation = class(TEEEquation)
    p, X, G: double;
    v1, v2: integer;
    constructor Create(cv1, cv2: integer; cp, cX, cG: double);
    procedure SetIndexs(EstimatedBusIndex: TIntegerList; index: integer); override;
    procedure UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
      DelShift: integer); override;
  private
    RowIndex: integer;
    V2Colmon: longint;
    V1Colmon: longint;
  end;

  { TVAREquation }

  TVAREquation = class(TPowerEquation)
    X2, G2: double;
    constructor Create(cv1, cv2: integer; cp, cX, cG, cX2, cG2: double);
    procedure UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
      DelShift: integer); override;
  end;

  { TPowerSumEquation }

  TPowerSumEquation = class(TEEEquation)
    p, X, G: TDoubleList;
    OtherV: TIntegerList;
    v1: integer;
    constructor Create(MainVusIndex: integer);
    procedure SetIndexs(EstimatedBusIndex: TIntegerList; index: integer); override;
    procedure UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
      DelShift: integer); override;
  private
    RowIndex: integer;
    OtherVColmon: TIntegerList;
    V1Colmon: longint;
    procedure Dvx(k, v2, DelShift: integer; v, d: array of extended;
      const HM: TEEMartix); virtual;
    function Getcdpdv1Andcdpdd1AndcE(k, v2: integer; v, d: array of extended;
      out cdpdd1, cE: extended): extended; virtual;
  end;

  { TVarSumEquation  }

  TVarSumEquation = class(TPowerSumEquation)
    X2, G2: TDoubleList;
    constructor Create(MainVusIndex: integer);
    procedure Dvx(k, v2, DelShift: integer; v, d: array of extended;
      const HM: TEEMartix); override;
    function Getcdpdv1Andcdpdd1AndcE(k, v2: integer; v, d: array of extended;
      out cdpdd1, cE: extended): extended; override;
  end;

  { TES }

  TES = class(TLoadFlowSolver)
  public
    Measurments: TObjectList;
    EstimatedBusIndex: TIntegerList;
    KVMVAA: boolean;
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList): boolean;
      override;
    function Step: boolean; override;
    constructor Create();
    destructor Destroy; override;
    procedure UpdateVoltage; override;
    procedure UpdateDrawing(Drawing: TObject; LoadFlowBuss: TObjectList); override;
  private
    Equations, CorrectionList: TObjectList;
    DemathEquation: TStringList;
    EqNUM, UCB: integer;
    H, er, RMat, HT: TEEMartix;
    HtR, HtRH, Rer, HtRer, dx, Calcx: TEEMartix;
    v, d: array of extended;
    procedure AddSlackBusMeasurmentVolt(const k: integer; LFBus: TLoadFlowBus;
      const Drawing: TObject);
    procedure CorrectElementObservablty(cE: TElicment; EL: TIntegerObjectMap;
      BSA: TBreakerStatusArray);
    function FindUnObservableElements(EL: TIntegerObjectMap): boolean;
    function getBusIndex(const bus: TBus; LoadFlowBuss: TObjectList): integer;
    function GetMultiBusIndex(const bus: TMultiBus; SubIndex: integer;
      LoadFlowBuss: TObjectList): integer;
    procedure LineEquation(cline: TLine; elelist: TIntegerObjectMap;
      LoadFlowBuss: TObjectList);
    procedure MultiBusWEquation(MultiBus: TObject; LoadFlowBuss: TObjectList);
    procedure ObservablityCorrection(EL: TIntegerObjectMap);
    procedure Trans2WEquation(e: TObject; Drawing: TObject;
      LoadFlowBuss: TObjectList);
    procedure BusEquation(b: TBus; LoadFlowBuss: TObjectList);
    procedure BuildMaximaEquation(Drawing: TObject; LoadFlowBuss: TObjectList);
    procedure DimathPi(const Measur: TCustemPimeasurmen; const b1i, b2i: integer;
      imp1, imp2, imp3: complex; cline: TElicment);
    procedure PowerVarSumEquation(imp1, imp2: array of complex;
      vindex: array of integer; MainBus: integer);
    function DMathBusEquation(M: TBusmeasurmen; cbusindex: integer; b: TBus): boolean;
    procedure UpdateBusLoad(Drawing: TObject; LoadFlowBuss: TObjectList);
    procedure Trans3WEquation(e: Pointer; el: TIntegerObjectMap;
      LoadFlowBuss: TObjectList);
    procedure ObservablityCHeck(SlackBusIndex: integer);
    procedure IniMatrix(Drawing: TObject; LoadFlowBuss: TObjectList);
    procedure TRans3wShuntEquation(Transformer: TTransFormer3WShunt;
      const EL: TIntegerObjectMap; LoadFlowBuss: TObjectList);
    procedure UpdateElementObservablty(cE: TElicment; EL: TIntegerObjectMap;
      BSA: TBreakerStatusArray);
    function VoltageEquation(const aV: double; const BaseVolt: string;
      const cbusindex: integer): boolean;
  end;

implementation

{ TVarSumEquation }

constructor TVarSumEquation.Create(MainVusIndex: integer);
begin
  inherited Create(MainVusIndex);
  x2 := TDoubleList.Create;
  G2 := TDoubleList.Create;
end;

procedure TVarSumEquation.Dvx(k, v2, DelShift: integer; v, d: array of extended;
  const HM: TEEMartix);
begin
  HM[RowIndex, OtherVColmon[k]] := (-(sin(g[k] - d[v2] - d[v1]) * v[v1]) / x[k]);
  HM[RowIndex, OtherVColmon[k] + DelShift] :=
    (cos(g[k] - d[v2] + d[v1]) * v[v1] * v[v2]) / x[k];
end;

function TVarSumEquation.Getcdpdv1Andcdpdd1AndcE(k, v2: integer;
  v, d: array of extended; out cdpdd1, cE: extended): extended;
begin
  Result := (-(v[v2] * sin(g[k] - d[v2] + d[v1])) / x[k] +
    (2 * v[v1] * sin(g[k])) / x[k] - (2 * v[v1]) / x2[k]);
  cdpdd1 := -(cos(g[k] - d[v2] + d[v1]) * v[v1] * v[v2]) / x[k];
  cE := -(v[v1] * v[v1] * sin(g[k]) / x[k] - (v[v1] * v[v2] *
    sin(g[k] + d[v1] - d[v2])) / x[k] - v[v1] * v[v1] / x2[k] * sin(-g2[k]));
end;

{ TPowerSumEquation }

constructor TPowerSumEquation.Create(MainVusIndex: integer);
begin
  v1 := MainVusIndex;
  OtherV := TIntegerList.Create;
  X := TDoubleList.Create;
  G := TDoubleList.Create;
  OtherVColmon := TIntegerList.Create;
  W := 1000;
end;

procedure TPowerSumEquation.SetIndexs(EstimatedBusIndex: TIntegerList; index: integer);
var
  k: integer;
begin
  RowIndex := index;
  V1Colmon := EstimatedBusIndex.IndexOf(v1);
  for k := 0 to OtherV.Count - 1 do
    OtherVColmon.Add(EstimatedBusIndex.IndexOf(OtherV[k]));
end;

procedure TPowerSumEquation.UpdateMatrixs(HM, EM: TEEMartix;
  v, d: array of extended; DelShift: integer);
var
  dpdv1, dpdd1, E, Ce, cdpdv1, cdpdd1: extended;
  v2: longint;
  k: integer;
begin
  if V1Colmon < 0 then
    raise Exception.Create('slack bus!');
  dpdv1 := 0;
  dpdd1 := 0;
  E := 0;
  for k := 0 to OtherV.Count - 1 do
  begin
    v2 := OtherV[k];
    cdpdv1 := Getcdpdv1Andcdpdd1AndcE(k, v2, v, d, cdpdd1, Ce);
    dpdv1 := dpdv1 + cdpdv1;
    dpdd1 := dpdd1 + cdpdd1;
    if OtherVColmon[k] > -1 then
      Dvx(k, v2, DelShift, v, d, HM);
    E := E + ce;
  end;
  HM[RowIndex, V1Colmon] := dpdv1;
  HM[RowIndex, V1Colmon + DelShift] := dpdd1;
  EM[RowIndex, 0] := E;
end;

function TPowerSumEquation.Getcdpdv1Andcdpdd1AndcE(k, v2: integer;
  v, d: array of extended; out cdpdd1, cE: extended): extended;
begin
  Result := (2 * v[v1] * cos(g[k])) / x[k] - (v[v2] * cos(g[k] - d[v2] + d[v1])) / x[k];
  cdpdd1 := (sin(g[k] - d[v2] + d[v1]) * v[v1] * v[v2]) / x[k];
  Ce := -((v[v1] * v[v1] * cos(g[k])) / x[k] -
    (v[v1] * v[v2] * cos(g[k] + d[v1] - d[v2])) / x[k]);
end;

procedure TPowerSumEquation.Dvx(k, v2, DelShift: integer; v, d: array of extended;
  const HM: TEEMartix);
begin
  HM[RowIndex, OtherVColmon[k]] := -(cos(g[k] - d[v2] + d[v1]) * v[v1]) / x[k];
  HM[RowIndex, OtherVColmon[k] + DelShift] :=
    -(sin(g[k] - d[v2] + d[v1]) * v[v1] * v[v2]) / x[k];
end;

procedure TBusEquation.SetIndexs(EstimatedBusIndex: TIntegerList; index: integer);
begin
  RowIndex := index;
  ColmonIndex := EstimatedBusIndex.IndexOf(Self.v1);
end;

constructor TBusEquation.Create(cv1: integer; CVvalue: double);
begin
  Self.v1 := cv1;
  Self.Vvalue := CVvalue;
  w := 1000 * 100; //multply by handred because of in other equation base power is 100
  //but for voltage base is 1
end;

procedure TBusEquation.UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
  DelShift: integer);
begin
  UseInt(DelShift);
  UseExtended(d[0]);
  HM[RowIndex, ColmonIndex] := 1;
  EM[RowIndex, 0] := Vvalue - v[v1];
end;

{ TEEEquation }

procedure TEEEquation.AddError(EQlist: TStringList; index: integer);
begin
  UseObject(EQlist);
  UseInt(index);
  raise Exception.Create('this is an abstract method');
end;

procedure TEEEquation.AddVDiffren(EstimatedBusIndex: TIntegerList;
  EQlist: TStringList; index, shift: integer);
begin
  UseObject(EstimatedBusIndex);
  UseObject(EQlist);
  UseInt(index);
  UseInt(shift);
  raise Exception.Create('this is an abstract method');
end;

procedure TEEEquation.UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
  DelShift: integer);
begin
  UseObject(HM);
  UseObject(EM);
  UseExtended(v[0]);
  UseExtended(d[0]);
  UseInt(DelShift);
  raise Exception.Create('must be implemented');
end;

procedure TEEEquation.SetIndexs(EstimatedBusIndex: TIntegerList; index: integer);
begin
  UseObject(EstimatedBusIndex);
  UseInt(index);
  raise Exception.Create('must be implemented');
end;


constructor TVAREquation.Create(cv1, cv2: integer; cp, cX, cG, cX2, cG2: double);
var
  sing2: extended;
begin
  inherited Create(cv1, cv2, cp, cX, cG);
  //this eqoation asume that g2 near 90
  sing2 := sin(cG2);
  if (abs(sing2) < 0.9) or (abs(sing2) > 1.1) then
    raise Exception.Create(' g2 is too small');
  x2 := cX2;
  g2 := cG2;
end;

procedure TVAREquation.UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
  DelShift: integer);
var
  TempV: extended;
begin
  if V1Colmon > -1 then
  begin
    HM[RowIndex, V1Colmon] := (-(v[v2] * sin(g - d[v2] + d[v1])) /
      x + (2 * v[v1] * sin(g)) / x - (2 * v[v1]) / x2);
    HM[RowIndex, V1Colmon + DelShift] := -(cos(g - d[v2] + d[v1]) * v[v1] * v[v2]) / x;
  end;
  if V2Colmon > -1 then
  begin
    HM[RowIndex, V2Colmon] := (-(sin(g - d[v2] - d[v1]) * v[v1]) / x);
    HM[RowIndex, V2Colmon + DelShift] := (cos(g - d[v2] + d[v1]) * v[v1] * v[v2]) / x;
  end;
  TempV := v[v1] * v[v1] * sin(g) / x;
  TempV := -(v[v1] * v[v2] * sin(g + d[v1] - d[v2])) / x;
  TempV := -v[v1] * v[v1] / x2 * sin(-g2);
  TempV := v[v1] * v[v1] * sin(g) / x - (v[v1] * v[v2] * sin(g + d[v1] - d[v2])) /
    x - v[v1] * v[v1] / x2 * sin(-g2);
  EM[RowIndex, 0] := Self.p - TempV;
end;

{ TPowerEquation }

constructor TPowerEquation.Create(cv1, cv2: integer; cp, cX, cG: double);
begin
  Self.v1 := cv1;
  Self.v2 := cv2;
  Self.X := cX;
  Self.G := cG;
  p := cp;
  w := 1000;
end;

procedure TPowerEquation.SetIndexs(EstimatedBusIndex: TIntegerList; index: integer);
begin
  RowIndex := index;
  V1Colmon := EstimatedBusIndex.IndexOf(v1);
  V2Colmon := EstimatedBusIndex.IndexOf(v2);
end;

procedure TPowerEquation.UpdateMatrixs(HM, EM: TEEMartix; v, d: array of extended;
  DelShift: integer);
begin
  if V1Colmon > -1 then
  begin
    HM[RowIndex, V1Colmon] := (2 * v[v1] * cos(g)) / x -
      (v[v2] * cos(g - d[v2] + d[v1])) / x;

    HM[RowIndex, V1Colmon + DelShift] := (sin(g - d[v2] + d[v1]) * v[v1] * v[v2]) / x;
  end;
  if V2Colmon > -1 then
  begin
    HM[RowIndex, V2Colmon] := -(cos(g - d[v2] + d[v1]) * v[v1]) / x;
    HM[RowIndex, V2Colmon + DelShift] := -(sin(g - d[v2] + d[v1]) * v[v1] * v[v2]) / x;
  end;
  EM[RowIndex, 0] := Self.p - ((v[v1] * v[v1] * cos(g)) / x -
    (v[v1] * v[v2] * cos(g + d[v1] - d[v2])) / x);
end;


{ TES }

function TES.Step: boolean;
var
  k: integer;
  index: longint;
  dxMax: extended;
begin
  Result := False;
  for k := 0 to Equations.Count - 1 do
    TEEEquation(Equations[k]).UpdateMatrixs(H, er, v, d, UCB);
  // h.print();
  //er.print();
  h.transpos(hT);
  ht.multwith(RMat, HtR);
  HtR.multwith(h, HtRH);
  HtRH.inv();
  HtRH.multwith(0.5);
  RMat.multwith(er, Rer);
  ht.multwith(Rer, HtRer);
  HtRer.multwith(2);
  HtRH.multwith(HtRer, dx);
  for k := 0 to EstimatedBusIndex.Count - 1 do
  begin
    index := EstimatedBusIndex[k];
    v[index] := v[index] + dx[k, 0];
    d[index] := d[index] + dx[k + UCB, 0];
  end;
  dxMax := dx.Max();
  if dxMax < 1e-4 then
    Result := True;
end;

function TES.Ini(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList): boolean;
begin
  if not (inherited ini(Drawing, LoadFlowBuss, VariableElements)) then
    Exit(False);
  if FindUnObservableElements((Drawing as TDrawing).GetElementList) then
    Exit(False);
  BuildMaximaEquation(Drawing, LoadFlowBuss);
  IniMatrix(Drawing, LoadFlowBuss);
  Result := True;
end;

procedure TES.IniMatrix(Drawing: TObject; LoadFlowBuss: TObjectList);
var
  k, NumOfVar: integer;
  LFBus, LFB0: TLoadFlowBus;
  Eq: TEEEquation;
begin
  UCB := 0;
  LFB0 := (LoadFlowBuss[0] as TLoadFlowBus);
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFBus := (LoadFlowBuss[k] as TLoadFlowBus);
    if LFBus.BusType <> slackbus then
      Inc(UCB);
  end;
  NumOfVar := UCB * 2;//two for each bus (v and d)
  EqNUM := Equations.Count;
  H := TEEMartix.Create(EqNUM, NumOfVar);
  HT := TEEMartix.Create(NumOfVar, EqNUM);
  er := TEEMartix.Create(EqNUM, 1);
  RMat := TEEMartix.Create(EqNUM, EqNUM);
  //intermidiate  matrix
  HtR := TEEMartix.Create(NumOfVar, EqNUM);
  HtRH := TEEMartix.Create(NumOfVar, NumOfVar);
  Rer := TEEMartix.Create(EqNUM, 1);
  HtRer := TEEMartix.Create(NumOfVar, 1);
  dx := TEEMartix.Create(NumOfVar, 1);
  Calcx := TEEMartix.Create(NumOfVar, 1);
  SetLength(v, LoadFlowBuss.Count);
  SetLength(d, LoadFlowBuss.Count);
  EstimatedBusIndex := TIntegerList.Create;
  Randomize;
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFBus := (LoadFlowBuss[k] as TLoadFlowBus);
    if LFBus.BusType <> slackbus then
    begin
      EstimatedBusIndex.Add(k);
      v[k] := 0.9;
      d[k] := 0.001;
    end
    else
      AddSlackBusMeasurmentVolt(k, LFBus, Drawing);
  end;

  for k := 0 to Equations.Count - 1 do
  begin
    Eq := Equations[k] as TEEEquation;
    Eq.SetIndexs(EstimatedBusIndex, k);
    RMat[k, k] := 1 / Eq.W;
  end;
  RMat.inv();
end;

procedure TES.UpdateElementObservablty(cE: TElicment; EL: TIntegerObjectMap;
  BSA: TBreakerStatusArray);
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
        if ElicE.IsObservableByEstateEstimator then
          cE.IsObservableByEstateEstimator := True;
        if cE.IsObservableByEstateEstimator then
          ElicE.IsObservableByEstateEstimator := True;
      end
      else if ElicE is TMultiBus then
      begin
        cMulti := ElicE as TMultiBus;
        if cMulti.IsSubOByEE[cMulti.GetSubBusIndex(ce.ID)] then
          cE.IsObservableByEstateEstimator := True;
        if cE.IsObservableByEstateEstimator then
        begin
          SubBusIndex := cMulti.GetSubBusIndex(ce.ID);
          cMulti.IsSubOByEE[SubBusIndex] := True;
          if cMulti.BC12Closed then
            cMulti.IsSubOByEE[OtherSubBus(SubBusIndex, cMulti)] := True;
        end;
      end
      else
        raise Exception.Create(ce.ClassName +
          ' is connected to element which is not bus or multibus');
    end;
  end;
end;

function TES.VoltageEquation(const aV: double; const BaseVolt: string;
  const cbusindex: integer): boolean;
begin
  Result := False;
  if not SameValue(aV, NAM) then
  begin
    Equations.Add(TBusEquation.Create(cbusindex, aV / StringToVolt(BaseVolt)));
    Result := True;
  end;
end;

function TES.FindUnObservableElements(EL: TIntegerObjectMap): boolean;
var
  E: TObject;
  cMultiBus: TMultiBus;
  k, m: integer;
  SlackBusList: TObjectList;
  IsConnector: array of boolean;
  Elec: TElicment;
begin
  Result := False;
  for E in El do
  begin
    if E is TBus then
      (E as TBus).IsObservableByEstateEstimator := ((E as TBus).BusType = slackbus)
    else if E is TMultiBus then
    begin
      cMultiBus := E as TMultiBus;
      cMultiBus.IsSubOByEE[0] := (cMultiBus.Bus1Type = slackbus);
      cMultiBus.IsSubOByEE[1] := (cMultiBus.Bus2Type = slackbus);
      if (cMultiBus.BC12Closed) and ((cMultiBus.IsSubOByEE[0])) then
        cMultiBus.IsSubOByEE[1] := True;
      if (cMultiBus.BC12Closed) and ((cMultiBus.IsSubOByEE[1])) then
        cMultiBus.IsSubOByEE[0] := True;
    end
    else if E is TElicment then
      (E as TElicment).IsObservableByEstateEstimator := False;
  end;
  //very bad code
  SlackBusList := TObjectList.Create(False);
  SetLength(IsConnector, multibusunit.MaxSubBusNum * el.Count);
  FillConectorArray(SlackBusList, IsConnector, EL);
  SlackBusList.Free;
  for k := 0 to EL.Count do
  begin
    for m := 0 to El.Count - 1 do
    begin
      E := EL.Data[m];
      if (E as TElement).Name.Contains('MAR') then
      begin
        if (E is TMultiBus) then
        begin
          //Cmult := (E as TMultiBus);
          // Cmult.Dead;
        end;
      end;
      if not IsConnector[m] then
        Continue;
      if E is TImpedance then
        UpdateElementObservablty(E as TImpedance, EL,
          TBreakerStatusArray.Create(True, True))
      else if E is TLine then
        UpdateElementObservablty(E as TLine, EL, TBreakerStatusArray.Create(
          (E as TLine).CB1Close, (E as TLine).CB2Close))
      else if E is TTransFormer2W then
        UpdateElementObservablty(E as TTransFormer2W, EL,
          TBreakerStatusArray.Create((E as TTransFormer2W).CB1Close,
          (E as TTransFormer2W).CB2Close))
      else if E is TTransFormer3WShunt then
        UpdateElementObservablty(E as TTransFormer3WShunt, EL,
          TBreakerStatusArray.Create((E as TTransFormer3WShunt).CB1Close,
          (E as TTransFormer3WShunt).CB2Close))
      else if E is TTransFormer3W then
        UpdateElementObservablty(E as TTransFormer3W, EL,
          TBreakerStatusArray.Create((E as TTransFormer3W).CB1Close,
          (E as TTransFormer3W).CB2Close, (E as TTransFormer3W).CB3Close))
      else if E is TShunt then
        UpdateElementObservablty(E as TShunt, EL,
          TBreakerStatusArray.Create((E as TShunt).CBClose))
      else if E is TPiImpedance then
        UpdateElementObservablty(E as TPiImpedance, EL, TBreakerStatusArray.Create(
          True, True));
    end;
  end;
  SetLength(IsConnector, 0);
  ObservablityCorrection(EL);
  for E in EL do
  begin
    if E is TElicment then
    begin
      Elec := E as TElicment;
      if not (Elec.IsObservableByEstateEstimator) and
        (not ((Elec is TBus) or (Elec is TMultiBus) or (Elec is TConection))) then
      begin
        if not Elec.Dead then
        begin
          Exit(True);
          //you should open cb directly you need to send massage for undo redo
        end;
      end;
    end;
  end;
end;

procedure TES.CorrectElementObservablty(cE: TElicment; EL: TIntegerObjectMap;
  BSA: TBreakerStatusArray);
var
  k: integer;
  ElicE: TElicment;
  cMulti: TMultiBus;
  CEDS: array of boolean;//conected elemnt observ state
  ObservableBySide: boolean;
begin
  if cE.ConectionCount <> Length(BSA) then
    raise Exception.Create('wrong conection');
  if (cE.IsObservableByEstateEstimator) or (Ce.Dead) then
    Exit;
  SetLength(CEDS, Length(BSA));
  for k := 0 to Length(BSA) - 1 do
  begin
    if BSA[k] = True then
    begin
      ElicE := EL[cE.GetConection(k)] as TElicment;
      if ElicE is TBus then
        CEDS[k] := ElicE.IsObservableByEstateEstimator
      else if ElicE is TMultiBus then
      begin
        cMulti := ElicE as TMultiBus;
        if cMulti.IsSubOByEE[cMulti.GetSubBusIndex(ce.ID)] then
          CEDS[k] := True;
      end
      else
        raise Exception.Create(ce.ClassName +
          ' is connected to element which is not bus or multibus');
    end;
  end;
  //may problem with 3w trans with on CB close
  ObservableBySide := True;
  for k := 0 to Length(BSA) - 1 do
  begin
    if (BSA[k]) and (not CEDS[k]) then
      ObservableBySide := False;
  end;
  cE.IsObservableByEstateEstimator := ObservableBySide;
end;

procedure TES.AddSlackBusMeasurmentVolt(const k: integer; LFBus: TLoadFlowBus;
  const Drawing: TObject);
var
  Bus: TBus;
  B: TElement;
  MB: TMultiBus;
begin
  B := (Drawing as TDrawing).GetElementList[LFBus.DrawingItemID] as TElement;
  if B is TBus then
  begin
    Bus := B as TBus;
    if not IsAm(Bus.M.V) then
      raise Exception.Create('slack Bus ' + Bus.Name + ',ID ' +
        Bus.ID.ToString() + ' has' + ' no measrment');
    V[k] := Bus.VoltToPerUnt(Bus.M.V).re;
  end
  else if B is TMultiBus then
  begin
    MB := B as TMultiBus;
    if (LFBus.SubMultiPlex and 1) = 1 then
    begin
      if not IsAm(MB.M.V1) then
        raise Exception.Create('slack Bus ' + MB.Name + ',ID ' +
          MB.ID.ToString() + 'Sub bus' + LFBus.SubMultiPlex.ToString() +
          'has' + ' no measrment');
      V[k] := MB.VoltToPerUnt(MB.M.V1).re;
    end
    else if (LFBus.SubMultiPlex and 2) = 2 then
    begin
      if not IsAm(MB.M.V2) then
        raise Exception.Create('slack Bus ' + MB.Name + ',ID ' +
          MB.ID.ToString() + 'Sub bus' + LFBus.SubMultiPlex.ToString() +
          'has' + ' no measrment');
      V[k] := MB.VoltToPerUnt(MB.M.V2).re;
    end
    else
      raise Exception.Create('un suported Bus index');
  end;
  d[k] := 0;
end;

procedure TES.ObservablityCorrection(EL: TIntegerObjectMap);
var
  m: integer;
  E: TObject;
begin
  for m := 0 to El.Count - 1 do
  begin
    E := EL.Data[m];
    if E is TImpedance then
      CorrectElementObservablty(E as TImpedance, EL,
        TBreakerStatusArray.Create(True, True))
    else if E is TLine then
      CorrectElementObservablty(E as TLine, EL, TBreakerStatusArray.Create(
        (E as TLine).CB1Close, (E as TLine).CB2Close))
    else if E is TTransFormer2W then
      CorrectElementObservablty(E as TTransFormer2W, EL,
        TBreakerStatusArray.Create((E as TTransFormer2W).CB1Close,
        (E as TTransFormer2W).CB2Close))
    else if E is TTransFormer3WShunt then
      CorrectElementObservablty(E as TTransFormer3WShunt, EL,
        TBreakerStatusArray.Create((E as TTransFormer3WShunt).CB1Close,
        (E as TTransFormer3WShunt).CB2Close))
    else if E is TTransFormer3W then
      CorrectElementObservablty(E as TTransFormer3W, EL,
        TBreakerStatusArray.Create((E as TTransFormer3W).CB1Close,
        (E as TTransFormer3W).CB2Close, (E as TTransFormer3W).CB3Close))
    else if E is TShunt then
      CorrectElementObservablty(E as TShunt, EL,
        TBreakerStatusArray.Create((E as TShunt).CBClose))
    else if E is TPiImpedance then
      CorrectElementObservablty(E as TPiImpedance, EL, TBreakerStatusArray.Create(
        True, True));
  end;
end;

procedure TES.ObservablityCHeck(SlackBusIndex: integer);
{var
  k, m: integer;
  IsObservable: array of boolean;
  NotObservableFound: boolean;
  NotObservableElementFiLe: TStringList;
  ParentElement, Cbus: TElicment;}
begin
 { SetLength(IsObservable, EstimatedBusIndex.Count);
  for k := 0 to EstimatedBusIndex.Count - 1 do
    IsObservable[k] := False;
  for k := 0 to Observ1.Count - 1 do
  begin
    for m := 0 to Observ1.Count - 1 do
    begin
      if (Observ1[m] = SlackBusIndex) or (Observ2[m] = SlackBusIndex) then
      begin
        IsObservable[Observ1[m]] := True;
        IsObservable[Observ2[m]] := True;
      end
      else if (IsObservable[Observ1[m]] = True) or (IsObservable[Observ2[m]] = True) then
      begin
        IsObservable[Observ1[m]] := True;
        IsObservable[Observ2[m]] := True;
      end;
    end;
  end;
  NotObservableFound := False;
  NotObservableElementFiLe := TStringList.Create;
  for k := 0 to EstimatedBusIndex.Count - 1 do
  begin
    if not IsObservable[k] then
    begin
      NotObservableFound := True;
      Cbus := TElicment(buslist[k]);
      ParentElement := utilUnit.GetParentElement(Cbus, elist);
      if ParentElement <> nil then
        NotObservableElementFiLe.Add(IntToStr(Self.elist.IndexOfData(ParentElement)) +
          ',' + ParentElement.element_name)
      else
        NotObservableElementFiLe.Add(IntToStr(Self.elist.IndexOfData(Cbus)) +
          ',' + cbus.element_name);
    end;
  end;
  NotObservableElementFiLe.SaveToFile('NotObservableElement.txt');
  NotObservableElementFiLe.Free;
  if NotObservableFound then
    raise Exception.Create('non observable bus ' + BusName[k]);  }
end;

function TES.getBusIndex(const bus: TBus; LoadFlowBuss: TObjectList): integer;
var
  k: integer;
  LFB: TLoadFlowBus;
begin
  Result := -1;
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFB := (LoadFlowBuss[k] as TLoadFlowBus);
    if LFB.DrawingItemID = bus.ID then
    begin
      if LFB.SubMultiPlex = 0 then
        Exit(k)
      else
        raise Exception.Create('Tbus with sub bus');
    end;
  end;
end;

function TES.GetMultiBusIndex(const bus: TMultiBus; SubIndex: integer;
  LoadFlowBuss: TObjectList): integer;
var
  k: integer;
  LFB: TLoadFlowBus;
begin
  Result := -1;
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    LFB := (LoadFlowBuss[k] as TLoadFlowBus);
    if LFB.DrawingItemID = bus.ID then
    begin
      if LFB.SubMultiPlex = 0 then
        raise Exception.Create('TMultiBus with no sub bus');
      if (SubIndex = 0) and ((LFB.SubMultiPlex and 1) = 1) then
        Exit(k)
      else if (SubIndex = 1) and ((LFB.SubMultiPlex and 2) = 2) then
        Exit(k);
    end;
  end;
end;

procedure TES.Trans3WEquation(e: Pointer; el: TIntegerObjectMap;
  LoadFlowBuss: TObjectList);
var
  Transformer: TTransFormer3W;
  oz1, oz2, oz3, oz1_1m, oz2_1m, oz3_1m, oz1_2m, oz2_2m, oz3_2m,
  oz1_3m, oz2_3m, oz3_3m: complex;
  bus1index, bus2index, bus3index, CenterBusIndex: integer;
  TPM: Tpimeasurmen;  //temp pi measurement
begin
  TPM := Tpimeasurmen.Create('ll');
  bus1index := -1;
  bus2index := -1;
  bus3index := -1;
  Transformer := TTransFormer3W(e);
  if (Transformer.Dead) or (not Transformer.IsObservableByEstateEstimator) then
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
    Transformer.GetPrimeryWindingPiModelImpPerUnit(oz1_1m, oz2_1m, oz3_1m);
    DimathPi(Transformer.M.Set1(TPM), bus1index, CenterBusIndex, oz1_1m,
      oz2_1m, oz3_1m, Transformer);
    Transformer.GetSecondryWindingPiModelImpPerUnit(oz1_2m, oz2_2m, oz3_2m);
    DimathPi(Transformer.M.Set2(TPM), CenterBusIndex, bus2index, oz1_2m,
      oz2_2m, oz3_2m, Transformer);
    Transformer.GetTertiaryWindingPiModelImpPerUnit(oz1_3m, oz2_3m, oz3_3m);
    DimathPi(Transformer.M.Set3(TPM), CenterBusIndex, bus3index, oz1_3m,
      oz2_3m, oz3_3m, Transformer);
    PowerVarSumEquation([oz2_1m, oz2_2m, oz2_3m], [oz3_1m, oz3_2m, oz3_3m],
      [bus1index, bus2index, bus3index], CenterBusIndex);
  end
  else
  begin
    Transformer.GetPiModelImpPerUnit(oz1, oz2, oz3);
    if Transformer.CB2Close then
      DimathPi(Transformer.M.Set12(TPM), bus1index, bus2index, oz1,
        oz2, oz3, Transformer)
    else
      DimathPi(Transformer.M.Set13(TPM), bus1index, bus3index, oz1,
        oz2, oz3, Transformer);
  end;
  Tpm.Free;
end;

procedure TES.TRans3wShuntEquation(Transformer: TTransFormer3WShunt;
  const EL: TIntegerObjectMap; LoadFlowBuss: TObjectList);
var
  oz1, oz2, oz3: complex;
  Bus1Index, Bus2Index: integer;
  PIM: Tpimeasurmen;
begin
  if (Transformer.Dead) or (not Transformer.IsObservableByEstateEstimator) then
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
  PIM := Tpimeasurmen.Create(Transformer.Name);
  DimathPi(Transformer.M.Set12(PIM), Bus1Index, Bus2Index, oz1, oz2, oz3, Transformer);
  PIM.Free;
end;

procedure TES.Trans2WEquation(e: TObject; Drawing: TObject; LoadFlowBuss: TObjectList);
var
  ct: TTransFormer2W;
  z3, z2, z1: complex;
  EL: TIntegerObjectMap;
  Bus1Index, Bus2Index: integer;
begin
  ct := TTransFormer2W(e);
  if (ct.Dead) or (not ct.IsObservableByEstateEstimator) then
    exit;
  ct.GetImpPerUnit(z1, z2, z3);
  EL := (Drawing as TDrawing).GetElementList;
  Bus1Index := GLFBI(ct.ID, ct.GetConection(0), LoadFlowBuss, EL);
  Bus2Index := GLFBI(ct.ID, ct.GetConection(1), LoadFlowBuss, EL);
  //PiEquation(z1, z2, z3, ct, EL, Bus1Index, Bus2Index, ct.M);
  DimathPi(ct.M, Bus1Index, Bus2Index, z1, z2, z3, ct);
end;

procedure TES.MultiBusWEquation(MultiBus: TObject; LoadFlowBuss: TObjectList);
var
  MB: TMultiBus;
  Bus1Index, Bus2Index: integer;
  LFBus1, LFBus2: TLoadFlowBus;
begin
  MB := TMultiBus(MultiBus);
  if multibusunit.MaxSubBusNum > 2 then
    raise Exception.Create('multibusunit.MaxSubBusNum>2');
  if Mb.IsSubOByEE[0] then
  begin
    Bus1Index := GetMultiBusIndex(MB, 0, LoadFlowBuss);
    if (Bus1Index < 0) then
      raise Exception.Create('load flow bus can''t be found ' + Mb.Name + ' Bus1');
    LFBus1 := LoadFlowBuss[Bus1Index] as TLoadFlowBus;
    if LFBus1.BusType <> slackbus then
      VoltageEquation(MB.M.V1, MB.BaseVolt, Bus1Index);
  end;
  if Mb.IsSubOByEE[1] then
  begin
    Bus2Index := GetMultiBusIndex(MB, 0, LoadFlowBuss);
    if (Bus2Index < 0) then
      raise Exception.Create('load flow bus can''t be found ' + Mb.Name + ' Bus2');
    LFBus2 := LoadFlowBuss[Bus2Index] as TLoadFlowBus;
    if LFBus2.BusType <> slackbus then
      VoltageEquation(MB.M.V2, MB.BaseVolt, Bus2Index);
  end;
end;

function TES.DMathBusEquation(M: TBusmeasurmen; cbusindex: integer; b: TBus): boolean;
begin
  Result := VoltageEquation(M.V, b.BaseVolt, cbusindex);
end;

procedure TES.BusEquation(b: TBus; LoadFlowBuss: TObjectList);
var
  cbusindex: integer;
begin
  if (b.Dead) or (not b.IsObservableByEstateEstimator) then
    Exit;
  cbusindex := getBusIndex(b, LoadFlowBuss);
  if (cbusindex < 0) then
    //raise Exception.Create('bus index -1');
    Exit;
  if bustype[cbusindex] = slackbus then
    Exit;
  DMathBusEquation(b.M, cbusindex, b);
end;

procedure TES.LineEquation(cline: TLine; elelist: TIntegerObjectMap;
  LoadFlowBuss: TObjectList);
var
  Limp1, Limp2, Limp3: complex;
  Bus2Index, Bus1Index: integer;
begin
  cline.GetImpPerUnit(Limp1, Limp2, Limp3);
  GetLineBuss(Bus2Index, Bus1Index, elelist, cLine, LoadFlowBuss);
  DimathPi(cline.M, Bus1Index, Bus2Index, Limp1, Limp2, Limp3, cline);
end;


constructor TES.Create();
begin
  //inherited Create(list);
  KVMVAA := False;
  Measurments := TObjectList.Create;
  CorrectionList := TObjectList.Create;
  DemathEquation := TStringList.Create;
end;

destructor TES.Destroy;
begin
  inherited Destroy;
  self.Measurments.Free;
  Self.Equations.Free;
  Self.DemathEquation.Free;
  CorrectionList.Free;
end;

procedure TES.BuildMaximaEquation(Drawing: TObject; LoadFlowBuss: TObjectList);
var
  k: integer;
  e: TElicment;
  elelist: TIntegerObjectMap;
  o: TObject;
begin
  Equations := TObjectList.Create(True);
  elelist := (Drawing as TDrawing).GetElementList;
  for k := 0 to elelist.Count - 1 do
  begin
    o := elelist.Data[k];
    if not (o is TElicment) then Continue;
    e := o as TElicment;
    if e.Dead then
      Continue;
    if e is TTransFormer3W then
      Trans3WEquation(e as TTransFormer3W, (Drawing as TDrawing).GetElementList,
        LoadFlowBuss)
    else if e is TTransFormer3WShunt then
      TRans3wShuntEquation(e as TTransFormer3WShunt,
        (Drawing as TDrawing).GetElementList, LoadFlowBuss)
    else if e is TTransFormer2W then
      Trans2WEquation(e as TTransFormer2W, Drawing, LoadFlowBuss)
    else if e is TLine then
      LineEquation(e as TLine, (Drawing as TDrawing).GetElementList, LoadFlowBuss)
    else if e is TBus then
      BusEquation(e as TBus, LoadFlowBuss)
    else if e is TMultiBus then
      MultiBusWEquation(e as TMultiBus, LoadFlowBuss);
  end;
end;

procedure TES.DimathPi(const Measur: TCustemPimeasurmen; const b1i, b2i: integer;
  imp1, imp2, imp3: complex; cline: TElicment);
var
  z1, g1, z2, g2, z3, g3, Mult, cp1, cp2, cq1, cq2: double;
begin
  z1 := cmod(imp1);
  g1 := carg(imp1);
  z2 := cmod(imp2);
  g2 := carg(imp2);
  z3 := cmod(imp3);
  g3 := carg(imp3);
  Mult := 1;
  if KVMVAA then
    Mult := 1e6;
  cp1 := Measur.p1;
  cp2 := Measur.p2;
  cq1 := Measur.q1;
  cq2 := Measur.q2;
  if not SameValue(cp1, NAM) then
    Equations.Add(TPowerEquation.Create(b1i, b2i,
      cline.pfMVA(Measur.p1 * Mult).re, z2, g2));
  if not SameValue(cp2, NAM) then
    Equations.Add(TPowerEquation.Create(b2i, b1i,
      cline.pfMVA(Measur.p2 * Mult).re, z2, g2));
  if not SameValue(cq1, NAM) then
    Equations.Add(TVAREquation.Create(b1i, b2i, cline.pfMVA(Measur.q1 * Mult).re,
      z2, g2, z1, g1));
  if not SameValue(cq2, NAM) then
    Equations.Add(TVAREquation.Create(b2i, b1i, cline.pfMVA(Measur.q2 * Mult).re,
      z2, g2, z3, g3));
end;

procedure TES.PowerVarSumEquation(imp1, imp2: array of complex;
  vindex: array of integer; MainBus: integer);
var
  PSE: TPowerSumEquation;
  k: integer;
  VSE: TVarSumEquation;
begin
  if (Length(imp1) < 2) then
    raise Exception.Create('wrong TPowerSumEquation ');
  PSE := TPowerSumEquation.Create(vindex[0]);
  VSE := TVarSumEquation.Create(vindex[0]);
  VSE.v1 := MainBus;
  PSE.v1 := MainBus;
  for k := 0 to Length(imp1) - 1 do
  begin
    PSE.X.Add(cmod(imp1[k]));
    PSE.G.Add(carg(imp1[k]));
    PSE.OtherV.Add(vindex[k]);
    VSE.X.Add(cmod(imp1[k]));
    vSE.G.Add(carg(imp1[k]));
    VSE.X2.Add(cmod(imp2[k]));
    vSE.G2.Add(carg(imp2[k]));
    vSE.OtherV.Add(vindex[k]);
  end;
  Equations.Add(PSE);
  Equations.Add(VSE);
end;

procedure TES.UpdateBusLoad(Drawing: TObject; LoadFlowBuss: TObjectList);
var
  k, m, bi: integer;
  BusLoadP, BusLoadQ, v1, v2, v1del, v2del, X, ga, y1: double;
  elic: TElicment;
  l: TLoad;
  cone: TObject;
  conbus, Bus: TBus;
  PowerSeted: array of boolean;
  YQ, Ybet: complex;
  DeltaBranch: double;
  lFBus: TLoadFlowBus;
  E: TObject;
  Mbus: TMultiBus;
begin
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    BusLoadP := 0.0;
    BusLoadQ := 0.0;
    v1 := cmod(vbus[k]);
    v1del := carg(vbus[k]);
    Ybet := 0.0;
    for m := 0 to LoadFlowBuss.Count - 1 do
    begin
      y1 := cmod(ybus[k, m]);
      if (m = k) or (y1 < 1e-5) then
        Continue;
      Ybet := Ybet + ybus[k, m];
      v2 := cmod(vbus[m]);
      v2del := carg(vbus[m]);
      X := cmod(1 / ybus[k, m]);
      ga := carg(1 / ybus[k, m]);
      BusLoadQ := BusLoadQ + v1 * v1 * sin(ga) / X - v1 * v2 *
        sin(ga + v1del - v2del) / X;
      BusLoadp := BusLoadP + v1 * v1 * cos(ga) / X - v1 * v2 *
        cos(ga + v1del - v2del) / X;
    end;
    DeltaBranch := 0;
    PQ[k].re := BusLoadP;
    YQ := Ybet - ybus[k, k];//1
    //WL(ybus[k, k].re, ' ', ybus[k, k].im);
    if abs(cmod(Yq)) > 1e-5 then
      DeltaBranch := v1 * v1 * cmod(YQ) * sin(carg(1 / YQ));
    BusLoadQ := BusLoadQ - DeltaBranch;//2
    PQ[k].im := BusLoadQ;
    //there was a problem in equation 1 and 2 I fixed it using try and error !!
    //so problem may appear again /-\
  end;

  SetLength(PowerSeted, LoadFlowBuss.Count);
  for k := 0 to LoadFlowBuss.Count - 1 do
    PowerSeted[k] := False;
  for k := 0 to (Drawing as TDrawing).GetElementList.Count - 1 do
  begin
    E := (Drawing as TDrawing).GetElementList.Data[k];
    if not (E is TElicment) then
      Continue;
    elic := e as TElicment;
    if (elic is TLoad) and (elic.ConectionCount > 0) then
    begin
      raise Exception.Create('TLoad');
      l := elic as TLoad;
      cone := (Drawing as TDrawing).GetElementList[l.GetConection(0)];
      if cone is Tbus then
      begin
        conbus := cone as TBus;
        bi := GLFBI(l.ID, conbus.ID, LoadFlowBuss, (Drawing as TDrawing).GetElementList);
        if (bi > -1) and (not PowerSeted[bi]) then
        begin
          PowerSeted[bi] := True;
          l.setload(PQ[bi].re, PQ[bi].im);
        end;
      end
      else
        raise Exception.Create('multi bus is''nt implemented');
    end;
  end;
  for k := 0 to LoadFlowBuss.Count - 1 do
  begin
    if (not PowerSeted[k]) then
    begin
      lFBus := LoadFlowBuss[k] as TLoadFlowBus;
      if lFBus.IsFictional then
        Continue;
      ConE := (Drawing as TDrawing).GetElementList[lFBus.DrawingItemID] as TElicment;
      if cone is TBus then
      begin
        Bus := cone as TBus;
        Bus.SetPerUnitPQ(PQ[k]);
        Bus.InjectedP := WatToReadableString(StringToWat(Bus.InjectedP));
        Bus.InjectedQ := VarToReadableString(StringToVar(Bus.InjectedQ));
      end
      else if cone is TMultiBus then
      begin
        Mbus := (cone as TMultiBus);
        if (lFBus.SubMultiPlex = 1) or (lFBus.SubMultiPlex = 3) then
        begin
          Mbus.SetActiveBusPerUnitPQ(0, pq[k]);
          Mbus.Injected1P := WatToReadableString(StringToWat(Mbus.Injected1P));
          Mbus.Injected1Q := VarToReadableString(StringToVar(Mbus.Injected1Q));
        end
        else if (lFBus.SubMultiPlex = 2) then
        begin
          Mbus.SetActiveBusPerUnitPQ(1, pq[k]);
          Mbus.Injected2P := WatToReadableString(StringToWat(Mbus.Injected2P));
          Mbus.Injected2Q := VarToReadableString(StringToVar(Mbus.Injected2Q));
        end;
      end
      else
        Exception.Create('LoadFlowBuss conected to non bus');
    end;
  end;
end;

procedure TES.UpdateVoltage;
var
  k: integer;
begin
  for k := 0 to Length(V) - 1 do
  begin
    vbus[k].re := v[k] * cos(d[k]);
    vbus[k].im := v[k] * sin(d[k]);
  end;
end;

procedure TES.UpdateDrawing(Drawing: TObject; LoadFlowBuss: TObjectList);
begin
  inherited UpdateDrawing(Drawing, LoadFlowBuss);
  UpdateBusLoad(Drawing, LoadFlowBuss);
end;

end.
