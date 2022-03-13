unit SEMeasurementUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringSplitPatchUnit, fphttpclient, dateutils,
  fpexprpars, GenircDataUnit
  , typinfo, strutils, Math;

type

  { TMeasurmen }

  TMeasurmen = class
    Abstract
    TargetName: string;
    constructor Create(tn: string);
  end;

  TShuntMeasurmen = class(TMeasurmen)
    I, p, q, V: double;
  end;

  TCustemPimeasurmen = class Abstract(TMeasurmen)
    FI1, FI2, Fp1, Fq1, Fp2, Fq2: double;
    FV1, FV2: double;
    constructor Create(tn: string);
  published
    property I1: double read FI1 write FI1;
    property I2: double read FI2 write FI2;
    property P1: double read FP1 write FP1;
    property Q1: double read FQ1 write FQ1;
    property P2: double read FP2 write FP2;
    property Q2: double read Fq2 write FQ2;
    property V1: double read FV1 write FV1;
    property V2: double read FV2 write FV2;
  end;

  { Tpimeasurmen }

  Tpimeasurmen = class(TCustemPimeasurmen)
  end;

  { T2wMeasurmen }

  TTrans2wMeasurmen = class(TCustemPimeasurmen)
    tap: double;
    constructor Create(tn: string);
  end;

  { TTrans3wMeasurmen }

  TTrans3wMeasurmen = class(TTrans2wMeasurmen)
    FI3, FP3, FQ3, FV3: double;
    constructor Create(tn: string);
    function Set1(Pi: Tpimeasurmen): Tpimeasurmen;
    function Set2(Pi: Tpimeasurmen): Tpimeasurmen;
    function Set3(Pi: Tpimeasurmen): Tpimeasurmen;
    function Set12(Pi: Tpimeasurmen): Tpimeasurmen;
    function Set13(Pi: Tpimeasurmen): Tpimeasurmen;
  published
    property I3: double read FI3 write FI3;
    property P3: double read FP3 write FP3;
    property Q3: double read FQ3 write FQ3;
    property V3: double read FV3 write FV3;
  end;

  { TBusmeasurmen }

  TBusmeasurmen = class(TMeasurmen)
  private
    FV: double;
  public
    constructor Create(tn: string);
  published
    property V: double read FV write FV;
  end;

  { TMultiBusmeasurmen }

  TMultiBusmeasurmen = class(TMeasurmen)
  private
    FV1, FV2: double;
  public
    constructor Create(tn: string);
  published
    property V1: double read FV1 write FV1;
    property V2: double read FV2 write FV2;
  end;

  TNoMeasurmen = class(TMeasurmen)
  end;

  TSample = class
    ID: string;
    V: double;
  end;

  { TEPF }

  TEPF = class//expresion parser function
  public
    TimeString: TDateTime;
    IDList: TGeneralInteger64ObjectMap;
    procedure GfunctionFromServer(var R: TFPExpressionResult;
      const Args: TExprParameterArray);
    procedure GfunctionFromIDList(var R: TFPExpressionResult;
      const Args: TExprParameterArray);
    procedure Dummyfunction(var R: TFPExpressionResult;
      const Args: TExprParameterArray);
    procedure ExtractIDsfunction(var R: TFPExpressionResult;
      const Args: TExprParameterArray);
  end;

procedure SetMeasermentFromDataString(DataString: string; M: TMeasurmen;
  T: TDateTime; EV: TFPExpressionParser; EPF: TEPF);
function UpdateServerdataStringFirstValue(D: string; const P, V: string): string;
function UpdateServerdataStringSecondValue(D: string; const P, V: string): string;
function CanMeasurementBeEvaluted(MS: string): boolean;
function GetDataFromSampleServer(const IDs: string; const time: TDateTime): string;
procedure GetDataFromSampleServer(IDList: TGeneralInteger64ObjectMap;
  const time: TDateTime);
function IsAm(V: double): boolean;//is available measurement

const
  NAM = 1e30;// Not Avalaible measurment;
  EmptyMString = 'Null';

implementation

{ TMultiBusmeasurmen }

constructor TMultiBusmeasurmen.Create(tn: string);
begin
  inherited;
  V1 := NAM;
  V2 := NAM;
end;

function GetDataFromSampleServer(const IDs: string; const time: TDateTime): string;
var
  Request: string;
  Data, IP, Stime, Etime: string;
  Rdata: TStringList;
  CT: TDateTime;
begin
  Result := '';
  if IDs.Length < 1 then exit;
  Stime := FloatToStr(time);
  CT := IncMinute(time, 5);
  //CT:=EncodeDate(2018, 6, 7) + EncodeTime(1, 5, 0, 0);
  Etime := FloatToStr(CT);
  Rdata := TStringList.Create;
  IP := '127.0.0.1';
  Request := 'http://' + IP + ':8086?module=TFPWebModule1';
  Rdata.Add('module=TFPWebModule1');
  Rdata.Add('RequestType=IDsc');
  Rdata.Add('SampleInterval=5');
  Rdata.Add('EndTime=' + Etime);
  Rdata.Add('StartTime=' + Stime);
  Rdata.Add('IDsList=' + IDs);
  //Request := Request + IDs;
  try
    Data := TFPHTTPClient.SimpleFormPost(Request, Rdata);
  except
    on e: Exception do
      Exit(FloatToStr(NAM));
  end;
  Result := Data;
end;

procedure GetDataFromSampleServer(IDList: TGeneralInteger64ObjectMap;
  const time: TDateTime);
var
  IDString, Sep, R: string;
  k, index: integer;
  Rs: TStringArray;
  ID: int64;
  V: extended;
begin
  IDString := '';
  Sep := '';
  for k := 0 to IDList.Count - 1 do
  begin
    IDString := IDString + sep + (IDList.Data[k] as TSample).ID;
    sep := 'g';
  end;
  R := GetDataFromSampleServer(IDString, time);
  if R.Length < 1 then Exit;
  Rs := R.Split(':', TStringSplitOptions.ExcludeEmpty);
  // if (Length(R) mod 3 <> 0) then
  //  Exit;
  k := 0;
  while K < Length(RS) do
  begin
    ID := StrToInt64(Rs[k]);//you have to use try
    V := StrToFloat(Rs[k + 1]);
    index := IDList.IndexOf(ID);
    if index > -1 then
      (IDList.Data[index] as TSample).V := V;
    Inc(k, 3);
  end;
end;

procedure TEPF.GfunctionFromServer(var R: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  ID, V: string;
  VS: TStringArray;
begin
  ID := Args[0].ResString;
  V := GetDataFromSampleServer(ID, TimeString);
  VS := V.Split(':', TStringSplitOptions.ExcludeEmpty);
  if Length(VS) > 2 then
    R.ResFloat := StrToFloat(V.Split(':', TStringSplitOptions.ExcludeEmpty)[1])
  else
    R.ResFloat := NAM;
  //R.ResFloat:=-999;
end;

procedure TEPF.GfunctionFromIDList(var R: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  ID: string;
  IntID: int64;
  Index: integer;
begin
  ID := Args[0].ResString;
  IntID := StrToInt64(ID);
  Index := IDList.IndexOf(IntID);
  if Index > -1 then
    R.ResFloat := (IDList.Data[Index] as TSample).V
  else
    R.ResFloat := NAM;
end;

procedure TEPF.Dummyfunction(var R: TFPExpressionResult;
  const Args: TExprParameterArray);
begin
  R.ResFloat := 1;
end;

procedure TEPF.ExtractIDsfunction(var R: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  Sample: TSample;
begin
  Sample := TSample.Create;
  Sample.ID := Args[0].ResString;
  Sample.V := NAM;
  IDList.Add(StrToInt64(Sample.ID), Sample);
end;

function EvaluteMeasurementstring(MS: string; out Evalue: double;
  T: TDateTime; EV: TFPExpressionParser; EPF: TEPF): boolean;
var
  RT: TResultType;
begin
  Result := False;
  if MS = EmptyMString then
    Exit(False);
  EPF.TimeString := T;
  try
    EV.Expression := MS;
    Evalue := ArgToFloat(EV.Evaluate);
    RT := EV.ResultType;
    Result := True;
  except

  end;
end;

function ExtractMeasurementServerIDs(MS: string): boolean;
var
  EV: TFPExpressionParser;
  EPF: TEPF;
  Evalue: TExprFloat;
begin
  Result := False;
  if MS = EmptyMString then
    Exit(False);
  EV := TFPExpressionParser.Create(nil);
  EPF := TEPF.Create;
  EPF.TimeString := Now;
  try
    EV.Identifiers.AddFunction('G', 'F', 'S', @EPF.ExtractIDsfunction);
    EV.Expression := MS;
    Evalue := EV.Evaluate.ResFloat;
    Result := True;
  finally
    begin
      Ev.Free;
      EPF.Free;
    end;
  end;
end;

function CanMeasurementBeEvaluted(MS: string): boolean;
var
  EV: TFPExpressionParser;
  EPF: TEPF;
  Evalue: TExprFloat;
begin
  Result := False;
  if MS = EmptyMString then
    Exit(False);
  EV := TFPExpressionParser.Create(nil);
  EPF := TEPF.Create;
  EPF.TimeString := Now;
  try
    EV.Identifiers.AddFunction('G', 'F', 'S', @EPF.Dummyfunction);
    EV.Expression := MS;
    Evalue := EV.Evaluate.ResFloat;
    Result := True;
  finally
    begin
      Ev.Free;
      EPF.Free;
    end;
  end;
end;

procedure SetMeasermentFromDataString(DataString: string; M: TMeasurmen;
  T: TDateTime; EV: TFPExpressionParser; EPF: TEPF);
var
  D1, D2: TStringArray;
  n, k: integer;
  Evalue: double;
  EvaluationResult: boolean;
begin
  D1 := StringSplitPatchUnit.StringSplitPatch(DataString, [';'],
    TStringSplitOptions.ExcludeEmpty);
  for n := 0 to Length(D1) - 1 do
  begin
    D2 := StringSplitPatchUnit.StringSplitPatch(D1[n], [':'],
      TStringSplitOptions.ExcludeEmpty);
    EvaluationResult := EvaluteMeasurementstring(D2[1], Evalue, T, EV, EPF);
    if not EvaluationResult then
      EvaluationResult := EvaluteMeasurementstring(D2[2], Evalue, T, EV, EPF);
    if (EvaluationResult) and (IsPublishedProp(M, D2[0])) then
      SetFloatProp(M, D2[0], Evalue);
    SetLength(D2, 0);
  end;
end;

function IsAm(V: double): boolean;
begin
  Result := V < NAM;
end;

procedure UpdateServerdataStringValue(var D: string; P, V: string;
  const IsFirstValue: boolean);
var
  EndPos: integer;
  StartPos, SimiColonPos, DoubleColonPos, Pposition: SizeInt;
begin
  Pposition := Pos(P, D);
  if Pposition < 1 then
    raise Exception.Create('');
  StartPos := PosEx(':', D, Pposition);
  if not IsFirstValue then
    StartPos := PosEx(':', D, StartPos + 1);
  SimiColonPos := PosEx(';', D, StartPos + 1);
  DoubleColonPos := PosEx(':', D, StartPos + 1);
  if DoubleColonPos < 1 then
    EndPos := SimiColonPos
  else if Pposition < -1 then
    EndPos := DoubleColonPos
  else
    EndPos := Min(SimiColonPos, DoubleColonPos);
  Delete(D, StartPos + 1, EndPos - StartPos - 1);
  if V.IsEmpty then
    V := EmptyMString;
  Insert(V, D, StartPos + 1);
end;

function UpdateServerdataStringFirstValue(D: string; const P, V: string): string;
  //data string,Property name,value
begin
  UpdateServerdataStringValue(D, P, V, True);
  Result := D;
end;

function UpdateServerdataStringSecondValue(D: string; const P, V: string): string;
  //data string,Property name,value
begin
  UpdateServerdataStringValue(D, P, V, False);
  Result := D;
end;

{ Tpimeasurmen }

constructor TCustemPimeasurmen.Create(tn: string);
begin
  inherited;
  I1 := NAM;
  I2 := NAM;
  p1 := NAM;
  q1 := NAM;
  p2 := NAM;
  q2 := NAM;
  V1 := NAM;
  V2 := NAM;
end;

{ TMeasurmen }

constructor TMeasurmen.Create(tn: string);
begin
  TargetName := TN;
end;

{ TTrans3wMeasurmen }

constructor TTrans3wMeasurmen.Create(tn: string);
begin
  inherited Create(tn);
  I3 := NAM;
  p3 := NAM;
  q3 := NAM;
  V3 := NAM;
end;

function TTrans3wMeasurmen.Set1(Pi: Tpimeasurmen): Tpimeasurmen;
begin
  Pi.p1 := Self.p1;
  Pi.q1 := Self.q1;
  Pi.p2 := NAM;
  pi.q2 := NAM;
  Result := Pi;
end;

function TTrans3wMeasurmen.Set2(Pi: Tpimeasurmen): Tpimeasurmen;
begin
  Pi.p1 := Self.p2;
  Pi.q1 := Self.q2;
  Pi.p2 := NAM;
  pi.q2 := NAM;
  Result := Pi;
end;

function TTrans3wMeasurmen.Set3(Pi: Tpimeasurmen): Tpimeasurmen;
begin
  Pi.p1 := Self.p3;
  Pi.q1 := Self.q3;
  Pi.p2 := NAM;
  pi.q2 := NAM;
  Result := Pi;
end;

function TTrans3wMeasurmen.Set12(Pi: Tpimeasurmen): Tpimeasurmen;
begin
  Pi.p1 := Self.p1;
  Pi.q1 := Self.q1;
  Pi.p2 := Self.p2;
  pi.q2 := Self.q2;
  Result := Pi;
end;

function TTrans3wMeasurmen.Set13(Pi: Tpimeasurmen): Tpimeasurmen;
begin
  Pi.p1 := Self.p1;
  Pi.q1 := Self.q1;
  Pi.p2 := Self.p3;
  pi.q2 := Self.q3;
  Result := Pi;
end;

{ TTrans2wMeasurmen }

constructor TTrans2wMeasurmen.Create(tn: string);
begin
  inherited;
  tap := NAM;
end;

{ TBusmeasurmen }

constructor TBusmeasurmen.Create(tn: string);
begin
  inherited;
  Self.V := NAM;
end;

end.
