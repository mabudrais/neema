unit DMathSolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoadFlowSolverUnit, contnrs,   elementUnit, busunit,
  GenircDataUnit, UComplex, ModifyedSleUnit, WritlnUnit;

type

  { TDMathSolver }

  TDMathSolver = class(TLoadFlowSolver)
  protected
    NumOfBus, NumOfSlackBus, NumOfPQ: integer;
    y, th, v, del, d: PDouble;
    j, PQnr: PDouble;
    Index: PInteger;
    function IniPQIndex: integer;
    function CalcNumOfSlack: integer;
    function SS(cindex, num_of_bus: integer; IsSin, isV: boolean): extended;
    function UpdateJ(X: PVector): integer;
    function UpdatePQ(X: PVector): integer;
    procedure UpdateJ();
    procedure UpdatePQ;
  public
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList):Boolean;
      override;
    function Step: boolean; override;
     function GetSBus(busIndex: integer): complex; override;
    function GetVBus(busIndex: integer): complex; override;
    procedure SetVBus(busIndex: integer; Volt: complex); override;
     procedure UpdateVoltage;override;
  end;

implementation

var
  cinst: TDMathSolver;
  Nvar: integer;

function TDMathSolver.CalcNumOfSlack: integer;
var
  k: integer;
begin
  Result := 0;
  for k := 0 to NumOfBus - 1 do
  begin
    if bustype[k] = slackbus then
      Inc(Result);
  end;
end;

function TDMathSolver.IniPQIndex: integer;
var
  k: integer;
begin
  Result := 0;
  for k := 0 to NumOfBus - 1 do
  begin
    if bustype[k] <> slackbus then
    begin
      Index[Result] := k;
      Inc(Result);
    end;
  end;
end;

function TDMathSolver.Ini(Drawing: TObject; LoadFlowBuss,
  VariableElements: TObjectList): Boolean;
var
  k, m: integer;
  cy: complex;
begin
  cinst := Self;
  inherited ini(Drawing, LoadFlowBuss, VariableElements);
  NumOfBus := Length(VBus);
  NumOfSlackBus := CalcNumOfSlack();
  NumOfPQ := NumOfBus - NumOfSlackBus;
  getmem(Index, NumOfPQ * SizeOf(integer));
  NumOfPQ := IniPQIndex();
  NumOfSlackBus := 1;//should be calculated
  NumOfPQ := NumOfBus - NumOfSlackBus; //even pv is calculated as PQnr
  getmem(J, NumOfPQ * NumOfPQ * 4 * SizeOf(extended));
  getmem(y, NumOfBus * NumOfBus * SizeOf(extended));
  getmem(th, NumOfBus * NumOfBus * SizeOf(extended));
  getmem(V, NumOfBus * SizeOf(extended));
  getmem(del, NumOfBus * SizeOf(extended));
  Getmem(d, NumOfPQ * 2 * SizeOf(extended));
  getmem(PQnr, NumOfPQ * 2 * SizeOf(extended));
  for k := 0 to NumOfBus - 1 do
  begin
    for m := 0 to NumOfBus - 1 do
    begin
      if m = k then
        cy := ybus[k, m]
      else
        cy := ybus[k, m] * -1;
      y[k * NumOfBus + m] := cmod(cy);
      th[k * NumOfBus + m] := carg(cy);
    end;
  end;
  for k := 0 to NumOfBus - 1 do
  begin
    V[k] := cmod(vbus[k]);
    del[k] := carg(vbus[k]);
  end;
  for k := 0 to NumOfPQ * NumOfPQ * 4 - 1 do
    j[k] := 0;
end;

function TDMathSolver.SS(cindex, num_of_bus: integer; IsSin, isV: boolean): extended;
var
  k: integer;
  cy, ang, r: extended;
  mul: ValReal;
begin
  Result := 0;
  for k := 0 to num_of_bus - 1 do
  begin
    if k = cindex then
      Continue;
    cy := y[cindex * NumOfBus + k];
    if abs(cy) < 1e-6 then
      Continue;
    ang := th[cindex * NumOfBus + k] - del[cindex] + del[k];
    if IsSin then
      mul := sin(ang)
    else
      mul := cos(ang);
    r := V[k] * cy * mul;
    if isV then
      r := V[cindex] * r;
    Result := Result + r;
  end;
end;

procedure Jacobian(X: PVector; D: PMatrix);
var
  DNumOfPQ, m, k: integer;
begin
  DNumOfPQ := cinst.NumOfPQ * 2;
  cinst.UpdateJ(X);
  for k := 0 to DNumOfPQ - 1 do
  begin
    for m := 0 to DNumOfPQ - 1 do
    begin
      D^[k + 1]^[m + 1] := cinst.j[k * DNumOfPQ + m];
    end;
  end;
end;

procedure Equations(X, F: PVector);
var
  DNumOfBus, k: integer;
begin
  DNumOfBus := cinst.UpdatePQ(X);
  //WL('eq');
  for k := 0 to DNumOfBus - 1 do
  begin
    F^[k + 1] := cinst.PQnr[k];
    // WL(PQnr[k].ToString());
  end;
  //  WL('eqend');
end;

procedure Jacobian2(X: PVector; D: PMatrix);
const
  EtaMin = 1E-6;  { Relative increment used to compute derivatives }
var
  I, J: integer;
  R, Temp: Float;
  Eta: Float;
  Delta: PVector;  { Increment }
  Xminus: PVector;  { X - Delta }
  Xplus: PVector;  { X + Delta }
  Fminus: PVector;  { F(X - Delta) }
  Fplus: PVector;  { F(X + Delta) }
begin

  DimVector(Delta, Nvar);
  DimVector(Xminus, Nvar);
  DimVector(Xplus, Nvar);
  DimVector(Fminus, Nvar);
  DimVector(Fplus, Nvar);
  Eta := Sqrt(MachEp);

  if Eta < EtaMin then
    Eta := EtaMin;
  for I := 1 to Nvar do
  begin
    if X^[I] <> 0 then
      Delta^[I] := Eta * Abs(X^[I])
    else
      Delta^[I] := Eta;
    Xplus^[I] := X^[I] + Delta^[I];
    Xminus^[I] := X^[I] - Delta^[I];
  end;
  for J := 1 to Nvar do
  begin
    Temp := X^[J];
    X^[J] := Xminus^[J];
    Equations(X, Fminus);
    X^[J] := Xplus^[J];
    Equations(X, Fplus);
    R := 1.0 / (2.0 * Delta^[J]);
    for I := 1 to Nvar do
      D^[I]^[J] := R * (Fplus^[I] - Fminus^[I]);
    X^[J] := Temp;
  end;
  DelVector(Delta, Nvar);
  DelVector(Xminus, Nvar);
  DelVector(Xplus, Nvar);
  DelVector(Fminus, Nvar);
  DelVector(Fplus, Nvar);
end;

function TDMathSolver.UpdateJ(X: PVector): integer;
var
  k, ci1, DNumOfPQ: integer;
begin
  DNumOfPQ := 2 * NumOfPQ;//Double number of PQnr
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    V[ci1] := X^[k + NumOfPQ + 1];
    del[ci1] := X^ [k + 1];
  end;
  Result := DNumOfPQ;
  UpdateJ();
end;

procedure TDMathSolver.UpdatePQ;
var
  k, ci1: integer;
begin
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    PQnr[k] := PQ[ci1].re - (SS(ci1, NumOfBus, False, True) + v[ci1] *
      V[ci1] * y[ci1 * NumOfBus + ci1] * cos(th[ci1 * NumOfBus + ci1]));
    PQnr[k + NumOfPQ] := PQ[ci1].im + (SS(ci1, NumOfBus, True, True) +
      v[ci1] * V[ci1] * y[ci1 * NumOfBus + ci1] * sin(th[ci1 * NumOfBus + ci1]));
  end;
end;

function TDMathSolver.UpdatePQ(X: PVector): integer;
var
  k, ci1: integer;
begin
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    V[ci1] := X^[k + NumOfPQ + 1];
    del[ci1] := X^ [k + 1];
  end;
  UpdatePQ();
  Result := NumOfPQ * 2;
end;

function TDMathSolver.Step: boolean;
var
  term, k, m, ci1, ci2, shift, DNumOfPQ, MaxIter, EEE: integer;
  ca, MaxDelta: double;
  F, X: PVector;
  Tol, DBN: Float;
begin
  Result := True;
  DNumOfPQ := 2 * NumOfPQ;//Double number of PQnr
  DimVector(X, DNumOfPQ);
  DimVector(F, DNumOfPQ);
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    X^[k + NumOfPQ + 1] := V[ci1];
    X^[k + 1] := del[ci1];
  end;
  MaxIter := 100;
  Tol := 0.00001;
  DBN := X^[1];
  Nvar := 2 * NumOfPQ;
  NewtEqs(@Equations, @Jacobian, X, F, 1, DNumOfPQ, MaxIter, Tol);
  DBN := X^[1];
  EEE := MathErr;
  //if  EEE= OptNonConv then
  if EEE <> OptOk then
    raise Exception.Create('Dmath faile to solve');
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    V[ci1] := X^[k + NumOfPQ + 1];
    del[ci1] := X^ [k + 1];
  end;
  for k := 0 to NumOfBus - 1 do
  begin
    vbus[k].re := v[k] * cos(del[k]);
    vbus[k].im := v[k] * sin(del[k]);
  end;
end;

procedure TDMathSolver.UpdateJ();
var
  shift: integer;
  ci2: integer;
  m, DNumOfPQ, k, ci1: integer;
begin
  DNumOfPQ := 2 * NumOfPQ;
  //J1
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := Index[m];
      if ci1 <> ci2 then
        j[k * DNumOfPQ + m] :=
          -1 * v[ci1] * v[ci2] * y[ci1 * NumOfBus + ci2] *
          sin(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[k * DNumOfPQ + m] := ss(ci1, NumOfBus, True, True);
    end;
  end;
  //J2
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := Index[m];
      if ci1 <> ci2 then
        j[k * DNumOfPQ + (m + NumOfPQ)] :=
          v[ci1] * y[ci1 * NumOfBus + ci2] * cos(th[ci1 * NumOfBus + ci2] -
          del[ci1] + del[ci2])
      else
        j[k * DNumOfPQ + (m + NumOfPQ)] :=
          2 * v[ci1] * y[ci1 * NumOfBus + ci1] * cos(th[ci1 * NumOfBus + ci1]) +
          ss(ci1, NumOfBus, False, False);
    end;
  end;
  shift := 2 * NumOfPQ * NumOfPQ;
  //J3
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := Index[m];
      if ci1 <> ci2 then
        j[shift + k * DNumOfPQ + m] :=
          -1 * v[ci1] * v[ci2] * y[ci1 * NumOfBus + ci2] *
          cos(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[shift + k * DNumOfPQ + m] := ss(ci1, NumOfBus, False, True);
    end;
  end;
  //J4
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := Index[m];
      if ci1 <> ci2 then
        j[shift + k * DNumOfPQ + (m + NumOfPQ)] :=
          -1 * v[ci1] * y[ci1 * NumOfBus + ci2] *
          sin(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[shift + k * DNumOfPQ + (m + NumOfPQ)] :=
          -2 * v[ci1] * y[ci1 * NumOfBus + ci1] * sin(th[ci1 * NumOfBus + ci1]) -
          ss(ci1, NumOfBus, True, False);
    end;
  end;
end;
 function TDMathSolver.GetSBus(busIndex: integer): complex;
begin
  Result := cinit((SS(busIndex, NumOfBus, False, True) + v[busIndex] *
    V[busIndex] * y[busIndex * NumOfBus + busIndex] *
    cos(th[busIndex * NumOfBus + busIndex])),
    -(SS(busIndex, NumOfBus, True, True) + v[busIndex] * V[busIndex] *
    y[busIndex * NumOfBus + busIndex] * sin(th[busIndex * NumOfBus + busIndex])));
end;

function TDMathSolver.GetVBus(busIndex: integer): complex;
begin
  Result.re := v[busIndex] * cos(del[busIndex]);
  Result.im := v[busIndex] * sin(del[busIndex]);
end;

procedure TDMathSolver.SetVBus(busIndex: integer; Volt: complex);
begin
  V[busIndex] := cmod(Volt);
  del[busIndex] := carg(Volt);
end;
 procedure TDMathSolver.UpdateVoltage;
var
  k: integer;
begin
  begin //update voltage
    for k := 0 to NumOfBus - 1 do
    begin
      vbus[k].re := v[k] * cos(del[k]);
      vbus[k].im := v[k] * sin(del[k]);
    end;
  end;
end;
end.
