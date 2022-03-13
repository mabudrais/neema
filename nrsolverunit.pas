unit NrsolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoadFlowSolverUnit, contnrs, elementUnit, busunit,
  GenircDataUnit, UComplex, ModifyedSleUnit;

type

  { TNRSolver3 }

  TNRSolver3 = class(TLoadFlowSolver)
  private
    NumOfBus, NumOfSlackBus, NumOfPQ, NumOfPV: integer;
    J, y, th, v, del, PQnr, d: PDouble;
    PQIndex, PQPVIndex: array of integer;
    procedure IniPQPVIndex(aTreatPVasPQ: boolean);
    procedure CalcNumOfSlackPQPV(out SlackBusNum, PQBusNum, PVBusNum: integer);
    function SS(cindex, num_of_bus: integer; IsSin, isV: boolean): extended;
  public
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList): boolean;
      override;
    procedure DeIni(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList);
      override;
    function Step: boolean; override;
    function GetSBus(busIndex: integer): complex; override;
    function GetVBus(busIndex: integer): complex; override;
    procedure SetVBus(busIndex: integer; Volt: complex); override;
    procedure UpdateVoltage; override;
  end;

implementation

procedure TNRSolver3.CalcNumOfSlackPQPV(out SlackBusNum, PQBusNum, PVBusNum: integer);
var
  k: integer;
begin
  SlackBusNum := 0;
  PQBusNum := 0;
  PVBusNum := 0;
  for k := 0 to NumOfBus - 1 do
  begin
    if bustype[k] = slackbus then
      Inc(SlackBusNum)
    else if bustype[k] = regulatingbus then
      Inc(PVBusNum)
    else
    if bustype[k] = loadbus then
      Inc(PQBusNum)
    else
      raise Exception.Create('unknown bus type');
  end;
end;

procedure TNRSolver3.IniPQPVIndex(aTreatPVasPQ: boolean);
var
  k, m, m2: integer;
begin
  m := 0;
  m2 := 0;
  for k := 0 to NumOfBus - 1 do
  begin
    if (bustype[k] = loadbus) or ((bustype[k] = regulatingbus) and (aTreatPVasPQ)) then
    begin
      PQIndex[m] := k;
      PQPVIndex[m2] := k;
      Inc(m2);
      Inc(m);
    end
    else if bustype[k] = regulatingbus then
    begin
      PQPVIndex[m2] := k;
      Inc(m2);
    end;
  end;
end;

function TNRSolver3.Ini(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList): boolean;
var
  k, m: integer;
  cy: complex;
begin
  Result := inherited ini(Drawing, LoadFlowBuss, VariableElements);
  if not Result then
    Exit;
  NumOfBus := Length(VBus);
  CalcNumOfSlackPQPV(NumOfSlackBus, NumOfPQ, NumOfPV);
  SetLength(PQIndex, NumOfPQ);
  SetLength(PQPVIndex, NumOfPQ + NumOfPV);
  IniPQPVIndex(TreatPVasPQ);
  if TreatPVasPQ then
  begin
    getmem(J, (NumOfPQ + NumOfPV) * (NumOfPQ + NumOfPV) * 4 * SizeOf(extended));
    Getmem(d, (NumOfPQ + NumOfPV) * 2 * SizeOf(extended));
    getmem(PQnr, (NumOfPQ + NumOfPV) * 2 * SizeOf(extended));
  end
  else
  begin
    getmem(J, (NumOfPQ * 2 + NumOfPV) * (NumOfPQ * 2 + NumOfPV) * SizeOf(extended));
    Getmem(d, (NumOfPQ * 2 + NumOfPV) * SizeOf(extended));
    getmem(PQnr, (NumOfPQ * 2 + NumOfPV) * SizeOf(extended));
  end;
  getmem(y, NumOfBus * NumOfBus * SizeOf(extended));
  getmem(th, NumOfBus * NumOfBus * SizeOf(extended));
  getmem(V, NumOfBus * SizeOf(extended));
  getmem(del, NumOfBus * SizeOf(extended));
  for k := 0 to NumOfBus - 1 do
  begin
    for m := 0 to NumOfBus - 1 do
    begin
      if m = k then
        cy := ybus[k, m]
      else
        cy := ybus[k, m] * -1;
      {if cmod(ybus[k, m]) > 1e-3 then
        Ln(k, ' ', m, ' ', ybus[k, m].re, ' ', ybus[k, m].im);}
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

procedure TNRSolver3.DeIni(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList);
begin
  inherited DeIni(Drawing, LoadFlowBuss, VariableElements);
  Freemem(J);
  Freemem(d);
  Freemem(PQnr);
  Freemem(y);
  Freemem(th);
  Freemem(V);
  Freemem(del);
  SetLength(PQIndex, 0);
  SetLength(PQPVIndex, 0);
end;

function TNRSolver3.SS(cindex, num_of_bus: integer; IsSin, isV: boolean): extended;
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

function TNRSolver3.Step: boolean;
var
  term, k, m, ci1, ci2, shift, MatrixWidth, J1Width: integer;
  ca, MaxDelta: double;
begin
  Result := False;
  MatrixWidth := Length(PQPVIndex) + Length(PQIndex);//Double number of PQnr
  J1Width := Length(PQPVIndex);
  //J1
  for k := 0 to J1Width - 1 do
  begin
    ci1 := PQPVIndex[k];
    for m := 0 to J1Width - 1 do
    begin
      ci2 := PQPVIndex[m];
      if ci1 <> ci2 then
        j[k * MatrixWidth + m] :=
          -1 * v[ci1] * v[ci2] * y[ci1 * NumOfBus + ci2] *
          sin(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[k * MatrixWidth + m] := ss(ci1, NumOfBus, True, True);
    end;
  end;
  //J2
  for k := 0 to NumOfPQ + NumOfPV - 1 do
  begin
    ci1 := PQPVIndex[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := PQIndex[m];
      if ci1 <> ci2 then
        j[k * MatrixWidth + (m + J1Width)] :=
          v[ci1] * y[ci1 * NumOfBus + ci2] * cos(th[ci1 * NumOfBus + ci2] -
          del[ci1] + del[ci2])
      else
        j[k * MatrixWidth + (m + J1Width)] :=
          2 * v[ci1] * y[ci1 * NumOfBus + ci1] * cos(th[ci1 * NumOfBus + ci1]) +
          ss(ci1, NumOfBus, False, False);
    end;
  end;
  shift := J1Width * J1Width + J1Width * NumOfPQ;
  //J3
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := PQIndex[k];
    for m := 0 to NumOfPQ + NumOfPV - 1 do
    begin
      ci2 := PQPVIndex[m];
      if ci1 <> ci2 then
        j[shift + k * MatrixWidth + m] :=
          -1 * v[ci1] * v[ci2] * y[ci1 * NumOfBus + ci2] *
          cos(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[shift + k * MatrixWidth + m] := ss(ci1, NumOfBus, False, True);
    end;
  end;
  //J4
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := PQIndex[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := PQIndex[m];
      if ci1 <> ci2 then
        j[shift + k * MatrixWidth + (m + J1Width)] :=
          -1 * v[ci1] * y[ci1 * NumOfBus + ci2] *
          sin(th[ci1 * NumOfBus + ci2] - del[ci1] + del[ci2])
      else
        j[shift + k * MatrixWidth + (m + J1Width)] :=
          -2 * v[ci1] * y[ci1 * NumOfBus + ci1] * sin(th[ci1 * NumOfBus + ci1]) -
          ss(ci1, NumOfBus, True, False);
    end;
  end;
  //Ln('new newton');
  // for k := 0 to DNumOfPQ * DNumOfPQ do
  //Ln(FormatFloat('#.##', j[k]));
  //PQnr
  for k := 0 to J1Width - 1 do
  begin
    ci1 := PQPVIndex[k];
    PQnr[k] := PQ[ci1].re - (SS(ci1, NumOfBus, False, True) + v[ci1] *
      V[ci1] * y[ci1 * NumOfBus + ci1] * cos(th[ci1 * NumOfBus + ci1]));
  end;
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := PQIndex[k];
    PQnr[k + J1Width] := PQ[ci1].im + (SS(ci1, NumOfBus, True, True) +
      v[ci1] * V[ci1] * y[ci1 * NumOfBus + ci1] * sin(th[ci1 * NumOfBus + ci1]));
  end;
  {for k := 0 to (NumOfPQ + J1Width) - 1 do
  begin
    Ln('***');
    for m := 0 to (NumOfPQ + J1Width) - 1 do
      Ln(j[k * MatrixWidth + m]);
  end;}
  //solving
  slegen(J1Width + NumOfPQ, J1Width + NumOfPQ, j[0], PQnr[0], d[0], ca, term);
  // raise Exception.Create('solve the upove problem');
  if term <> 1 then
    Exit(True);
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := PQIndex[k];
    // Ln(k, ' ', ' ', PQnr[k]);
  end;
  //update value
  for k := 0 to J1Width - 1 do
  begin
    ci1 := PQPVIndex[k];
    del[ci1] := del[ci1] + d[k] * Accelerationfactor;
  end;
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := PQIndex[k];
    V[ci1] := v[ci1] + d[k + J1Width] * Accelerationfactor;
  end;
  MaxDelta := 0;
  for k := 0 to J1Width + NumOfPQ - 1 do
  begin
    if MaxDelta < abs(d[k]) then
      MaxDelta := abs(d[k]);
  end;
  // Ln(MaxDelta);
  //Sleep(1000);
  if MaxDelta < 0.00001 then
    Result := True;

end;

function TNRSolver3.GetSBus(busIndex: integer): complex;
begin
  Result := cinit((SS(busIndex, NumOfBus, False, True) + v[busIndex] *
    V[busIndex] * y[busIndex * NumOfBus + busIndex] *
    cos(th[busIndex * NumOfBus + busIndex])),
    -(SS(busIndex, NumOfBus, True, True) + v[busIndex] * V[busIndex] *
    y[busIndex * NumOfBus + busIndex] * sin(th[busIndex * NumOfBus + busIndex])));
end;

function TNRSolver3.GetVBus(busIndex: integer): complex;
begin
  Result.re := v[busIndex] * cos(del[busIndex]);
  Result.im := v[busIndex] * sin(del[busIndex]);
end;

procedure TNRSolver3.SetVBus(busIndex: integer; Volt: complex);
begin
  V[busIndex] := cmod(Volt);
  del[busIndex] := carg(Volt);
end;

procedure TNRSolver3.UpdateVoltage;
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
