unit CartizianNrsolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoadFlowSolverUnit, contnrs, elementUnit, busunit,
  GenircDataUnit, UComplex, ModifyedSleUnit;

type

  { TCNRSolver }

  TCNRSolver = class(TLoadFlowSolver)
  private
    NumOfBus, NumOfSlackBus, NumOfPQ: integer;
    J, yR, YI, VR, VI, PQnr, d:  array of Double;
    Index: PInteger;
    function CalcSSR(ci1: integer): complex;
    function IniPQIndex: integer;
    function CalcNumOfSlack: integer;
    function RIRI(cindex, num_of_bus: integer; isQ: boolean): extended;
    function RRII(cindex, num_of_bus: integer; isQ: boolean): extended;
  public
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList):Boolean;
      override;
    function Step: boolean; override;
    function GetSBus(busIndex: integer): complex; override;
    function GetVBus(busIndex: integer): complex; override;
    procedure SetVBus(busIndex: integer; Volt: complex); override;
    procedure UpdateVoltage; override;
  end;

implementation

function TCNRSolver.CalcNumOfSlack: integer;
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

function TCNRSolver.IniPQIndex: integer;
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

function TCNRSolver.CalcSSR( ci1: integer): complex;
var
  SSr,CY,cV: complex;
  m: Integer;
begin
  SSr:=0;
      for m:=0 to  NumOfBus -1 do
      begin
         if m=ci1 then Continue;
         cY:=YR[ci1 * NumOfBus + m]+YI[ci1 * NumOfBus + m]*i;
         cV:=(VR[m]+VI[m]*i);
         SSr:=ssr+cV*-1*cY;
      end;
  Result:=SSr;
end;

function TCNRSolver.Ini(Drawing: TObject; LoadFlowBuss,
  VariableElements: TObjectList): Boolean;
var
  k, m: integer;
  cy: complex;
begin
  if (inherited Ini(Drawing, LoadFlowBuss, VariableElements)) then
    Exit(False);
  NumOfBus := Length(VBus);
  NumOfSlackBus := CalcNumOfSlack();
  NumOfPQ := NumOfBus - NumOfSlackBus;
  getmem(Index, NumOfPQ * SizeOf(integer));
  NumOfPQ := IniPQIndex();
  NumOfSlackBus := 1;//should be calculated
  NumOfPQ := NumOfBus - NumOfSlackBus; //even pv is calculated as PQnr
  SetLength(J, NumOfPQ * NumOfPQ * 4 * SizeOf(extended));
  SetLength(yR, NumOfBus * NumOfBus * SizeOf(extended));
  SetLength(YI, NumOfBus * NumOfBus * SizeOf(extended));
  SetLength(VR, NumOfBus * SizeOf(extended));
  SetLength(VI, NumOfBus * SizeOf(extended));
  SetLength(d, NumOfPQ * 2 * SizeOf(extended));
  SetLength(PQnr, NumOfPQ * 2 * SizeOf(extended));
  for k := 0 to NumOfBus - 1 do
  begin
    for m := 0 to NumOfBus - 1 do
    begin
      if m = k then
        cy := ybus[k, m]
      else
        cy := ybus[k, m] * -1;
      yI[k * NumOfBus + m] := cy.im;
      YR[k * NumOfBus + m] := cy.re;
    end;
  end;
  for k := 0 to NumOfBus - 1 do
  begin
    VI[k] := (vbus[k].im);
    VR[k] := (vbus[k].re);
  end;
  for k := 0 to NumOfPQ * NumOfPQ * 4 - 1 do
    j[k] := 0;
  Result:=True;
end;

function TCNRSolver.RRII(cindex, num_of_bus: integer; isQ: boolean): extended;
var
  k: integer;
  cy, ang, r: extended;
  mul, cyR, cyI: double;
begin
  Result := 0;
  for k := 0 to num_of_bus - 1 do
  begin
    if k = cindex then
      Continue;
    cyR := -yR[cindex * NumOfBus + k];
    cyI := -yI[cindex * NumOfBus + k];
   { mul := -1;
    if isQ then
      mul := 1;}
    r :=  VI[k] * cyI  -VR[k] * cyR  ;
    Result := Result + r;
  end;
end;

function TCNRSolver.RIRI(cindex, num_of_bus: integer; isQ: boolean): extended;
var
  k: integer;
  cy, ang, r: extended;
  mul, cyR, cyI: double;
begin
  Result := 0;
  for k := 0 to num_of_bus - 1 do
  begin
    if k = cindex then
      Continue;
    cyR := -yR[cindex * NumOfBus + k];
    cyI := -yI[cindex * NumOfBus + k];
    mul := -1;
    if isQ then
      mul := 1;
    r := VR[k] * cyI * mul + VI[k] * cyR * mul;
    Result := Result + r;
  end;
end;
function TCNRSolver.Step: boolean;
var
  term, k, m, ci1, ci2, shift, DNumOfPQ: integer;
  ca, MaxDelta: double;
  SSr,Vci1, Vci1g, Yci11,NewS: complex;
begin
  Result := False;
  for k := 0 to NumOfPQ * NumOfPQ * 4 - 1 do
    j[k] := 0;
  DNumOfPQ := 2 * NumOfPQ;//Double number of PQnr
  //J1
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    for m := 0 to NumOfPQ - 1 do
    begin
      ci2 := Index[m];
      if ci1 <> ci2 then
        j[k * DNumOfPQ + m] :=
            VR[ci1] * YR[ci1 * NumOfBus + ci2] + VI[ci1] * YI[ci1 * NumOfBus + ci2]
      else
        j[k * DNumOfPQ + m] :=2*VR[ci1]*YR[ci1 * NumOfBus + ci1]+RRII(ci1,NumOfBus,False);
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
          -vR[ci1] * yI[ci1 * NumOfBus + ci2] + vI[ci1] * yR[ci1 * NumOfBus + ci2]
      else
        j[k * DNumOfPQ + (m + NumOfPQ)] :=
          2*VI[ci1]*YR[ci1 * NumOfBus + ci1]+RIRI(ci1,NumOfBus,False);
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
          vI[ci1] * yR[ci1 * NumOfBus + ci2] -vR[ci1] * yI[ci1 * NumOfBus + ci2]
      else
        j[shift + k * DNumOfPQ + m] := RIRI(ci1,NumOfBus,True)-2*VR[ci1]*YI[ci1 * NumOfBus + ci1];
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
          vR[ci1] * yR[ci1 * NumOfBus + ci2] + vI[ci1] * yI[ci1 * NumOfBus + ci2]
      else
        j[shift + k * DNumOfPQ + (m + NumOfPQ)] :=
            RRII(ci1,NumOfBus,True)-2*VI[ci1]*YI[ci1 * NumOfBus + ci1];
    end;
  end;
  //WriteLn('new newton');
  // for k := 0 to DNumOfPQ * DNumOfPQ do
  //WriteLn(FormatFloat('#.##', j[k]));
  //PQnr
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    NewS:=GetSBus(ci1);
    PQnr[k] := PQ[ci1].re - NewS.re;
    PQnr[k + NumOfPQ] := PQ[ci1].im + NewS.im;
  end;
  //solving
  slegen(DNumOfPQ, DNumOfPQ, j[0], PQnr[0], d[0], ca, term);
  // raise Exception.Create('solve the upove problem');
  if term <> 1 then
    Exit(True);
  WriteLn('PQnr');
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    //WriteLn(k, ' ', ' ', PQnr[k]);
  end;
  //update value
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    WriteLn('d ', d[k + NumOfPQ], ' ', d[k]);
    VI[ci1] := vI[ci1] + d[k + NumOfPQ];
    VR[ci1] := VR[ci1] + d[k] ;
  end;
  MaxDelta := 0;
  for k := 0 to DNumOfPQ - 1 do
  begin
    if MaxDelta < abs(d[k]) then
      MaxDelta := abs(d[k]);
  end;
   WriteLn('MaxDelta ',MaxDelta);
  //Sleep(1000);
  if MaxDelta < 0.00001 then
    Result := True;

end;

function TCNRSolver.GetSBus(busIndex: integer): complex;
var
  ci1: Integer;
  SSr,  Vci1, Vci1g, Yci11: complex;
begin
  Result:=0;
  ci1:=busIndex;
  SSr:=CalcSSR(ci1);
    Vci1:=VR[ci1]+VI[ci1]*i;
        Vci1g:=(VR[ci1]-VI[ci1]*i);
    Yci11:=YR[ci1 * NumOfBus + ci1]+YI[ci1 * NumOfBus + ci1]*i;
    Result:=Vci1g*(Vci1*Yci11-SSr);         {
  Result := cinit((SS(busIndex, NumOfBus, False, True) + v[busIndex] *
    V[busIndex] * y[busIndex * NumOfBus + busIndex] *
    cos(th[busIndex * NumOfBus + busIndex])),
    -(SS(busIndex, NumOfBus, True, True) + v[busIndex] * V[busIndex] *
    y[busIndex * NumOfBus + busIndex] * sin(th[busIndex * NumOfBus + busIndex])));}
end;

function TCNRSolver.GetVBus(busIndex: integer): complex;
begin
  Result.re := vR[busIndex] ;
  Result.im := vI[busIndex] ;
end;

procedure TCNRSolver.SetVBus(busIndex: integer; Volt: complex);
begin
  VR[busIndex] :=Volt.re;
  VI[busIndex] := Volt.im;
end;

procedure TCNRSolver.UpdateVoltage;
var
  k: integer;
begin
  begin //update voltage
    for k := 0 to NumOfBus - 1 do
    begin
      vbus[k].re := vR[k] ;
      vbus[k].im := vI[k] ;
    end;
  end;
end;

end.
