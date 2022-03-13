unit MarquardtSolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DMathSolverUnit, contnrs, EEMartixUnit, Math;

type

  { TMarquardtSolver }
  //from gridcal doc
  TMarquardtSolver = class(TDMathSolver)
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList): boolean;
      override;
    function Step: boolean; override;
  private
    H, Ht, dz: TEEMartix;
    H2: TEEMartix;
    A: TEEMartix;
    RHS: TEEMartix;
    dzT: TEEMartix;
    dzdzT: TEEMartix;
    dx: TEEMartix;
    dxTldxPlusRHS, ldxPlusRHS: TEEMartix;
    dxT: TEEMartix;
    VMarquardt: integer;
    FirstStep: boolean;
    fPrev: extended;
    function UpdateVoltages: extended;
  public
    procedure UpdateVoltage; override;
  end;

implementation

{ TMarquardtSolver }

function TMarquardtSolver.Ini(Drawing: TObject;
  LoadFlowBuss, VariableElements: TObjectList): boolean;
begin
  if (inherited Ini(Drawing, LoadFlowBuss, VariableElements)) then
    Exit(False);
  H := TEEMartix.Create(2 * NumOfPQ, 2 * NumOfPQ);
  Ht := TEEMartix.Create(2 * NumOfPQ, 2 * NumOfPQ);
  H2 := TEEMartix.Create(2 * NumOfPQ, 2 * NumOfPQ);
  A := TEEMartix.Create(2 * NumOfPQ, 2 * NumOfPQ);
  dz := TEEMartix.Create(2 * NumOfPQ, 1);
  dzT := TEEMartix.Create(1, 2 * NumOfPQ);
  dzdzT := TEEMartix.Create(1, 1);
  RHS := TEEMartix.Create(2 * NumOfPQ, 1);
  dx := TEEMartix.Create(2 * NumOfPQ, 1);
  dxT := TEEMartix.Create(1, 2 * NumOfPQ);
  ldxPlusRHS := TEEMartix.Create(2 * NumOfPQ, 1);
  dxTldxPlusRHS := TEEMartix.Create(1, 1);
  FirstStep := True;
  Result := True;
end;

function TMarquardtSolver.Step: boolean;
var
  Lamp, f, roh, maxTol: extended;
  k: integer;
  ComputeH: boolean;
begin
  Result := False;
  if FirstStep then
  begin
    VMarquardt := 2;
    fPrev := 1e9;
    ComputeH := True;
  end;
  if ComputeH then
    UpdateJ();
  UpdatePQ();
  H.fill(J);
  dz.fill(PQnr);
  dz.multwith(-1);
  H.transpos(HT);
  HT.multwith(H, H2);
  if FirstStep then
    Lamp := 1e-3 * H2.MaxDaig;
  A.fill(H2);
  for k := 0 to A.row - 1 do
    A.cell[k, k] := A.cell[k, k] + Lamp;
  HT.multwith(dz, RHS);
 { A.inv();
  A.multwith(RHS, dx);}
  A.Solve(RHS, dx);
  dz.transpos(dzT);
  dzT.multwith(dz, dzdzT);
  f := 0.5 * dzdzT.cell[0, 0];
  ldxPlusRHS.fill(dx);
  ldxPlusRHS.multwith(Lamp);
  ldxPlusRHS.sum(RHS);
  dx.transpos(dxT);
  dxT.multwith(ldxPlusRHS, dxTldxPlusRHS);
  roh := (fPrev - f) / (0.5 * dxTldxPlusRHS.cell[0, 0]);
  if roh >= 0 then
  begin
    ComputeH := True;  //strange !!
    Lamp := Lamp * max(1 / 3, 1 - (2 * roh - 1) ** 3);
    VMarquardt := 2;
    maxTol := UpdateVoltages();
  end
  else
  begin
    ComputeH := False;
    Lamp := Lamp * VMarquardt;
    VMarquardt := VMarquardt * 2;
  end;
  if maxTol < 1e-6 then
  begin
    for k := 0 to NumOfBus - 1 do
    begin
      vbus[k].re := v[k] * cos(del[k]);
      vbus[k].im := v[k] * sin(del[k]);
    end;
    Result := True;
  end;
  fPrev := f;
  FirstStep := False;
end;

function TMarquardtSolver.UpdateVoltages: extended;
var
  ci1, k: integer;
  cddel: extended;
  cdv, maxTol: extended;
begin
  maxTol := 1e-9;
  for k := 0 to NumOfPQ - 1 do
  begin
    ci1 := Index[k];
    cdv := dx.cell[k + NumOfPQ, 0];
    V[ci1] := v[ci1] - cdv;
    cddel := dx.cell[k, 0];
    del[ci1] := del[ci1] - cddel;
    if abs(cdv) > maxTol then
      maxTol := abs(cdv);
    if abs(cddel) > maxTol then
      maxTol := abs(cddel);
  end;
  Result := maxTol;
end;


procedure TMarquardtSolver.UpdateVoltage;
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
