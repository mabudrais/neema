unit JuliaUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoadFlowSolverUnit, UComplex, contnrs, BusUnit;

type

  { TJulia }

  TJulia = class(TLoadFlowSolver)
    function Ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList):Boolean;
      override;
  private
    function JC(C: complex): String;
  end;

implementation

procedure PrintJulia(aLFS: TObject);
var
  LFS: TLoadFlowSolver;
begin
  LFS := aLFS as TLoadFlowSolver;
end;

{ TJulia }
 function TJulia.JC(C:complex):String; //Julia complex
var
  R: String;
begin
  R:=UComplex.cstr(C);
  Result:=r.Replace('i','im');
end;
function TJulia.Ini(Drawing: TObject; LoadFlowBuss,
  VariableElements: TObjectList): Boolean;
var
  k, m, Mul: integer;
  Ks, VT: string;
begin
  inherited Ini(Drawing, LoadFlowBuss, VariableElements);
  WriteLn('NumOfBus=', Length(VBus));
  WriteLn('V = zeros(ComplexF64, NumOfBus)');
  WriteLn('PQ = zeros(ComplexF64, NumOfBus)');
  WriteLn('Y = zeros(ComplexF64, NumOfBus, NumOfBus)');
  WriteLn('BussName = Array(String, NumOfBus)');
  for k := 0 to Length(VBus) - 1 do
  begin
    Ks := IntToStr(k + 1);
    WriteLn('V[' + Ks + ']=' + JC(VBus[k]));
    WriteLn('PQ[' + Ks + ']=' + JC(PQ[k]));
    VT := 'VDbus';
    if BusType[k] = regulatingbus then
      VT := 'PVbus'
    else if BusType[k] = loadbus then
      VT := 'PQbus';
    WriteLn('VT[' + Ks + ']=' + VT);
    WriteLn('BussName[' + Ks + ']="' + (LoadFlowBuss[k] as TLoadFlowBus).Name+'"');
  end;
  for k := 0 to Length(VBus) - 1 do
  begin
    for m := 0 to Length(VBus) - 1 do
    begin
      if abs(cmod(YBus[k, m])) > 1e-6 then
        begin
        if m=k then
          Mul:=1
          else
            Mul:=-1;
            WriteLn('Y[' + (k + 1).ToString() + ',' + (m + 1).ToString() +
          ']=' + JC(YBus[k, m]*Mul));
        end;
    end;
  end;
end;

end.
