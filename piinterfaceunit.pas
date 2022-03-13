unit PiInterfaceUnit;

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
interface

uses
  Classes, SysUtils, ucomplex;

type

  { IPI }

  IPI = interface
    ['{8F662A4A-2B03-4470-A9E0-3A682005A667}']
    procedure GetImpPerUnit(out oz1, oz2, oz3: complex);
    procedure CBSatatus(out CB1, CB2: boolean);
    procedure BussIDs(out aID1, aID2: integer);
    function PISelf(): TObject;
    // procedure Get(V1PerUnit, V2PerUnit: complex; out S12, S21: complex);
  end;

procedure GetALLM(aPI: IPI; VBus1, VBus2: complex;
  out V1, V2, I1, I2, S12, S21: complex);

implementation

procedure GetALLM(aPI: IPI; VBus1, VBus2: complex;
  out V1, V2, I1, I2, S12, S21: complex);
var
  absV1, Del1, absV2, Del2, Z1, Lamda1, Z2, Lamda2, Z3, Lamda3: double;
  oz3, oz2, oz1, VALeg1, VALeg2, TempS12: complex;
  Q12, P12: double;
  P21, Q21: real;
  CB1, CB2: boolean;
begin
  aPI.CBSatatus(CB1, CB2);
  aPI.GetImpPerUnit(oz1, oz2, oz3);
  I1 := 0;
  I2 := 0;
  S12 := 0;
  S21 := 0;
  V1 := 0;
  V2 := 0;
  if (not CB1) and (not CB2) then Exit;
  V1 := VBus1;
  V2 := VBus2;
  if (not CB1) then
  begin
    if abs(cmod(oz1)) > 1e13 then
    begin
      V1 := VBus2;
      V2 := VBus2;
      Exit;
    end;
  end;
  if (not CB2) then
  begin
    if abs(cmod(oz3)) > 1e13 then
    begin
      V1 := VBus1;
      V2 := VBus1;
      Exit;
    end;
  end;
  absV1 := cmod(VBus1);
  Del1 := carg(VBus1);
  absV2 := cmod(VBus2);
  Del2 := carg(VBus2);
  Z1 := cmod(oz1);
  Lamda1 := carg(oz1);
  Z2 := cmod(oz2);
  Lamda2 := carg(oz2);
  Z3 := cmod(oz3);
  Lamda3 := carg(oz3);
  TempS12 := 1 / oz2;
  TempS12 := VBus1 * cong((VBus1 - VBus2) / oz2);
  P12 := absV1 * absV1 / Z2 * cos(Lamda2) - absV1 * absV2 / Z2 *
    cos(Lamda2 + Del1 - Del2);
  Q12 := absV1 * absV1 / Z2 * sin(Lamda2) - absV1 * absV2 / Z2 *
    sin(Lamda2 + Del1 - Del2);
  VALeg1 := VBus1 * cong(VBus1 / oz1);
  VALeg2 := VBus2 * cong(VBus2 / oz3);
  P12 := P12 + VALeg1.re;
  Q12 := Q12 + VALeg1.im;
  S12 := P12 + i * Q12;
  P21 := absV2 * absV2 / Z2 * cos(Lamda2) - absV2 * absV1 / Z2 *
    cos(Lamda2 + Del2 - Del1);
  Q21 := absV2 * absV2 / Z2 * sin(Lamda2) - absV2 * absV1 / Z2 *
    sin(Lamda2 + Del2 - Del1);
  P21 := P21 + VALeg2.re;
  Q21 := Q21 + VALeg2.im;
  S21 := P21 + i * Q21;
end;

end.
