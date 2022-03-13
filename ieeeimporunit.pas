unit IeeeImporUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, UtilUnit, UComplex;

function ImportCase(FileName: string; NeemaObject: TObject): TObject;

implementation

uses DrawingUnit, BusUnit, PiImpedanceUnit, ShuntUnit, ConsolAppUnit;

function GI(C: string; var k: integer; out I: integer): boolean;//get integer
var
  OldK, start: integer;
  Ist: string;
begin
  OldK := k;
  Inc(k);
  while (C[k] = ' ') or (C[k] = #10) or (C[k] = #13) do
    Inc(k);
  if not ((C[k] in ['0'..'9']) or (c[k] = '-')) then
  begin
    k := OldK;
    Exit(False);
  end;
  start := k;
  while (c[k] in ['0'..'9']) or ((c[k] = '-') and (k = start)) do
    Inc(k);
  Dec(k);
  Ist := Copy(C, OldK + 1, k - OldK);
  I := StrToInt(Ist);
  Result := True;
end;

function GF(C: string; var k: integer; out I: double): boolean;//get integer
var
  OldK, start: integer;
  Ist: string;
begin
  OldK := k;
  Inc(k);
  while (C[k] = ' ') or (C[k] = #10) or (C[k] = #13) do
    Inc(k);
  if not ((C[k] in ['0'..'9']) or (c[k] = '-')) then
  begin
    k := OldK;
    Exit(False);
  end;
  start := k;
  while (c[k] in ['0'..'9']) or (c[k] = '.') or ((c[k] = '-') and (k = start)) do
    Inc(k);
  Dec(k);
  Ist := Copy(C, OldK + 1, k - OldK);
  I := StrToFloat(Ist);
  Result := True;
end;

function GS(C, Target: string; var k: integer): boolean;//get String
var
  OldK, len, n: integer;
begin
  OldK := k;
  Inc(k);
  while (C[k] = ' ') or (C[k] = #10) or (C[k] = #13) do
    Inc(k);
  len := Length(Target);
  n := 0;
  while n < len do
  begin
    if Target[n + 1] <> C[k] then
    begin
      k := OldK;
      Exit(False);
    end;
    Inc(k);
    Inc(n);
  end;
  Dec(k);
  Result := True;
end;

function GMS(C: string; Target: array of string; var k: integer): boolean;
  //get multi String
var
  n, Oldk: integer;
  T: string;
begin
  Oldk := k;
  for n := 0 to Length(Target) - 1 do
  begin
    T := Target[n];
    if not GS(C, T, k) then
    begin
      k := Oldk;
      Exit(False);
    end;
  end;
  Exit(True);
end;

function FS(C: string; T: char; var k: integer): boolean;
var
  NewPos: SizeInt;
  n, OldK: integer;
begin
  Result := False;
  OldK := k;
  n := 1;
  NewPos := NPos(T, C, n);
  while (NewPos < k) and (NewPos > -1) do
  begin
    Inc(n);
    NewPos := NPos(T, C, n);
  end;
  if NewPos > 0 then
  begin
    k := NewPos + Length(T) - 1;
    Result := True;
  end
  else
    k := oldK;
end;

function GetC(C: string; T: char; var k: integer): boolean;
var
  OldK: integer;
begin
  OldK := k;
  while (C[k] = ' ') or (C[k] = #10) or (C[k] = #13) do
    Inc(k);
  if C[k] <> T then
  begin
    k := OldK;
    Exit(False);
  end;
end;

function AddBus(const Neema: TNeema; const C: string; var k: integer;
  XPos, YPos: integer): boolean;
var
  cBus: TBus;
  BusVAngle, BusVm, BusB, BusG, BusQ, Busp: double;
  BusArea, BusType, BusIndex: integer;
  cShunt: TShunt;
begin
  if not GS(C, '[', k) then
    Exit(False);
  GI(C, k, BusIndex);
  GS(C, ',', k);
  GI(C, k, BusType);
  GS(C, ',', k);
  GF(C, k, Busp);
  GS(C, ',', k);
  GF(C, k, BusQ);
  GS(C, ',', k);
  GF(C, k, BusG);
  GS(C, ',', k);
  GF(C, k, BusB);
  GS(C, ',', k);
  GI(C, k, BusArea);
  GS(C, ',', k);
  GF(C, k, BusVm);
  GS(C, ',', k);
  GF(C, k, BusVAngle);
  FS(C, ']', k);
  cBus := Neema.AddElement(XPos, YPos, TBus) as TBus;
  if BusType = 1 then
    cBus.BusType := loadbus
  else if BusType = 2 then
    cBus.BusType := regulatingbus
  else if BusType = 3 then
    cBus.BusType := slackbus;
  cBus.InjectedP := FloatToStr(Busp * -1 / 100);
  cBus.Injectedq := FloatToStr(BusQ * -1 / 100);
  cBus.BusVoltageI := '0';
  if cBus.BusType = regulatingbus then
    cBus.BusVoltageR := FloatToStr(BusVm)
  else
  begin
    cBus.BusVoltageR := FloatToStr(BusVm * cos(BusVAngle * pi / 180));
    cBus.BusVoltageI := FloatToStr(BusVm * Sin(BusVAngle * pi / 180));
  end;
  cBus.Name := 'bus' + BusIndex.ToString();
  if busG > 1e-5 then
    raise Exception.Create('busg is not suported');
  if BusB > 1e-5 then
  begin
    cShunt := Neema.AddElement(XPos, YPos, TShunt) as TShunt;
    cShunt.BaseVA := '1';
    cShunt.BaseVolt := '1';
    cShunt.CBClose := True;
    cShunt.ShuntVAR := FloatToStr((-BusB / 100));
    Neema.CDrawing.ConectElements(cBus.ID, cShunt.ID);
  end;
end;

function AddGenData(const Neema: TNeema; const C: string; var k: integer): boolean;
var
  Bus: TBus;
  P: double;
  BusIndex: integer;
begin
  if not GS(C, '[', k) then
    Exit(False);
  GI(C, k, BusIndex);
  GS(C, ',', k);
  GF(C, k, P);
  GS(C, ',', k);
  FS(C, ']', k);
  Bus := Neema.GetElement('bus' + BusIndex.ToString()) as TBus;
  Bus.InjectedP := FloatToStr(UtilUnit.StringToWat(Bus.InjectedP) + P / 100);
end;

function AddBranch(const Neema: TNeema; const C: string; var k: integer): boolean;
var
  Bus1, Bus2: TBus;
  R, X, B, rateB, RateA, RateC, TapAngel, TapRatio, a, Leg1, Leg2: double;
  Bus1Index, Bus2Index: integer;
  Oz1, Oz2, Oz3: complex;
  PiImp: TPiImpedance;
begin
  if not GS(C, '[', k) then
    Exit(False);
  GI(C, k, Bus1Index);
  GS(C, ',', k);
  GI(C, k, Bus2Index);
  GS(C, ',', k);
  GF(C, k, R);
  GS(C, ',', k);
  GF(C, k, X);
  GS(C, ',', k);
  GF(C, k, B);
  GS(C, ',', k);
  GF(C, k, RateA);
  GS(C, ',', k);
  GF(C, k, rateB);
  GS(C, ',', k);
  GF(C, k, RateC);
  GS(C, ',', k);
  GF(C, k, TapRatio);
  GS(C, ',', k);
  GF(C, k, TapAngel);
  FS(C, ']', k);
  Bus1 := Neema.GetElement('bus' + Bus1Index.ToString()) as TBus;
  Bus2 := Neema.GetElement('bus' + Bus2Index.ToString()) as TBus;
  PiImp := Neema.AddElement(round(Bus1.XPos / 2 + Bus2.XPos / 2),
    round(Bus1.YPos / 2 + Bus2.YPos / 2), TPiImpedance) as TPiImpedance;
  Neema.CDrawing.ConectElements(Bus1.ID, PiImp.ID);
  Neema.CDrawing.ConectElements(Bus2.ID, PiImp.ID);
  PiImp.ImpedanceR2 := FloatToStr(R);
  PiImp.ImpedanceX2 := FloatToStr(X);
  {if abs(B) > 1e-6 then
    PiImp.ImpedanceR1 := FloatToStr(-1 / B);   }
  if abs(B) > 1e-6 then
  begin
    PiImp.ImpedanceX1 := FloatToStr(-2 / B);
    PiImp.ImpedanceX3 := FloatToStr(-2 / B);
  end;
  if (TapRatio) > 1e-6 then
  begin
    a := TapRatio;
    //tap are reversed
    Leg2 := infto13(a, (a - 1));
    Leg1 := infto13((a * a), (1 - a));
    Oz1 := cinit(R, X) * Leg1;
    Oz2 := cinit(R, X) * a;
    Oz3 := cinit(R, X) * Leg2;
    PiImp.ImpedanceR1 := FloatToStr(Oz1.re);
    PiImp.ImpedanceX1 := FloatToStr(Oz1.im);
    PiImp.ImpedanceR2 := FloatToStr(Oz2.re);
    PiImp.ImpedanceX2 := FloatToStr(Oz2.im);
    PiImp.ImpedanceR3 := FloatToStr(Oz3.re);
    PiImp.ImpedanceX3 := FloatToStr(Oz3.im);
  end;
end;

procedure SetBusPos(var XPos: integer; var YPos: integer);
begin
  if XPos < 920 then
    Inc(XPos, 300)
  else
  begin
    XPos := 10;
    Inc(YPos, 150);
  end;
end;

procedure ReadBuss(const Neema: TNeema; const C: string);
var
  XPos: integer;
  YPos: integer;
  k: integer;
begin
  k := Pos('ppc["bus"]', C) - 1;
  if k < 1 then
    Exit;
  GMS(C, ['ppc', '[', '"', 'bus', '"', ']', '=', 'array', '(', '['], k);
  YPos := 10;
  XPos := 10;
  AddBus(Neema, C, k, XPos, YPos);
  if GS(C, ',', k) then
  begin
    SetBusPos(XPos, YPos);
    while AddBus(Neema, C, k, XPos, YPos) do
    begin
      SetBusPos(XPos, YPos);
      if not GS(C, ',', k) then
        Break;
    end;
  end;
end;

procedure ReadBranch(const Neema: TNeema; const C: string);
var
  k: integer;
begin
  k := Pos('ppc["branch"]', C) - 1;
  if k < 1 then
    Exit;
  GMS(C, ['ppc', '[', '"', 'branch', '"', ']', '=', 'array', '(', '['], k);
  AddBranch(Neema, C, k);
  if GS(C, ',', k) then
  begin
    while AddBranch(Neema, C, k) do
    begin
      if not GS(C, ',', k) then
        Break;
    end;
  end;
end;

procedure ReadGens(const Neema: TNeema; const C: string);
var
  k: integer;
begin
  k := Pos('ppc["gen"]', C) - 1;
  if k < 1 then
    Exit;
  GMS(C, ['ppc', '[', '"', 'gen', '"', ']', '=', 'array', '(', '['], k);
  AddGenData(Neema, C, k);
  if GS(C, ',', k) then
  begin
    while AddGenData(Neema, C, k) do
    begin
      if not GS(C, ',', k) then
        Break;
    end;
  end;
end;

function ImportCase(FileName: string; NeemaObject: TObject): TObject;
var
  Content: TStringList;
  C: string;
  Neema: TNeema;
begin
  Result := nil;
  Neema := NeemaObject as TNeema;
  Content := TStringList.Create;
  Content.LoadFromFile(FileName);
  C := Content.Text;
  ReadBuss(Neema, C);
  ReadBranch(Neema, C);
  ReadGens(Neema, C);
end;

end.
