unit ElementColoringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GenircDataUnit, neemacanvasUnit, Math, UtilUnit;

type

  { TColoring }

  TColoring = class
  public
    procedure ModifyColor(Drawing: TObject);
  private
    procedure BusColoring(const aBus: TObject);
    procedure MultBusColoring(const aMBus: TObject);
    function GetVoltColor(Volt: double): TNeemaColour;
    procedure LineColoring(const cTr: TObject);
    procedure ShuntColoring(E: TObject);
    procedure SVCColoring(const cSVC: TObject);
    procedure T2Coloring(const cT2: TObject);
    procedure T3Coloring(const cT3: TObject);
    procedure T3ShuntColoring(const cT3: TObject);
    function VoltToColor(aVoltString: string): TNeemaColour;
    function VoltToColor(aVoltString: string; E: TObject): TNeemaColour;
  end;

implementation

uses
  DrawingUnit, TransFormer2WUnit, TransFormer3WUnit, TransFormer3WShuntUnit,
  TransmissionLineUnit, SVCUnit, ShuntUnit, BusUnit, multibusunit, ElectricElentUnit;

{ TColoring }
function TColoring.GetVoltColor(Volt: double): TNeemaColour;
begin
  if SameValue(500 * 1000, Volt, 1) then
    Exit(clnemFuchsia)
  else if SameValue(220 * 1000, Volt, 1) then
    Exit(clYellow)
  else if SameValue(110 * 1000, Volt, 1) then
    Exit(clNemBlue)
  else if SameValue(33 * 1000, Volt, 1) then
    Exit(clNemRed)
  else
    Exit(clNemBlack);
end;

procedure TColoring.T2Coloring(const cT2: TObject);
var
  T2: TTransFormer2W;
  HColor, LColor: TNeemaColour;
begin
  T2 := cT2 as TTransFormer2W;
  HColor := VoltToColor(T2.PrimeryBaseolt, T2);
  LColor := VoltToColor(T2.SecendryBaseVolt, T2);
  T2.Circle1Color := HColor;
  T2.Circle2Color := LColor;
  T2.CB1Color := HColor;
  T2.CB2Color := LColor;
end;

procedure TColoring.T3Coloring(const cT3: TObject);
var
  HColor, LColor, MColor: TNeemaColour;
  T3: TTransFormer3W;
begin
  T3 := cT3 as TTransFormer3W;
  HColor := VoltToColor(T3.PrimeryBaseolt, T3);
  MColor := VoltToColor(T3.SecendryBaseVolt, T3);
  LColor := VoltToColor(T3.TertiaryBaseVolt, T3);
  T3.Circle1Color := HColor;
  T3.Circle2Color := MColor;
  T3.Circle3Color := LColor;
  T3.CB1Color := HColor;
  T3.CB2Color := MColor;
  T3.CB3Color := LColor;
end;

procedure TColoring.T3ShuntColoring(const cT3: TObject);
var
  HColor, LColor, MColor: TNeemaColour;
  T3: TTransFormer3WShunt;
begin
  T3 := cT3 as TTransFormer3WShunt;
  HColor := VoltToColor(T3.PrimeryBaseolt, T3);
  MColor := VoltToColor(T3.SecendryBaseVolt, T3);
  LColor := VoltToColor(T3.TertiaryBaseVolt, T3);
  T3.Circle1Color := HColor;
  T3.Circle2Color := MColor;
  T3.Circle3Color := LColor;
  T3.CB1Color := HColor;
  T3.CB2Color := MColor;
  T3.CB3Color := LColor;
end;

function TColoring.VoltToColor(aVoltString: string): TNeemaColour;
var
  MainColor: TNeemaColour;
  V: extended;
begin
  StringToVolt(aVoltString, V);
  Result := GetVoltColor(V);
end;

function TColoring.VoltToColor(aVoltString: string; E: TObject): TNeemaColour;
var
  Elic: TElicment;
begin
  Elic := E as TElicment;
  if Elic.Dead then
    Result := clNemWhite
  else
    Result := VoltToColor(aVoltString);
end;

procedure TColoring.SVCColoring(const cSVC: TObject);
var
  V2: extended;
  V1: extended;
  T2: TSVC;
  HColor, LColor: TNeemaColour;
begin
  T2 := cSVC as TSVC;
  HColor := VoltToColor(T2.TransformerPrimeryBaseolt, T2);
  t2.Circle1Color := HColor;
  T2.CB1Color := HColor;
  LColor := VoltToColor(T2.TransformerSecendryBaseVolt, T2);
  t2.Circle2Color := LColor;
  T2.CB2Color := LColor;
end;

procedure TColoring.LineColoring(const cTr: TObject);
var
  V: extended;
  MainColor: TNeemaColour;
  T: TLine;
begin
  T := cTr as TLine;
  T.MainColor := VoltToColor(T.BaseVolt, T);
end;

procedure TColoring.BusColoring(const aBus: TObject);
begin
  (aBus as TBus).BusColor := VoltToColor((aBus as TBus).BaseVolt, aBus);
end;

procedure TColoring.MultBusColoring(const aMBus: TObject);
var
  Mbus: TMultiBus;
  MainColor: TNeemaColour;
begin
  Mbus := aMBus as TMultiBus;
  MainColor := VoltToColor((aMBus as TMultiBus).BaseVolt);
  if Mbus.SubBusDead[0] then
    Mbus.Bus1Color := clNemWhite
  else
    Mbus.Bus1Color := MainColor;
  if Mbus.SubBusDead[1] then
    Mbus.Bus2Color := clNemWhite
  else
    Mbus.Bus2Color := MainColor;
end;

procedure TColoring.ShuntColoring(E: TObject);
begin
  (E as TShunt).Color := VoltToColor((E as TShunt).BaseVolt, E);
end;

procedure TColoring.ModifyColor(Drawing: TObject);
var
  El: TIntegerObjectMap;
  T2: TTransFormer2W;
  k: integer;
  e: TObject;
begin
  El := (Drawing as TDrawing).GetElementList;
  for k := 0 to EL.Count - 1 do
  begin
    e := El.Data[k];
    if E is TBus then
      BusColoring(El.Data[k] as TBus)
    else if E is TMultiBus then
      MultBusColoring(El.Data[k] as TMultiBus)
    else if E is TTransFormer2W then
      T2Coloring(El.Data[k] as TTransFormer2W)
    else if E is TTransFormer3W then
      T3Coloring(El.Data[k] as TTransFormer3W)
    else if E is TTransFormer3WShunt then
      T3ShuntColoring(El.Data[k] as TTransFormer3WShunt)
    else if e is TLine then
      LineColoring(e)
    else if e is TSVC then
      SVCColoring(e)
    else if e is TShunt then
      ShuntColoring(E);
  end;
end;

end.
