unit ElectricElentUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo
  , elementUnit, UComplex, UtilUnit, SEMeasurementUnit;

type

  { TElicment }

  TElicment = class abstract(TElement)
  private
    FBaseVA: string; //in VA not mva
    FBaseVolt: string;
    FServerData: string;
    procedure SetFBaseVolt(AValue: string);
    procedure SetBaseVA(AValue: string);
  public
    Dead: boolean;
    IsObservableByEstateEstimator: boolean;
    function PerUnitFromImp(cImp: complex): complex;
    function pfMVA(MVA: complex): complex;
    function VoltToPerUnt(aV: complex): complex;
    constructor Create; override;
    procedure OpenAllCB(); virtual;
    function GetMeasurment(): TMeasurmen; virtual;
    procedure IniServerDataString();
  published
    property BaseVA: string read FBaseVA write SetBaseVA; //in VA not mva
    property BaseVolt: string read FBaseVolt write SetFBaseVolt;
    property ServerData: string read FServerData write FServerData;
  end;

implementation

{ TElicment }

function TElicment.PerUnitFromImp(cImp: complex): complex;
var
  BV, BaseImp: double;
begin
  BV := StringToVolt(BaseVolt);
  BaseImp := (BV * BV) / StringToVA(BaseVA);
  Result := cImp / BaseImp;
end;

function TElicment.pfMVA(MVA: complex): complex;
begin
  Result := MVA / StringToVA(BaseVA);
end;

function TElicment.VoltToPerUnt(aV: complex): complex;
begin
  Result := aV / StringToVolt(BaseVolt);
end;

constructor TElicment.Create;
begin
  inherited Create;
  BaseVolt := '1';
  BaseVA := '1';
end;

procedure TElicment.OpenAllCB();
begin

end;

function TElicment.GetMeasurment(): TMeasurmen;
begin
  Result := nil;
end;

procedure TElicment.IniServerDataString();
var
  Mtyps: TStringList;
  M, k: integer;
  ME: TMeasurmen;
begin
  Mtyps := TStringList.Create;
  Mtyps.AddStrings(['P', 'Q', 'V', 'I']);
  ME := GetMeasurment();
  if ME = nil then
    raise Exception.Create('M is null');
  for M := 0 to Mtyps.Count - 1 do
  begin
    if IsPublishedProp(ME, Mtyps[M]) then
      ServerData := ServerData + Mtyps[M] + ':' + EmptyMString + ':' + EmptyMString + ':0;';
    for k := 1 to 3 do
    begin
      if IsPublishedProp(ME, Mtyps[M] + k.ToString()) then
        ServerData := ServerData + Mtyps[M] + k.ToString() + ':' + EmptyMString + ':' + EmptyMString + ':0;';
    end;
  end;
end;

procedure TElicment.SetFBaseVolt(AValue: string);
var
  Volt: extended;
begin
  if FBaseVolt = AValue then
    Exit;
  if StringToVolt(LowerCase(AValue), Volt) then
    FBaseVolt := AValue;
end;

procedure TElicment.SetBaseVA(AValue: string);
var
  VA: extended;
begin
  if FBaseVA = AValue then
    Exit;
  if StringToVA(AValue, VA) then
    FBaseVA := AValue;
end;

end.
