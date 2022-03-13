unit GenircDataUnit;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, gmap, gutil, fgl, UComplex;

type
  TIntegerList = specialize TFPGList<integer>;
  TDoubleList = specialize TFPGList<double>;

  TMRLMap = specialize TFPGMap<string, TObject>;
  TGeneralIntegerObjectMap = specialize TFPGMapObject<integer, TObject>;
  TGeneralInteger64ObjectMap = specialize TFPGMapObject<int64, TObject>;
  TIntegerIntegerMap = specialize TFPGMap<integer, integer>;
  TGeneralIntegerDoubleMap = specialize TFPGMap<int64, double>;
  TIntegerObjectMap = class;
  { TTreeEnumerator }

  TTreeEnumerator = class
  private
    FTree: TIntegerObjectMap;
    FCurrent: TObject;
    FCurrentIndex: integer;
  public
    constructor Create(ATree: TIntegerObjectMap);
    function MoveNext: boolean;
    property Current: TObject read FCurrent;
  end;

  { TIntegerDoubleMap }

  TIntegerDoubleMap = class(TGeneralIntegerDoubleMap)
    function Addine(const AKey: int64; const AData: double): integer;
      inline;//add if not exist
  end;

  { TIntegerObjectMap }

  TIntegerObjectMap = class(TGeneralIntegerObjectMap)
    constructor Create(AFreeObjects: boolean);
    constructor Create;
    function Addw(O: TObject): integer;
    function GetEnumerator: TTreeEnumerator;
  end;

  twodarray = array of array of complex;
  twodarrayex = array of array of extended;
  twodarrayd = array of array of double;

implementation

uses elementUnit;

{ TTreeEnumerator }

constructor TTreeEnumerator.Create(ATree: TIntegerObjectMap);
begin
  Self.FTree := ATree;
  Self.FCurrent := nil;
end;

function TTreeEnumerator.MoveNext: boolean;
begin
  if (FCurrent = nil) then
    FCurrentIndex := 0
  else
    Inc(FCurrentIndex);
  if FCurrentIndex < FTree.Count then
    FCurrent := FTree.Data[FCurrentIndex]
  else
    FCurrent := nil;
  Result := FCurrent <> nil;
end;

{ TIntegerDoubleMap }

function TIntegerDoubleMap.Addine(const AKey: int64; const AData: double): integer;
begin
  if Self.IndexOf(AKey) > -1 then
    // Raise Exception.Create('kea already exist');
    // WL(['key ', AKey.ToString(), ' already exist']);
    Result := Self.Add(AKey, AData);
end;

{ TIntegerObjectMap }

function TIntegerObjectMap.Addw(O: TObject): integer;
var
  maxkey, K: integer;
begin
  if not (o is TElement) then
    raise Exception.Create('not(o is TElicment)');
  maxkey := -1;
  for K := 0 to Self.Count - 1 do
  begin
    if Keys[k] > maxkey then
      maxkey := Keys[k];
  end;
  AddOrSetData(maxkey + 1, O);
  (o as TElement).ID := maxkey + 1;
  Result := maxkey + 1;
end;

function TIntegerObjectMap.GetEnumerator: TTreeEnumerator;
begin
  Result := TTreeEnumerator.Create(Self);
end;

constructor TIntegerObjectMap.Create(AFreeObjects: boolean);
begin
  inherited Create(AFreeObjects);
  Self.Duplicates := TDuplicates.dupError; //not working because is not sorted
end;

constructor TIntegerObjectMap.Create;
begin
  inherited Create;
  Self.Duplicates := TDuplicates.dupError;
end;

end.
