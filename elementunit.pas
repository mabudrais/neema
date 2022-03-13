unit elementUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, neemacanvasUnit, PersistentObjectListUnit, StringSplitPatchUnit
  , GenircDataUnit;

type

  { TElement }
  //element  constructor should not have parameter and must be override
  TElement = class abstract(TPersistedJsonObject)
  private
    FID: integer;
    FVisable: boolean;
    FName: string;
    FX, FY: integer;
    function GetConectionsStr: string;
    procedure SetConectionsStr(AValue: string);
    function GetConectionCount: integer;
  protected
    Conections: TIntegerList; //please don element dirctly use addConection Function
  public
    IsSelected: boolean;
    property ConectionCount: integer read GetConectionCount;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); virtual;
    procedure draw_background(const c: INeemaCanvas; XT, YT: integer);
    procedure SetXY(cx, cy: integer);
    function GetNextConectionPos(out ConX, ConY: integer): boolean; virtual;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean; virtual;
    function GetMaxConectioNum(): integer; virtual;
    procedure AddConection(ConID: integer); virtual;
    procedure RemoveConection(ConID: integer); virtual;
    function GetConection(Index: integer): integer;
    function IsConectedTo(ToID:Integer):Boolean;
    function SetSelected(X, Y: integer): boolean; virtual;
  published
    property ID: integer read FID write FID;
    property Name: string read FName write FName;
    property Visable: boolean read FVisable write FVisable;
    property XPos: integer read FX write FX;
    property YPos: integer read FY write FY;
    property ConectionsStr: string read GetConectionsStr write SetConectionsStr;
  end;

  TElementClass = class of TElement;

implementation


{ TElement }

procedure TElement.Draw(const c: INeemaCanvas; Tick: boolean);
begin

end;

procedure TElement.draw_background(const c: INeemaCanvas; XT, YT: integer);
begin

end;

procedure TElement.SetXY(cx, cy: integer);
begin
  XPos := cx;
  YPos := cy;
end;

function TElement.GetConectionsStr: string;
var
  Comma: string;
  k: integer;
begin
  Comma := '';
  Result := '';
  for k := 0 to Conections.Count - 1 do
  begin
    Result := Result + Comma + IntToStr(Conections[k]);
    Comma := ',';
  end;
end;

procedure TElement.SetConectionsStr(AValue: string);
var
  Data: TStringArray;
  k: integer;
begin
  Data := StringSplitPatch(AValue, [','], TStringSplitOptions.ExcludeEmpty);
  for k := 0 to Length(Data) - 1 do
  begin
    Conections.Add(StrToInt(Data[k]));
  end;
end;

function TElement.GetMaxConectioNum(): integer;
begin
  raise Exception.Create('not implemnted');
  Result := -1;
end;

function TElement.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    ConX := XPos;
    ConY := YPos;
  end;
end;

function TElement.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
var
  Index: integer;
begin
  Result := False;
  Index := Conections.IndexOf(cId);
  if Index > -1 then
  begin
    Result := True;
    ConX := XPos;
    ConY := YPos;
  end;
end;

constructor TElement.Create;
begin
  inherited Create;
  Conections := TIntegerList.Create;
end;

destructor TElement.Destroy;
begin
  Conections.Free;
  inherited Destroy;
end;

procedure TElement.AddConection(ConID: integer);
begin
  if ConID = ID then
    raise Exception.Create('You can''t connect element to self');
  Conections.Add(ConID);
end;

procedure TElement.RemoveConection(ConID: integer);
begin
  Conections.Remove(ConID);
end;

function TElement.GetConection(Index: integer): integer;
begin
  if Index >= Conections.Count then
    raise Exception.Create('Index>= Conections.Count');
  Result := Conections[Index];
end;

function TElement.IsConectedTo(ToID: Integer): Boolean;
var
  k: Integer;
begin
  Result:=False;
  for k:=0 to Conections.Count-1 do
  begin
    if Conections[k]=ToID then Exit(True);
  end;
end;

function TElement.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
end;

function TElement.GetConectionCount: integer;
begin
  Result := Conections.Count;
end;

end.
