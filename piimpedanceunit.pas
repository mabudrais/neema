unit PiImpedanceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, neemacanvasUnit, UtilUnit,PiInterfaceUnit;

type

  { TPiImpedance }

  TPiImpedance = class(TElicment,IPI)
  private
    FImpedanceR1, FImpedanceR2, FImpedanceR3, FImpedanceX1, FImpedanceX2,
    FImpedanceX3: string;
  public
    procedure BussIDs(out aID1, aID2: integer);
    procedure CBSatatus(out CB1, CB2: boolean);
    constructor Create; override;
    function GetMaxConectioNum(): integer; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function PISelf: TObject;
    function SetSelected(X, Y: integer): boolean; override;
    procedure GetConectionPos2(isfrist: boolean; out ConX, ConY: integer);
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    procedure GetImpPerUnit(out oz1, oz2, oz3: complex);
  published
    property ImpedanceR1: string read FImpedanceR1 write FImpedanceR1;
    property ImpedanceR2: string read FImpedanceR2 write FImpedanceR2;
    property ImpedanceR3: string read FImpedanceR3 write FImpedanceR3;
    property ImpedanceX1: string read FImpedanceX1 write FImpedanceX1;
    property ImpedanceX2: string read FImpedanceX2 write FImpedanceX2;
    property ImpedanceX3: string read FImpedanceX3 write FImpedanceX3;
  end;

implementation

{ TPiImpedance }

procedure TPiImpedance.BussIDs(out aID1, aID2: integer);
begin
        aID1 := -1;
    aID2 := -1;
    if Conections.Count > 0 then
      aID1 := Conections[0];
    if Conections.Count > 1 then
      aID2 := Conections[1];

end;

procedure TPiImpedance.CBSatatus(out CB1, CB2: boolean);
begin
CB1:=True;
CB2:=True;
end;

constructor TPiImpedance.Create;
begin
  inherited Create;
  ImpedanceR1 := '0';
  ImpedanceR2 := '0';
  ImpedanceR3 := '0';
  Impedancex1 := '1e13';
  Impedancex2 := '1';
  ImpedanceX3 := '1e13';
end;

function TPiImpedance.GetMaxConectioNum(): integer;
begin
  Result := 2;
end;

procedure TPiImpedance.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldPenColor: TNeemaColour;
  OldBrushColour: integer;
begin
  OldPenColor := C.getPenColor;
  OldBrushColour := c.getBrushColor;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(XPos - 11, YPos - 6, XPos + 21, YPos + 20);
  end;
  c.setBrushColor(ClNemSilver);
  c.setPenColor(clNemBlack);
  c.Rectangle(XPos - 10, YPos - 5, XPos + 20, YPos + 19);
  c.setBrushColor(ClNemSilver);
  c.TextOut(XPos, YPos, 'pi');
  c.setBrushColor(OldBrushColour);
  c.setPenColor(OldPenColor);
end;

function TPiImpedance.PISelf: TObject;
begin
      Result:=Self;
end;

function TPiImpedance.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
  if isINSR(X, Y, XPos - 10, YPos, XPos + 20, YPos + 20) then
  begin
    Result := True;
    IsSelected := True;
  end;
end;

procedure TPiImpedance.GetConectionPos2(isfrist: boolean; out ConX, ConY: integer);
begin
  if isfrist then
  begin
    ConX := XPos - 10;
    ConY := YPos - 5;
  end
  else
  begin
    ConX := XPos + 20;
    ConY := YPos - 5;
  end;
end;

function TPiImpedance.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
var
  Index: integer;
begin
  Result := False;
  Index := Conections.IndexOf(cID);
  if Index > -1 then
  begin
    GetConectionPos2(Index = 0, ConX, ConY);
    Result := True;
  end;
end;

function TPiImpedance.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    GetConectionPos2(Conections.Count = 0, ConX, ConY);
  end;
end;

procedure TPiImpedance.GetImpPerUnit(out oz1, oz2, oz3: complex);
var
  VB, BaseImp: extended;
begin
  VB := StringToVolt(BaseVolt);
  BaseImp := VB * VB / StringToVA(BaseVA);
  oz1 := cinit(StrToFloat(ImpedanceR1), StrToFloat(ImpedanceX1)) / VB;
  oz2 := cinit(StrToFloat(ImpedanceR2), StrToFloat(ImpedanceX2)) / VB;
  oz3 := cinit(StrToFloat(ImpedanceR3), StrToFloat(ImpedanceX3)) / VB;
end;

end.
