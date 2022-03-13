unit TransmissionLineUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex, UtilUnit, neemacanvasUnit,
  SEMeasurementUnit, drawingutilunit, PiInterfaceUnit;

type

  { TLine }

  TLine = class(TElicment, IPI)
  private
    FLength, FR, FX, FC, FRC1, FRC2: string;
    FCB1Close, FCB2Close, FRC1Close, FRC2Close: boolean;
    procedure DrawMeasurment(const c: INeemaCanvas);
    procedure GetConectionPos2(isfrist: boolean; out ConX, ConY: integer);
    procedure GetImp(out oz1, oz2, oz3: complex);
    procedure AddRCImp(CB: boolean; RC: string; var imp: complex);
  public
    M: Tpimeasurmen;
    CalculatedS1, CalculatedS2, CalculatedV1, CalculatedV2: complex;
    MainColor: integer;
    procedure BussIDs(out aID1, aID2: integer);
    procedure CBSatatus(out CB1, CB2: boolean);
    constructor Create; override;
    destructor Destroy; override;
    function GetMaxConectioNum(): integer; override;
    function GetNextConectionPos(out ConX, ConY: integer): boolean; override;
    function GetConectionPos(cID: integer; out ConX, ConY: integer): boolean; override;
    function PISelf: TObject;
    procedure setData(cr, cx, cc, clength, cv, cVA: double);
    procedure GetImpPerUnit(out oz1, oz2, oz3: complex);
    procedure GetSPerUnit(V1PerUnit, V2PerUnit: complex; out S12, S21: complex);
    procedure UpdateCalculatedS();
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
    procedure OpenAllCB(); override;
    function GetMeasurment(): TMeasurmen; override;
  published
    property Length: string read FLength write FLength;
    property R: string read FR write FR;
    property x: string read FX write FX;
    property C: string read FC write FC;
    property RC1: string read FRC1 write FRC1;
    property RC2: string read FRC2 write FRC2;
    property CB1Close: boolean read FCB1Close write FCB1Close;
    property CB2Close: boolean read FCB2Close write FCB2Close;
    property RC1Close: boolean read FRC1Close write FRC1Close;
    property RC2Close: boolean read FRC2Close write FRC2Close;
  end;

const
  TLINEWIDTH = 180;
  TLINEHEIGHT = 10;
  SELECTIONMARGIN = 3;

implementation


{ TLine }

procedure TLine.setData(cr, cx, cc, clength, cv, cVA: double);
begin
  R := cr.ToString();
  X := cx.ToString();
  C := cc.ToString();
  Length := clength.ToString();
  BaseVolt := cv.ToString();
  BaseVA := cVA.ToString();
end;

procedure TLine.GetImpPerUnit(out oz1, oz2, oz3: complex);
var
  BaseImp: double;
begin
  GetImp(oz1, oz2, oz3);
  BaseImp := BaseImpedance(StringToVolt(BaseVolt), StringToVA(BaseVA));
  oz1 := oz1 / BaseImp;
  oz2 := oz2 / BaseImp;
  oz3 := oz3 / BaseImp;
end;

procedure TLine.GetImp(out oz1, oz2, oz3: complex);
var
  z, yLine, gamma, Zc, Y2: complex;
  y1: double;
begin
  y1 := (C.ToDouble);
  if (y1 < 1) then
    raise Exception.Create('the line capicitance is vey high');
  z := (R.ToDouble + i * x.ToDouble) * Length.ToDouble;
  yLine := ((1 / y1) * i) * Length.ToDouble();
  gamma := csqrt(z * yLine);
  Zc := csqrt(z / yLine);
  oz2 := Zc * csh(gamma);
  Y2 := (1 / Zc) * cth(gamma / 2);
  oz1 := 1 / y2;
  oz3 := oz1;
  AddRCImp(RC1Close, RC1, oz1);
  AddRCImp(RC2Close, RC2, oz3);
  //setimpedance(zy2.re, zy2.im, Z2.re, Z2.im, zy2.re, zy2.im, nomv,gelist);
end;

procedure TLine.AddRCImp(CB: boolean; RC: string; var imp: complex);
var
  RCD, BV: double;
  RCImp: complex;
begin
  if CB then
  begin
    RCD := StringToVar(RC);
    if RCD < 0 then
      raise Exception.Create('negative RC1');
    BV := StringToVolt(BaseVolt);
    RCImp := (Bv * BV) / (RCD) * i;
    imp := (imp * RCImp) / (imp + RCImp);
  end;
end;

constructor TLine.Create;
begin
  inherited Create;
  m := Tpimeasurmen.Create('');
  IniServerDataString();
  RC1Close := False;
  RC2Close := False;
  CB1Close := True;
  CB2Close := True;
  MainColor := clNemBlack;
end;

procedure TLine.CBSatatus(out CB1, CB2: boolean);
begin
  CB1 := CB1Close;
  CB2 := CB2Close;
end;

procedure TLine.BussIDs(out aID1, aID2: integer);
begin
  aID1 := -1;
  aID2 := -1;
  if Conections.Count > 0 then
    aID1 := Conections[0];
  if Conections.Count > 1 then
    aID2 := Conections[1];
end;

destructor TLine.Destroy;
begin
  m.Free;
  inherited Destroy;
end;

function TLine.GetMaxConectioNum(): integer;
begin
  Result := 2;
end;

procedure TLine.GetSPerUnit(V1PerUnit, V2PerUnit: complex; out S12, S21: complex);
var
  V1, Del1, V2, Del2, Z1, Lamda1, Z2, Lamda2, Z3, Lamda3: double;
  oz3, oz2, oz1, VALeg1, VALeg2, TempS12: complex;
  Q12, P12: double;
  P21, Q21: real;
begin
  V1 := cmod(V1PerUnit);
  Del1 := carg(V1PerUnit);
  V2 := cmod(V2PerUnit);
  Del2 := carg(V2PerUnit);
  GetImpPerUnit(oz1, oz2, oz3);
  Z1 := cmod(oz1);
  Lamda1 := carg(oz1);
  Z2 := cmod(oz2);
  Lamda2 := carg(oz2);
  Z3 := cmod(oz3);
  Lamda3 := carg(oz3);
  TempS12 := 1 / oz2;
  TempS12 := V1PerUnit * cong((V1PerUnit - V2PerUnit) / oz2);
  P12 := V1 * V1 / Z2 * cos(Lamda2) - V1 * V2 / Z2 * cos(Lamda2 + Del1 - Del2);
  Q12 := V1 * V1 / Z2 * sin(Lamda2) - V1 * V2 / Z2 * sin(Lamda2 + Del1 - Del2);
  VALeg1 := V1PerUnit * cong(V1PerUnit / oz1);
  VALeg2 := V2PerUnit * cong(V2PerUnit / oz3);
  P12 := P12 + VALeg1.re;
  Q12 := Q12 + VALeg1.im;
  S12 := P12 + i * Q12;
  P21 := V2 * V2 / Z2 * cos(Lamda2) - V2 * V1 / Z2 * cos(Lamda2 + Del2 - Del1);
  Q21 := V2 * V2 / Z2 * sin(Lamda2) - V2 * V1 / Z2 * sin(Lamda2 + Del2 - Del1);
  P21 := P21 + VALeg2.re;
  Q21 := Q21 + VALeg2.im;
  S21 := P21 + i * Q21;
end;

procedure TLine.UpdateCalculatedS();
var
  S12, S21: complex;
begin
  GetSPerUnit(CalculatedV1 / StringToVolt(BaseVolt),
    CalculatedV2 / StringToVolt(BaseVolt), S12, S21);
  CalculatedS1 := S12 * StringToVA(BaseVA);
  CalculatedS2 := S21 * StringToVA(BaseVA);
end;

procedure TLine.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldBrushColour: integer;
begin
  inherited Draw(c, Tick);
  OldBrushColour := c.getBrushColor;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(Self.XPos - 3, YPos - 3, XPos + TLINEWIDTH + 3, YPos + TLINEHEIGHT + 3);
  end;
  c.setBrushColor(MainColor);
  if Dead then
    c.setBrushColor(clNemWhite);
  c.Rectangle(XPos + CBHALFWIDTH, YPos, XPos + TLINEWIDTH - CBHALFWIDTH,
    YPos + TLINEHEIGHT);
  DrawCB(c, XPos, YPos + round(TLINEHEIGHT / 2), CB1Close, clNemBlack);
  DrawCB(c, XPos + TLINEWIDTH, YPos + round(TLINEHEIGHT / 2), CB2Close, clNemBlack);
  if RC1.Length > 0 then
  begin
    DrawCB(c, XPos + CBHALFWIDTH * 2, YPos + TLINEHEIGHT * 3, RC1Close, MainColor);
    DrawRC(c, XPos + CBHALFWIDTH * 2, YPos + TLINEHEIGHT * 4, MainColor);
  end;
  if RC2.Length > 0 then
  begin
    DrawCB(c, XPos + TLINEWIDTH - CBHALFWIDTH * 2, YPos + TLINEHEIGHT *
      3, RC2Close, MainColor);
    DrawRC(c, XPos + TLINEWIDTH - CBHALFWIDTH * 2, YPos + TLINEHEIGHT * 4, MainColor);
  end;
  DrawMeasurment(c);
  c.setBrushColor(OldBrushColour);
end;

function TLine.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
  if (X > XPos) and (X < XPos + TLINEWIDTH) and
    (Y < YPos + TLINEHEIGHT + SELECTIONMARGIN) and (Y > YPos - SELECTIONMARGIN) then
  begin
    IsSelected := True;
    Result := True;
  end;
end;

procedure TLine.OpenAllCB();
begin
  CB1Close := False;
  CB2Close := False;
end;

function TLine.GetMeasurment(): TMeasurmen;
begin
  Result := M;
end;

procedure TLine.GetConectionPos2(isfrist: boolean; out ConX, ConY: integer);
begin
  if isfrist then
  begin
    ConX := XPos;
    ConY := YPos + round(TLINEHEIGHT / 2);
  end
  else
  begin
    ConX := XPos + TLINEWIDTH;
    ConY := YPos + round(TLINEHEIGHT / 2);
  end;
end;

procedure TLine.DrawMeasurment(const c: INeemaCanvas);
var
  Mult: integer;
begin
  c.setBrushColor(clNemWhite);
  //mw  volt
  Mult := 1;
  if CalculatedS1.re * Mult > 0 then
    c.TextOut(XPos, YPos - 20, UtilUnit.WatToString(abs(CalculatedS1.re)) + '>')
  else
    c.TextOut(XPos, YPos - 20, UtilUnit.WatToString(abs(CalculatedS1.re)) + '<');
  if CalculatedS1.im * Mult > 0 then
    c.TextOut(XPos, YPos - 40, UtilUnit.VarToString(abs(CalculatedS1.im)) + '>')
  else
    c.TextOut(XPos, YPos - 40, UtilUnit.VarToString(abs(CalculatedS1.im)) + '<');
  Mult := -1;
  if CalculatedS2.re * Mult > 0 then
    c.TextOut(XPos + TLINEWIDTH - 50, YPos - 20, UtilUnit.WatToString(
      abs(CalculatedS2.re)) + '>')
  else
    c.TextOut(XPos + TLINEWIDTH - 50, YPos - 20, UtilUnit.WatToString(
      abs(CalculatedS2.re)) + '<');
  if CalculatedS2.im * Mult > 0 then
    c.TextOut(XPos + TLINEWIDTH - 50, YPos - 40, UtilUnit.VarToString(
      abs(CalculatedS2.im)) + '>')
  else
    c.TextOut(XPos + TLINEWIDTH - 50, YPos - 40, UtilUnit.VarToString(
      abs(CalculatedS2.im)) + '<');
end;

function TLine.GetNextConectionPos(out ConX, ConY: integer): boolean;
begin
  Result := False;
  if Conections.Count < GetMaxConectioNum() then
  begin
    Result := True;
    GetConectionPos2(Conections.Count = 0, ConX, ConY);
  end;
end;

function TLine.GetConectionPos(cID: integer; out ConX, ConY: integer): boolean;
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

function TLine.PISelf: TObject;
begin
  Result := Self;
end;

end.
