unit UtilUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex, fpexprpars,Math;

type
  VoltString = type string;

function StringToVolt(V: VoltString; out RVolt: extended): boolean;
function StringToVolt(V: VoltString): extended;
function VoltToString(V: double): VoltString;
function VoltToReadableString(V: complex): VoltString;
function WatToString(W: double): VoltString;
function VarToString(V: double): VoltString;
function StringToWat(WatString: string; out W: extended): boolean;
function StringToWat(WatString: string): extended;
function StringToVar(VarString: string; out va: extended): boolean;
function StringToVar(VarString: string): extended;
function StringToVA(VAString: string; out va: extended): boolean;
function StringToVA(VAString: string): extended;
function StringToIntElectricValue(ValueString: string; Sufix: string;
  out V: extended; AllowMult: boolean = True): boolean;
function WatToReadableString(W: double): string;
function VarToReadableString(W: double): string;
function BaseImpedance(V, va: double): double;
function InfTo13(c1, c2: double): double;
procedure StarToDelta(st1, st2, st3: complex; out Da, Db, Dc: complex);
procedure setNominalVoltages(stlist: TStrings);
procedure SetTwoVal(X, Y: integer; out T1, T2: integer);
function isINSR(X, Y, RX1, RY1, RX2, RY2: integer): boolean;
function IsINCircle(X, Y, XC, YC, R: integer): boolean;
function EvaluateExpression(const Expression: string; out RVolt: extended): boolean;
function EvaluateIntExpression(const Expression: string; out RVolt: integer): boolean;
function ParalelImp(Imp1, Imp2: complex): complex;
function SeriesAdmitance(A1, A2: complex): complex;
function IsOpenCircuit(Imp: complex): boolean;
function IsShortCircuit(Imp: complex): boolean;
function IsShortCircuitAdmitance(A1: complex): boolean;
function IsOpenCircuitAdmitance(A1: complex): boolean;
function safeDiv(A1, A2: extended): extended;
function NFF(V:Extended;D:Integer):String;
implementation

function StringToVolt(V: VoltString): extended;
begin
  StringToVolt(V, Result);
end;

function VoltToString(V: double): VoltString;
begin
  if v > 1000 then
    Result := FormatFloat('###.#', V / 1000) + ' KV'
  else
    Result := NFF(V,2) + 'V';
end;

function VoltToReadableString(V: complex): VoltString;
var
  Sufix: string;
begin
  if cmod(v) > 1000 then
  begin
    Sufix := 'KV';
    V := V / 1000;
  end;
  if cmod(V) > 100 then
    Result := FormatFloat('###.#', cmod(V)) + '<' +
      FormatFloat('###.#', carg(V) * 180 / pi) + Sufix
  else
    Result := FormatFloat('###.##', cmod(V)) + '<' +
      FormatFloat('###.#', carg(V) * 180 / pi) + Sufix;
end;

function ElectricValueToString(V: double; Sufix: string): VoltString;
begin
  if abs(V) > 1000000 then
    Result := FormatFloat('###.#', V / 1000 / 1000) + ' M' + Sufix
  else if abs(V) > 1000 then
    Result := FormatFloat('###.#', V / 1000) + ' K' + Sufix
  else
    Result := FloatToStr(V) + ' ' + Sufix;
end;
  function WatToReadableString(W: double): string;
begin
  Result := ElectricValueToString(W, 'W');
end;

function VarToReadableString(W: double): string;
begin
  Result :=ElectricValueToString(W, 'VAR');
end;
function StringToElectricValue(ValueString: string; Sufix: string;
  out V: extended; AllowMult: boolean = True): boolean;
begin
  ValueString := LowerCase(ValueString);
  if AllowMult then
  begin
    ValueString := StringReplace((ValueString), 'k' + Sufix, '*1000', []);
    ValueString := StringReplace((ValueString), 'm' + Sufix, '*1000000', []);
  end;
  ValueString := StringReplace((ValueString), Sufix, '*1', []);
  Result := EvaluateExpression(ValueString, V);
end;

function WatToString(W: double): VoltString;
begin
  Result := ElectricValueToString(W, 'w');
end;

function VarToString(V: double): VoltString;
begin
  Result := ElectricValueToString(V, 'var');
end;

function StringToVA(VAString: string): extended;
begin
  StringToVA(VAString, Result);
end;

function StringToIntElectricValue(ValueString: string; Sufix: string;
  out V: extended; AllowMult: boolean): boolean;
begin

end;

function BaseImpedance(V, va: double): double;
begin
  Result := V * V / va;
end;

function InfTo13(c1, c2: double): double;
begin
  if abs(c2) < 1e-13 then
    Result := 1e13
  else
    Result := c1 / c2;
end;

function EvaluateExpression(const Expression: string; out RVolt: extended): boolean;
var
  R: TFPExpressionResult;
  EP: TFPExpressionParser;
  L: integer;
begin
  Result := False;
 { L:=Expression.Length;
  if L<1 then
    begin
            RVolt:=0;Exit(True);
    end;        }
  EP := TFPExpressionParser.Create(nil);
  RVolt := 0;
  r.ResultType := rtString;
  try
    EP.Expression := Expression;
    R := EP.Evaluate;
  except
    on e: Exception do
  end;
  begin
    EP.Free;
  end;
  if R.ResultType = rtInteger then
    RVolt := R.ResInteger
  else if R.ResultType = rtFloat then
    RVolt := R.ResFloat
  else
    Exit;
  Result := True;
end;

function EvaluateIntExpression(const Expression: string; out RVolt: integer): boolean;
var
  R: TFPExpressionResult;
  EP: TFPExpressionParser;
  L: integer;
begin
  Result := False;
 { L:=Expression.Length;
  if L<1 then
    begin
            RVolt:=0;Exit(True);
    end;        }
  EP := TFPExpressionParser.Create(nil);
  RVolt := 0;
  EP.Expression := Expression;
  try
    R := EP.Evaluate;
  finally
    EP.Free;
  end;
  if R.ResultType = rtInteger then
    RVolt := R.ResInteger
  else
    Exit(False);
  Result := True;
end;

{ TVoltStringHelper }

function StringToVolt(V: VoltString; out RVolt: extended): boolean;
var
  Expression: string;
begin
  Expression := StringReplace(LowerCase(V), 'kv', '*1000', []);
  Expression := StringReplace(LowerCase(Expression), 'v', '*1', []);
  Result := EvaluateExpression(Expression, RVolt);
end;

function StringToWat(WatString: string; out W: extended): boolean;
begin
  Result := StringToElectricValue(WatString, 'w', w);
end;

function StringToWat(WatString: string): extended;
begin
  StringToWat(WatString, Result);
end;

function StringToVar(VarString: string; out Va: extended): boolean;
begin
  Result := StringToElectricValue(VarString, 'var', Va);
end;

function StringToVar(VarString: string): extended;
begin
  StringToVar(VarString, Result);
end;

function StringToVA(VAString: string; out va: extended): boolean;
begin
  Result := StringToElectricValue(VAString, 'va', va);
end;

procedure setNominalVoltages(stlist: TStrings);
begin
  stlist.Clear;
  stlist.AddStrings(['500kv', '220kv', '110kv', '66kv', '33kv', '11kv', '415', '1']);
end;

procedure SetTwoVal(X, Y: integer; out T1, T2: integer);
begin
  T1 := X;
  T2 := Y;
end;

function isINSR(X, Y, RX1, RY1, RX2, RY2: integer): boolean;
begin
  Result := (X >= RX1) and (X <= RX2) and (Y >= RY1) and (Y <= RY2);
end;

function IsINCircle(X, Y, XC, YC, R: integer): boolean;
var
  DistSQR: extended;
begin
  distSQr := sqr(X - XC) + sqr(Y - YC);
  if DistSQR < (R * R) then
    Result := True
  else
    Result := False;
end;

procedure StarToDelta(st1, st2, st3: complex; out Da, Db, Dc: complex);
var
  Mult: complex;
begin
  Mult := (st1 * st2 + st1 * st3 + st2 * st3);
  Da := Mult / st1;
  Db := Mult / st2;
  Dc := Mult / st3;
end;

function ParalelImp(Imp1, Imp2: complex): complex;
begin
  if IsOpenCircuit(Imp1) then
    Exit(Imp2);
  if IsOpenCircuit(Imp2) then
    Exit(Imp1);
  if (IsShortCircuit(Imp1)) or (IsShortCircuit(Imp1)) then
    raise Exception.Create('short circiut');
  Result := 1 / (1 / Imp1 + 1 / Imp2);
end;

function SeriesAdmitance(A1, A2: complex): complex;
begin
  if IsShortCircuitAdmitance(A1) then
    Exit(A2);
  if IsShortCircuitAdmitance(A2) then
    Exit(A1);
  if (IsOpenCircuitAdmitance(A1)) or (IsOpenCircuitAdmitance(A2)) then
    raise Exception.Create('openCiruit');
  Result := 1 / (1 / A1 + 1 / A2);
end;

function IsShortCircuitAdmitance(A1: complex): boolean;
begin
  if Abs(cmod(A1)) > 1e8 then
    Result := True
  else
    Result := False;
end;

function IsOpenCircuitAdmitance(A1: complex): boolean;
begin
  if Abs(cmod(A1)) < 1e-8 then
    Result := True
  else
    Result := False;
end;

function safeDiv(A1, A2: extended): extended;
begin
  if abs(A2) < 1e-8 then
    raise Exception.Create('');
  Result := A1 / A2;
end;

function NFF(V: Extended; D: Integer): String;
var
  R, DPI: Int64;
  RS, DPS: String;
  DP: Extended;
begin
  R:=Round(V);
  DP:=V-R;
  RS:=IntToStr(R);
  DPI:=0;
  if D>0 then
  DPI:=Round(DP*Power(10,D));
  DPS:='';
  if D>0 then
  DPS:='.'+StringOfChar('0',D-1)+IntToStr(DPI);
  Result:=RS+DPS;
end;

function IsOpenCircuit(Imp: complex): boolean;
begin
  if Abs(cmod(Imp)) > 1e8 then
    Result := True
  else
    Result := False;
end;

function IsShortCircuit(Imp: complex): boolean;
begin
  if Abs(cmod(Imp)) < 1e-8 then
    Result := True
  else
    Result := False;
end;

end.
