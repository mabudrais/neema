unit EEMartixUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, omv, typ, inv, sle;

type

  { TEEMartix }

  TEEMartix = class
  private
    A: array of extended;
    function getc(r, c: integer): extended;
    procedure setc(r, c: integer; v: extended);
  public
    row, Col: integer;
    constructor Create(r, c: integer);
    property cell[r, c: integer]: extended read getc write setc; default;
    procedure fill(V: PDouble);
    procedure fill(B: TEEMartix);
    procedure transpos(tr: TEEMartix);
    procedure sum(te: TEEMartix);
    procedure inv();
    procedure multwith(B, C: TEEMartix);
    procedure Solve(B, C: TEEMartix);
    procedure multwith(ml: extended);
    function MaxDaig: extended;
    procedure writemat(Filename: string);
    procedure writematscilab(Filename: string);
    procedure print();
    procedure setElemntes(E: array of extended);
    function Max(): extended;
  end;

procedure test;

implementation

procedure test;
var
  a, b, c: TEEMartix;
begin
  a := TEEMartix.Create(2, 2);
  b := TEEMartix.Create(2, 2);
  a.setElemntes([9, 6, 4, 3]);
  a.inv();
  a.writemat('mohmat.txt');
end;

{ TEEMartix }

constructor TEEMartix.Create(r, c: integer);

begin
  Col := c;
  row := r;
  SetLength(A, r * c);
end;

procedure TEEMartix.setc(r, c: integer; v: extended);
begin
  A[r * Col + c] := v;
end;

function TEEMartix.getc(r, c: integer): extended;
begin
  Result := A[r * Col + c];
end;

procedure TEEMartix.transpos(tr: TEEMartix);
begin
  if (tr.row = Self.Col) and (tr.Col = Self.row) then
  begin
    omvtrm(Self.A[0], row, Col, Col, tr.A[0], tr.Col);
  end
  else
    raise Exception.Create('wrong dimention of transpos');
end;

procedure TEEMartix.inv();
var
  term: ArbInt;
begin
  if (Self.row = Self.Col) then
  begin
    invgen(Self.Col, Self.Col, Self.A[0], term);
    if term <> 1 then
      raise Exception.Create('failed to inv ' + term.ToString());
  end
  else
    raise Exception.Create('wrong dimention of transpos');
end;

procedure TEEMartix.multwith(B, C: TEEMartix);
begin
  if (Self.Col = B.row) and (C.row = Self.row) and (c.Col = b.Col) then
  begin
    omvmmm(Self.A[0], row, Col, Col, B.A[0], B.Col, B.Col, C.A[0], C.Col);
  end
  else
    raise Exception.Create('wrong dimention of mult');
end;

procedure TEEMartix.Solve(B, C: TEEMartix);
var
  ca: extended;
  term: integer;
begin
  if not ((Self.Col = B.row) and (C.row = Self.row) and (c.Col = b.Col)) then
    raise Exception.Create('wrong dimention of Solving');
  slegen(Self.row, self.Col, Self.A[0], B.A[0], C.A[0], ca, term);
  if term <> 1 then
    raise Exception.Create('failed to solve');
end;

procedure TEEMartix.writematscilab(Filename: string);
var
  k, m: integer;
  F: TextFile;
  comma: string;
begin
  AssignFile(F, Filename);
  Rewrite(f);
  Write(f, 'a=[');
  for k := 0 to Self.row - 1 do
  begin
    comma := '';
    for m := 0 to Self.Col - 1 do
    begin
      Write(f, comma, Self.A[k * Self.Col + m]);
      comma := ',';
    end;
    Write(F, ';');
  end;
  CloseFile(f);
end;

procedure TEEMartix.writemat(Filename: string);
var
  k, m: integer;
  F: TextFile;
begin
  AssignFile(F, Filename);
  Rewrite(f);
  for k := 0 to Self.row - 1 do
  begin
    for m := 0 to Self.Col - 1 do
      Write(f, ' ', Self.A[k * Self.Col + m]);
    WriteLn(F, '');
  end;
  CloseFile(f);
end;

procedure TEEMartix.setElemntes(E: array of extended);
var
  k: integer;
begin
  for k := Low(e) to High(E) do
    Self.A[k] := e[k];
end;

procedure TEEMartix.multwith(ml: extended);
var
  k: integer;
begin
  for k := 0 to Self.row * Self.Col - 1 do
    Self.A[k] := Self.A[k] * ml;
end;

function TEEMartix.Max(): extended;
var
  cmax: extended;
  cvalue: extended;
  k, m: integer;
begin
  cmax := 1e-6;
  for k := 0 to Self.row - 1 do
  begin
    for m := 0 to Self.Col - 1 do
    begin
      cvalue := abs(Self.A[k * Self.Col + m]);
      if cvalue > cmax then
        cmax := cvalue;
    end;
  end;
  Result := cmax;
end;

procedure TEEMartix.print();
var
  k, m: integer;
  comma: string;
begin
  for k := 0 to Self.row - 1 do
  begin
    comma := '';
    for m := 0 to Self.Col - 1 do
    begin
      Write(comma, FormatFloat('0.00', Self.A[k * Self.Col + m]));
      comma := ',';
    end;
    WriteLn(';');
  end;
end;

procedure TEEMartix.fill(V: PDouble);
var
  k, m: integer;
begin
  for k := 0 to Self.row - 1 do
  begin
    for m := 0 to Self.Col - 1 do
      cell[k, m] := V[k * Col + m];
  end;
end;

function TEEMartix.MaxDaig: extended;
var
  k: integer;
  cMaxDaig: extended;
begin
  if Self.row <> Self.Col then
    raise Exception.Create('only implemented for squre elemnt');
  cMaxDaig := 1e-6;
  for k := 0 to Self.row - 1 do
  begin
    if cell[k, k] > cMaxDaig then
      cMaxDaig := cell[k, k];
  end;
  Result := cMaxDaig;
end;

procedure TEEMartix.fill(B: TEEMartix);
var
  k, m: integer;
begin
  if (row <> B.row) or (Col <> b.Col) then
    raise Exception.Create('not equal dimention');
  for k := 0 to Self.row - 1 do
  begin
    for m := 0 to Self.Col - 1 do
      cell[k, m] := B.cell[k, m];
  end;
end;

procedure TEEMartix.sum(te: TEEMartix);
var
  k, m: integer;
begin
  if (te.row <> row) or (te.Col <> Col) then
    raise Exception.Create('matrix must have equel dimention');
  for k := 0 to Self.row - 1 do
  begin
    for m := 0 to Self.Col - 1 do
      cell[k, m] := te.cell[k, m] + cell[k, m];
  end;
end;

end.
