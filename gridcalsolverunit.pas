unit gridcalsolverunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoadFlowSolverUnit, GenircDataUnit, busunit,
  UComplex, process, dateutils, WritlnUnit, contnrs;

type

  { TGridCal }

  TGridCal = class(TLoadFlowSolver)
    destructor Destroy; override;
    procedure ini(Drawing: TObject; LoadFlowBuss,VariableElements: TObjectList); override;
    //function solve(elelist: TIntegerObjectMap): boolean; override;
    function Step: boolean; override;
  private
    content: TStringList;
    Executed: boolean;
    enrprocess: TProcess;
    CreatingTime: TDateTime;
    procedure A(contt: string);
  end;

implementation



destructor TGridCal.Destroy;
begin
  inherited Destroy;
end;

           {
function TGridCal.solve(elelist: TIntegerObjectMap): boolean;
var
  busNum: integer;
begin
  busNum := ini(elelist);
  while not Step(busNum) do
    Sleep(1000);
  update_vbus(busNum);
end;      }

procedure TGridCal.A(contt: string);
begin
  content.Add(contt);
end;

procedure TGridCal.ini(Drawing: TObject; LoadFlowBuss, VariableElements: TObjectList);
var
  Busnum, pqNum, PvNum, K, refNum, m: integer;
  pqvList: TStringList;
  cy: complex;
begin
  inherited ini(Drawing, LoadFlowBuss,VariableElements);
  Busnum := Length(VBus);
  content := TStringList.Create;
  pqvList := TStringList.Create;
  A('from numpy import zeros,ndarray');
  A('from scipy.sparse import csc_matrix, lil_matrix');
  A('from gridcalinterface import solve123');
  A('');
  A('n=' + Busnum.ToString());
  A('Ybus = lil_matrix((n, n), dtype=complex)');
  A('Ibus = zeros(n, dtype=complex)');
  A('Sbus = zeros(n, dtype=complex)');
  A('Vbus = zeros(n, dtype=complex)');
  pqNum := 0;
  PvNum := 0;
  refNum := 0;
  for K := 0 to Busnum - 1 do
  begin
    A('Vbus[' + k.ToString() + ']=' + FloatToStr(vbus[k].re) + '+1j*' + FloatToStr(vbus[k].im));
    A('Sbus[' + k.ToString() + ']=' + FloatToStr(PQ[k].re) + '+1j*' + FloatToStr(PQ[k].im));
    if bustype[k] = loadbus then
    begin
      pqvList.Add('pq[' + pqNum.ToString() + ']=' + k.ToString());
      Inc(pqNum);
    end
    else if bustype[k] = regulatingbus then
    begin
      pqvList.Add('pv[' + pvNum.ToString() + ']=' + k.ToString());
      Inc(pvNum);
    end
    else if bustype[k] = slackbus then
    begin
      if refNum > 0 then
        raise Exception.Create('only one slack is acceptable');
      pqvList.Add('ref[0]=' + k.ToString());
      Inc(refNum);
    end;
  end;
  A('ref= zeros(1, dtype=int)');
  A('pq= zeros(' + pqNum.ToString() + ', dtype=int)');
  A('pv= zeros(' + pvNum.ToString() + ', dtype=int)');
  content.AddStrings(pqvList);
  for K := 0 to Busnum - 1 do
  begin
    for m := 0 to Busnum - 1 do
    begin
      cy := ybus[k, m];
      if m <> k then
        cy := cy * -1;
      if abs(cmod(cy)) > 1e-5 then
        A('Ybus[' + k.ToString() + ',' + m.tostring() + ']=' + FloatToStr(cy.re) +
          '+1j*' + FloatToStr(cy.im));
    end;
  end;
  FreeAndNil(pqvList);
  A('Ybus=csc_matrix(Ybus)');
  // A('V =IwamotoNR(Ybus, Sbus, Vbus, Ibus, pv, pq, 0.0005,100,True)');
  //A('V =LevenbergMarquardtPF(Ybus, Sbus, Vbus, Ibus, pv, pq, 0.0005)');
  A('V =solve123(Ybus, Sbus, Vbus, Ibus,ref,pq, pv, 0.0005,2)');
  A('f = open(''gridcalput.txt'', ''w'')');
  A('k=0');
  A('f.write(str(V[0])+''\n'')');
  A('while k<n:');
  A('    f.write(''r,''+str(k)+'',''+str(V[1][k].real)+''\n'')');
  A('    f.write(''i,''+str(k)+'',''+str(V[1][k].imag)+''\n'')');
  A('    k=k+1');
  content.SaveToFile('G:\dev\temp\powerflow2\pysolver\pysolver2.py');
  content.Free;
  CreatingTime := Now;
end;

function TGridCal.Step: boolean;
var
  ResultFile: TStringList;
  t1: TDateTime;
  k: integer;
  Cline: TStringArray;
  cbusindex: longint;
  cbusValue: double;
  oldV, oldDEl: real;
  Converg: boolean;
begin
  Result := False;
  if not Executed then
  begin
    DeleteFile('gridcalput.txt');
    enrprocess := TProcess.Create(nil);
    enrprocess.Executable :=
      'C:\GridCal\python\python.exe';
    enrprocess.Parameters.Add('G:\dev\temp\powerflow2\pysolver\pysolver2.py');
    enrprocess.Execute;
    //enrprocess.Free;
    Executed := True;
    Exit(False);
  end;
  Sleep(1000);
  //SetMemoText('ExitStatus '+enrprocess.ExitStatus.ToString()) ;
  if (FileExists('gridcalput.txt')) and (enrprocess.ExitStatus = 0) then
  begin
    Result := True;
    ResultFile := TStringList.Create;
    ResultFile.LoadFromFile('gridcalput.txt');
    Converg := StrToBool(ResultFile[0]);
    if not Converg then
    begin
      WL('diverge, solution fail');
      Exit(True);
    end;
    for k := 1 to ResultFile.Count - 1 do
    begin
      Cline := ResultFile[k].Split(',', TStringSplitOptions.ExcludeEmpty);
      if Length(Cline) <> 3 then
      begin
        WL('wrong Newton Raphson file, solution fail');
        Exit(True);
      end;
      if not TryStrToInt(Cline[1], cbusindex) then
      begin
        WL('wrong bus index Newton Raphson file, solution fail');
        Exit(True);
      end;
      if not TryStrToFloat(Cline[2], cbusValue) then
      begin
        WL('wrong bus value Newton Raphson file, solution fail');
        Exit(True);
      end;
      if Cline[0] = 'r' then
        vbus[cbusindex].re := cbusValue
      else if Cline[0] = 'i' then
        vbus[cbusindex].im := cbusValue;
    end;
  end;
end;

end.
