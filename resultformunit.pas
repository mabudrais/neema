unit resultformunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  WritlnUnit, UtilUnit, ElectricElentUnit,
  StdCtrls, GenircDataUnit, multibusunit, ucomplex, PiInterfaceUnit,
  busCalculatedVoltageintefaceunit;

type

  { TResultForm }

  TResultForm = class(TForm)
    SaveButton: TButton;
    SaveDialog1: TSaveDialog;
    SearchEdit: TEdit;
    PCheckBox: TCheckBox;
    QCheckBox: TCheckBox;
    VCheckBox: TCheckBox;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure PCheckBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    EL: TIntegerObjectMap;
    procedure AddPi(cPI: IPI);
    procedure AG(N, T, M, V: string; G: TStringGrid);
    procedure Update;
  public
    procedure Update(aEL: TIntegerObjectMap);
  end;

var
  ResultForm: TResultForm;

implementation

{$R *.lfm}

{ TResultForm }

procedure TResultForm.SearchEditChange(Sender: TObject);
begin
  Update;
end;

procedure TResultForm.PCheckBoxChange(Sender: TObject);
begin
  Update;
end;

procedure TResultForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    StringGrid1.SaveToCSVFile(SaveDialog1.FileName);
end;

procedure TResultForm.Update(aEL: TIntegerObjectMap);
begin
  EL := aEL;
  Update;
end;

procedure TResultForm.AG(N, T, M, V: string; G: TStringGrid);
begin
  G.RowCount := G.RowCount + 1;
  G.Cells[0, G.RowCount - 1] := N;
  G.Cells[1, G.RowCount - 1] := T;
  G.Cells[2, G.RowCount - 1] := M;
  G.Cells[3, G.RowCount - 1] := V;
end;

procedure TResultForm.Update;
var
  O: TObject;
  k: integer;
  Bus: IBCVI;
  V: complex;
begin
  StringGrid1.RowCount := 1;
  for  O in el do
  begin
    if o is IPI then
      addpi(o as IPI)
    else if o is IBCVI then
    begin
      if not VCheckBox.Checked then Continue;
      Bus := o as IBCVI;
      for k := 0 to 1 do
      begin
        if Bus.GCVByIndex(k, V) then
          AG(Bus.GIName(), 'bus' + IntToStr(k + 1), 'V', VoltToString(
            cmod(V)), StringGrid1);
      end;
    end;
  end;
end;

procedure TResultForm.AddPi(cPI: IPI);
var
  Bus1ID, Bus2ID, PIID: integer;
  Bus1, Bus2: IBCVI;
  V1, V2, I1, I2, S12, S21: complex;
  aName, aClassName: string;
  aE: TElicment;
begin
  aE := cPI.PISelf() as TElicment;
  PIID := aE.ID;
  aClassName := ae.ClassName;
  aName := aE.Name;
  cPI.BussIDs(Bus1ID, Bus2ID);
  Bus1 := el[Bus1ID] as IBCVI;
  Bus2 := el[Bus2ID] as IBCVI;
  GetALLM(cpi, Bus1.GCVPerUnit(PIID), Bus2.GCVPerUnit(PIID), V1,
    V2, I1, I2, S12, S21);
  if PCheckBox.Checked then
    AG(aName, aClassName, 'P1', WatToString(S12.re * StringToVA(aE.BaseVA)),
      StringGrid1);
  if PCheckBox.Checked then
    AG(aName, aClassName, 'P2', WatToString(S21.re * StringToVA(aE.BaseVA)),
      StringGrid1);
  if QCheckBox.Checked then
    AG(aName, aClassName, 'Q1', VarToString(S12.im * StringToVA(aE.BaseVA)),
      StringGrid1);
  if QCheckBox.Checked then
    AG(aName, aClassName, 'Q2', VarToString(S21.im * StringToVA(aE.BaseVA)),
      StringGrid1);
end;

end.
