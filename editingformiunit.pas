unit EditingFormIUnit;

{$mode objfpc}{$H+}
{$Interfaces CORBA}
interface

uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs, Controls, Grids, typinfo,
  UtilUnit, StringSplitPatchUnit, SEMeasurementUnit, ElementNamingUnit;

type
  TFormArray = array of ^TForm;

  IEditingForm = interface
    ['{37D842CF-009C-4DE5-B280-B6F7DF0CDB94}']
    function SetElement(E: TObject; aElementNaming: IElementNaming): TForm;
  end;

function GetEditingForm(E: TObject): TForm;

procedure SPFE(O: TObject; Propertyname: string; Edit: TEdit;
  Sufix: string; var Error: boolean; AllowMult: boolean; PropertyType: string;
  AllowEmpty: boolean = False);
procedure SEN(o: TObject; Name: string; var Error: boolean; EN: IElementNaming);
procedure UpdateGridFromMeasurment(aE: TObject; G: TStringGrid);
procedure UpdateMeasurmentFromGrid(aE: TObject; G: TStringGrid);

implementation

uses ElementFactoryUnit, transimissionlineformunit, TransmissionLineUnit,
  transformer2wformunit, ElectricElentUnit
  , TransFormer2WUnit, multibusformunit, multibusunit, transformer3wformunit,
  SVCUnit, SVCFormUnit, ShuntUnit, ShuntFormUnit, TransFormer3WUnit,
  ttransformer3wshuntformunit, TransFormer3WShuntUnit, BusEditeFormUnit,
  BusUnit, NeemaLableFormUnit, LabelUnit, PiImpedanceUnit, PiImpedanceFormUnit;

function GetEditingForm(E: TObject): TForm;
var
  FormsArray: TFormArray;
  ClassArray: TClassArray;
  k: integer;
begin
  Result := nil;
  FormsArray := TFormArray.Create(@BusEditeForm, @TransimissionLineForm,
    @TransFormer2wForm, @MultiBusForm, @Transformer3wForm, @Transformer3wShuntForm,
    @NeemaLableForm, @SVCForm, @ShuntForm, @PiImpedanceForm);
  ClassArray := TClassArray.Create(TBus, TLine, TTransFormer2W,
    TMultiBus, TTransFormer3W, TTransFormer3WShunt, TNeemaLabel, TSVC,
    TShunt, TPiImpedance);
  k := 0;
  while k < Length(ClassArray) do
  begin
    if E is ClassArray[k] then
      Break;
    Inc(k);
  end;
  if k < Length(ClassArray) then
    Result := FormsArray[k]^;
end;
//set properety from edit
procedure SPFE(O: TObject; Propertyname: string; Edit: TEdit;
  Sufix: string; var Error: boolean; AllowMult: boolean; PropertyType: string;
  AllowEmpty: boolean = False);
var
  ValueString: string;
  FValue: extended;
  IValue: integer;
  PropertynameParts: TStringArray;
begin
  //ISIP = integer string to integer property
  //ISSP= Integer string to String propery
  if Error then
    Exit;
  Error := True;
  //UtilUnit.StringToIntElectricValue(Edit.Text,);
  if Propertyname.Contains('.') then
  begin
    PropertynameParts := Propertyname.Split('.');
    Propertyname := PropertynameParts[1];
  end;
  ValueString := Edit.Text;
  ValueString := LowerCase(ValueString);
  if ValueString.Length = 0 then
  begin
    if AllowEmpty then
    begin
      SetStrProp(O, Propertyname, Edit.Text);
      Error := False;
      Exit;
    end
    else
    begin
      Error := True;
      ShowMessage('error ' + Propertyname);
      Edit.SetFocus;
    end;
    Exit;
  end;
  if AllowMult then
  begin
    ValueString := StringReplace((ValueString), 'k' + Sufix, '*1000', []);
    ValueString := StringReplace((ValueString), 'm' + Sufix, '*1000000', []);
  end;
  ValueString := StringReplace((ValueString), Sufix, '*1', []);
  if (PropertyType = 'FSSP') or (PropertyType = 'FSFP') then
    Error := not EvaluateExpression(ValueString, FValue)
  else
  if (PropertyType = 'ISIP') or (PropertyType = 'ISSP') then
    Error := not EvaluateIntExpression(ValueString, IValue)
  else
    Error := True;
  if Error then
  begin
    ShowMessage('error ' + Edit.Text);
    Edit.SetFocus;
    Exit;
  end;
  if (PropertyType = 'FSSP') or (PropertyType = 'ISSP') then
    SetStrProp(O, Propertyname, Edit.Text)
  else if (PropertyType = 'ISIP') then
    SetInt64Prop(O, Propertyname, IValue)
  else if (PropertyType = 'FSFP') then
    SetFloatProp(O, Propertyname, FValue);
end;

procedure SEN(o: TObject; Name: string; var Error: boolean; EN: IElementNaming);
begin
  if GetStrProp(O, 'Name') = Name then Exit;
  if EN.IsPossableName(Name) then
    SetStrProp(O, 'Name', Name)
  else
  begin
    Error := True;
    ShowMessage('wrong Name');
  end;
end;

procedure UpdateGridFromMeasurment(aE: TObject; G: TStringGrid);
var
  E: TElicment;
  M, k: integer;
  D1, D2: TStringArray;
  PNAme: extended;
begin
  if g.ColCount <> 5 then
    raise Exception.Create('grid has wrong colmun count');
  G.RowCount := 1;
  E := aE as TElicment;
  D1 := StringSplitPatchUnit.StringSplitPatch(E.ServerData, [';'],
    TStringSplitOptions.ExcludeEmpty);
  for M := 0 to Length(D1) - 1 do
  begin
    G.RowCount := G.RowCount + 1;
    D2 := StringSplitPatchUnit.StringSplitPatch(D1[M], [':'],
      TStringSplitOptions.ExcludeEmpty);
    if Length(D2) <> (G.ColCount - 1) then//+1 for value
      raise Exception.Create('Server Data String has wrong Element count');
    PNAme := GetFloatProp(E.GetMeasurment(), D2[0]);
    if not IsAm(PNAme) then
      G.Cells[4, m + 1] := 'not avilable'
    else
      G.Cells[4, M + 1] := PNAme.ToString();
    for k := 0 to Length(D2) - 1 do
    begin
      if d2[k] <> EmptyMString then
        G.Cells[k, M + 1] := D2[k];
    end;
    SetLength(D2, 0);
  end;
end;

procedure UpdateMeasurmentFromGrid(aE: TObject; G: TStringGrid);
var
  E: TElicment;
  k: integer;
  M1, P, M2: string;
begin
  if g.ColCount <> 5 then
    raise Exception.Create('grid has wrong colmun count');
  E := aE as TElicment;
  for k := 1 to G.RowCount - 1 do
  begin
    P := G.Cells[0, k];
    M1 := G.Cells[1, k];
    M2 := G.Cells[2, k];
    if (Trim(M1).IsEmpty) or (CanMeasurementBeEvaluted(M1)) then
      E.ServerData := SEMeasurementUnit.UpdateServerdataStringFirstValue(
        E.ServerData, P, M1);
    if (Trim(M2).IsEmpty) or (CanMeasurementBeEvaluted(M2)) then
      E.ServerData := UpdateServerdataStringSecondValue(E.ServerData, P, M2);
  end;
end;

end.
