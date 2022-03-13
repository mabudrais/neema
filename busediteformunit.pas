unit BusEditeFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Grids, EditingFormIUnit, UtilUnit, ElementNamingUnit;

type

  { TBusEditeForm }

  TBusEditeForm = class(TForm, IEditingForm)
    BaseVoltEdit: TEdit;
    BusTypeComboBox: TComboBox;
    BASEMVAEdit: TEdit;
    BusVoltageREdit: TEdit;
    BusVoltageIEdit: TEdit;
    SESaveButton: TButton;
    MeasureStringGrid: TStringGrid;
    NameEdit: TEdit;
    BaseMVALabel: TLabel;
    NameLabel: TLabel;
    BaseVoltLabel: TLabel;
    BusTypeLabel: TLabel;
    VoltRealLabel: TLabel;
    VoltImageLabel: TLabel;
    WatLabel: TLabel;
    VarLabel: TLabel;
    MaxVarLabel: TLabel;
    MinVarLabel: TLabel;
    PageControl1: TPageControl;
    SaveButton: TButton;
    MaxVAREdit: TEdit;
    MinVAREdit: TEdit;
    BasicDataTabSheet: TTabSheet;
    SETabSheet: TTabSheet;
    WEdit: TEdit;
    VAREdit: TEdit;
    procedure BusTypeComboBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SESaveButtonClick(Sender: TObject);
  private
    Bus: TObject;
    ElementNaming: IElementNaming;
    procedure UpdateFromBus;
  public
    function SetElement(E: TObject; aElementNaming: IElementNaming): TForm;

  end;

var
  BusEditeForm: TBusEditeForm;

implementation

uses BusUnit;

{$R *.lfm}

{ TBusEditeForm }

procedure TBusEditeForm.SaveButtonClick(Sender: TObject);
var
  B: TBus;
  Volt, BusWat, BusVar, MaxBusVar, MINBusVar: extended;
  CError: boolean;
begin
  B := Bus as TBus;
  CError := False;
  SPFE(B, 'BaseVA', BASEMVAEdit, 'va', CError, True, 'FSSP');
  SPFE(B, 'BaseVolt', BaseVoltEdit, 'v', CError, True, 'FSSP');
  SPFE(B, 'BusVoltageR', BusVoltageREdit, 'v', CError, True, 'FSSP');
  SPFE(B, 'BusVoltageI', BusVoltageIEdit, 'v', CError, True, 'FSSP');
  SPFE(B, 'InjectedP', WEdit, 'w', CError, True, 'FSSP');
  SPFE(B, 'InjectedQ', VAREdit, 'var', CError, True, 'FSSP');
  if BusTypeComboBox.Text = BusTypeComboBox.Items[0] then
    B.BusType := slackbus
  else if BusTypeComboBox.Text = BusTypeComboBox.Items[1] then
    B.BusType := regulatingbus
  else if BusTypeComboBox.Text = BusTypeComboBox.Items[2] then
    B.BusType := loadbus
  else
  begin
    ShowMessage('wrong bus type');
    Exit;
  end;
  SPFE(B, 'MaxRegulationQ', MaxVAREdit, 'var', CError, True, 'FSSP');
  SPFE(B, 'MinRegulationQ', MINVAREdit, 'var', CError, True, 'FSSP');
  SEN(b, NameEdit.Text, CError, ElementNaming);
  if not CError then
    Close;
end;

procedure TBusEditeForm.SESaveButtonClick(Sender: TObject);
begin
  EditingFormIUnit.UpdateMeasurmentFromGrid(Self.Bus, MeasureStringGrid);
end;

procedure TBusEditeForm.BusTypeComboBoxChange(Sender: TObject);
begin
  MaxVAREdit.Enabled := BusTypeComboBox.Text = BusTypeComboBox.Items[1];
  MinVAREdit.Enabled := BusTypeComboBox.Text = BusTypeComboBox.Items[1];
end;

procedure TBusEditeForm.UpdateFromBus;
var
  B: TBus;
begin
  B := Bus as TBus;
  BASEMVAEdit.Text := b.BaseVA;
  BaseVoltEdit.Text := B.BaseVolt;
  if B.BusType = slackbus then
    BusTypeComboBox.Text := BusTypeComboBox.Items[0]
  else if B.BusType = regulatingbus then
    BusTypeComboBox.Text := BusTypeComboBox.Items[1]
  else if B.BusType = loadbus then
    BusTypeComboBox.Text := BusTypeComboBox.Items[2];
  BusVoltageREdit.Text := B.BusVoltageR;
  BusVoltageIEdit.Text := B.BusVoltageI;
  WEdit.Text := B.InjectedP;
  VAREdit.Text := B.InjectedQ;
  MaxVAREdit.Text := B.MaxRegulationQ;
  MinVAREdit.Text := B.MinRegulationQ;
  NameEdit.Text := B.Name;
  EditingFormIUnit.UpdateGridFromMeasurment(B, MeasureStringGrid);
end;

function TBusEditeForm.SetElement(E: TObject; aElementNaming: IElementNaming): TForm;
begin
  Bus := E;
  ElementNaming := aElementNaming;
  UpdateFromBus;
  Result := Self;
end;

end.
