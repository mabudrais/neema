
unit transformer2wformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  UtilUnit, EditingFormIUnit, Grids, SEMeasurementUnit,ElementNamingUnit;

type

  { TTransFormer2wForm }

  TTransFormer2wForm = class(TForm, IEditingForm)
    SESaveButton: TButton;
    Label12: TLabel;
    MeasureStringGrid: TStringGrid;
    NameEdit: TEdit;
    Label11: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    SETabSheet: TTabSheet;
    TAPStep_Edit1: TEdit;
    V1Edit: TEdit;
    V2Edit: TEdit;
    Label1: TLabel;
    BaseVoltEdit: TEdit;
    Label10: TLabel;
    Label2: TLabel;
    BASEMVA_Edit: TEdit;
    Label3: TLabel;
    Label9: TLabel;
    R_Edit: TEdit;
    Label4: TLabel;
    X_Edit: TEdit;
    Label5: TLabel;
    TAP_Edit: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MinTap_Edit: TEdit;
    NormalTap_Edit: TEdit;
    MaxTap_Edit: TEdit;
    CB1_CheckBox: TCheckBox;
    CB2_CheckBox: TCheckBox;
    SaveButton: TButton;
    procedure SaveButtonClick(Sender: TObject);
    procedure SESaveButtonClick(Sender: TObject);
  private
    TransFormer: TObject;
    ElementNaming:IElementNaming;
    procedure UpdateForm;
  public
    function SetElement(E: TObject;aElementNaming:IElementNaming): TForm;
  end;

var
  TransFormer2wForm: TTransFormer2wForm;

implementation

uses TransFormer2WUnit;

{$R *.lfm}

{ TTransFormer2wForm }

function TTransFormer2wForm.SetElement(E: TObject;
  aElementNaming: IElementNaming): TForm;
begin
  TransFormer := e;
  ElementNaming:=aElementNaming;
  UpdateForm;
  Result := Self;
end;

procedure TTransFormer2wForm.UpdateForm;
var
  T: TTransFormer2W;
begin
  T := TransFormer as TTransFormer2W;
  BASEMVA_Edit.Text := T.BaseVA;
  BaseVoltEdit.Text := T.BaseVolt;
  V1Edit.Text := T.PrimeryBaseolt;
  V2Edit.Text := T.SecendryBaseVolt;
  R_Edit.Text := T.PerUnitR;
  X_Edit.Text := T.PerUnitX;
  TAP_Edit.Text := IntToStr(T.Tap);
  MinTap_Edit.Text := IntToStr(T.MinTap);
  NormalTap_Edit.Text := IntToStr(T.MedTap);
  MaxTap_Edit.Text := IntToStr(T.MAXTap);
  TAPStep_Edit1.Text := T.TapStep;
  CB1_CheckBox.Checked := T.CB1Close;
  CB2_CheckBox.Checked := T.CB2Close;
  NameEdit.Text := T.Name;
  EditingFormIUnit.UpdateGridFromMeasurment(T, MeasureStringGrid);
end;

procedure TTransFormer2wForm.SaveButtonClick(Sender: TObject);
var
  T: TTransFormer2W;
  CError: boolean;
begin
  T := TransFormer as TTransFormer2W;
  CError := False;
  SPFE(T, 'BaseVA', BASEMVA_Edit, 'va', CError, True, 'FSSP');
  SPFE(T, 'BaseVolt', BaseVoltEdit, 'v', CError, True, 'FSSP');
  SPFE(T, 't.PrimeryBaseolt', V1Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'SecendryBaseVolt', V2Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'PerUnitR', R_Edit, '', CError, False, 'FSSP');
  SPFE(T, 'PerUnitX', X_Edit, '', CError, True, 'FSSP');
  SPFE(T, 'Tap', TAP_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MinTap', MinTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MedTap', NormalTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MAXTap', MaxTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'TapStep', TAPStep_Edit1, '', CError, False, 'FSSP');
  T.CB1Close := CB1_CheckBox.Checked;
  // T.CB2Close := CB2_CheckBox.Checked;
  T.CB2Close := CB1_CheckBox.Checked;
 SEN(T,NameEdit.Text,CError,ElementNaming);
  if not CError then
    Close;
end;

procedure TTransFormer2wForm.SESaveButtonClick(Sender: TObject);
begin
  EditingFormIUnit.UpdateMeasurmentFromGrid(Self.TransFormer, MeasureStringGrid);
end;

end.
