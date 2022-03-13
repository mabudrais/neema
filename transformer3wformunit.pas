unit transformer3wformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ComCtrls, EditingFormIUnit,ElementNamingUnit;

type

  { TTransformer3wForm transformer3wform }

  TTransformer3wForm = class(TForm, IEditingForm)
    BaseMVA_Edit: TEdit;
    BaseVolt_Edit: TEdit;
    Button1: TButton;
    NameEdit: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    NameLabel: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    MeasureStringGrid: TStringGrid;
    PageControl1: TPageControl;
    R1_Edit: TEdit;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    SETabSheet: TTabSheet;
    TapStep_Edit: TEdit;
    V1Edit: TEdit;
    V2Edit: TEdit;
    V3Edit: TEdit;
    X1_Edit: TEdit;
    R2_Edit: TEdit;
    X2_Edit: TEdit;
    R3_Edit: TEdit;
    X3_Edit: TEdit;
    Tap_Edit: TEdit;
    MinTap_Edit: TEdit;
    NormalTap_Edit: TEdit;
    MaxTap_Edit: TEdit;
    SaveButton: TButton;
    CB1_CheckBox: TCheckBox;
    CB2_CheckBox: TCheckBox;
    CB3_CheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    TransFormer: TObject;
    procedure UpdateForm;
  public
    function SetElement(E: TObject; aElementNaming: IElementNaming): TForm;
  end;

var
  Transformer3wForm: TTransformer3wForm;

implementation

uses TransFormer3WUnit;

{$R *.lfm}

{ TTransformer3wForm }

procedure TTransformer3wForm.FormCreate(Sender: TObject);
begin

end;

procedure TTransformer3wForm.Button1Click(Sender: TObject);
begin
  EditingFormIUnit.UpdateMeasurmentFromGrid(TransFormer,MeasureStringGrid);
end;

procedure TTransformer3wForm.SaveButtonClick(Sender: TObject);
var
  T: TTransFormer3W;
  CError: boolean;
begin
  T := TransFormer as TTransFormer3W;
  CError := False;
  SPFE(T, 'T.BaseVA', BASEMVA_Edit, 'va', CError, True, 'FSSP');
  SPFE(T, 'T.BaseVolt', BaseVolt_Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'T.PrimeryBaseolt', V1Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'T.SecendryBaseVolt', V2Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'TertiaryBaseVolt', V3Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'T.PerUnitPrimeryR', R1_Edit, '', CError, False, 'FSSP');
  SPFE(T, 'T.PerUnitPrimeryX', X1_Edit, '', CError, True, 'FSSP');
  SPFE(T, 'T.PerUnitSecondaryR', R2_Edit, '', CError, False, 'FSSP');
  SPFE(T, 'T.PerUnitSecondaryX', X2_Edit, '', CError, True, 'FSSP');
  SPFE(T, 'T.PerUnitTertiaryR', R3_Edit, '', CError, False, 'FSSP');
  SPFE(T, 'T.PerUnitTertiaryX', X3_Edit, '', CError, True, 'FSSP');
  SPFE(T, 'Tap', TAP_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MinTap', MinTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MedTap', NormalTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'MAXTap', MaxTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'T.TapStep', TapStep_Edit, '', CError, True, 'FSSP');
  T.CB1Close := CB1_CheckBox.Checked;
  T.CB2Close := CB2_CheckBox.Checked;
  T.CB3Close := CB3_CheckBox.Checked;
  SEN(T,NameEdit.Text,CError,ElementNaming);
  if not CError then
    Close;
end;

procedure TTransFormer3wForm.UpdateForm;
var
  T: TTransFormer3W;
begin
  T := TransFormer as TTransFormer3W;
  BASEMVA_Edit.Text := T.BaseVA;
  BaseVolt_Edit.Text := T.BaseVolt;

  V1Edit.Text := T.PrimeryBaseolt;
  V2Edit.Text := T.SecendryBaseVolt;
  V3Edit.Text := T.TertiaryBaseVolt;

  R1_Edit.Text := T.PerUnitPrimeryR;
  X1_Edit.Text := T.PerUnitPrimeryX;

  R2_Edit.Text := T.PerUnitSecondaryR;
  X2_Edit.Text := T.PerUnitSecondaryX;

  R3_Edit.Text := T.PerUnitTertiaryR;
  X3_Edit.Text := T.PerUnitTertiaryX;
  TAP_Edit.Text := IntToStr(T.Tap);
  MinTap_Edit.Text := IntToStr(T.MinTap);
  NormalTap_Edit.Text := IntToStr(T.MedTap);
  MaxTap_Edit.Text := IntToStr(T.MAXTap);
  TapStep_Edit.Text := T.TapStep;
  CB1_CheckBox.Checked := T.CB1Close;
  CB2_CheckBox.Checked := T.CB2Close;
  CB3_CheckBox.Checked := T.CB3Close;
  NameEdit.Text:=T.Name;
  EditingFormIUnit.UpdateGridFromMeasurment(TransFormer,MeasureStringGrid);
end;

function TTransformer3wForm.SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
begin
  TransFormer := e;
  ElementNaming:=aElementNaming;
  UpdateForm;
  Result := Self;
end;

end.
