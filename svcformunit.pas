unit SVCFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditingFormIUnit, UtilUnit, typinfo,ElementNamingUnit;

type

  { TSVCForm }

  TSVCForm = class(TForm, IEditingForm)
    BASEMVA_Edit: TEdit;
    BaseVoltEdit: TEdit;
    CB1_CheckBox: TCheckBox;
    CB2_CheckBox: TCheckBox;
    NameEdit: TEdit;
    Label9: TLabel;
    WithTransformerCheckBox: TCheckBox;
    V1Edit: TEdit;
    V2Edit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    TapStepEdit: TEdit;
    MaxVAREdit: TEdit;
    MinVarEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MaxTap_Edit: TEdit;
    MinTap_Edit: TEdit;
    NormalTap_Edit: TEdit;
    R_Edit: TEdit;
    SaveButton: TButton;
    TAP_Edit: TEdit;
    X_Edit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure WithTransformerCheckBoxChange(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    SVC: TObject;
    procedure ShowHideTransformerData(HT: boolean);
    procedure UpdateForm;
  public
    function SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
  end;

var
  SVCForm: TSVCForm;

implementation

uses SVCUnit;

{$R *.lfm}

{ TSVCForm }

procedure TSVCForm.FormCreate(Sender: TObject);
begin

end;

procedure TSVCForm.SaveButtonClick(Sender: TObject);
var
  T: TSVC;
  FValue: extended;
  IValue: longint;
  CError: boolean;
begin
  T := SVC as TSVC;
  CError := False;
  SPFE(T, 'BaseMVA', BASEMVA_Edit, 'va', CError, True, 'FSSP');
  SPFE(T, 'BaseVolt', BaseVoltEdit, 'v', CError, True, 'FSSP');
  WithTransformerCheckBox.Checked := T.HasTransformer;
  SPFE(T, 'TransformerPrimeryBaseolt', V1Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'TransformerSecendryBaseVolt', V2Edit, 'v', CError, True, 'FSSP');
  SPFE(T, 'TransformerPerUnitR', R_Edit, '', CError, False, 'FSSP');
  SPFE(T, 'TransformerPerUnitX', X_Edit, '', CError, True, 'FSSP');
  SPFE(T, 'MaxCompensationVar', MaxVAREdit, 'va', CError, True, 'FSSP');
  SPFE(T, 'MinCompensationVar', MinVarEdit, 'va', CError, True, 'FSSP');
  SPFE(T, 'TransformerTap', TAP_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'TransformerMinTap', MinTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'TransformerMedTap', NormalTap_Edit, '', CError, True, 'ISIP');
  SPFE(T, 'TransformerMAXTap', MaxTap_Edit, '', CError, True, 'ISIP');
  T.CB1Close := CB1_CheckBox.Checked;
  // T.CB2Close := CB2_CheckBox.Checked;
  T.CB2Close := CB1_CheckBox.Checked;
 SEN(T,NameEdit.Text,CError,ElementNaming);
  if not CError then
  Exit;
end;

procedure TSVCForm.WithTransformerCheckBoxChange(Sender: TObject);
begin
  (SVC as TSVC).HasTransformer := WithTransformerCheckBox.Checked;
  ShowHideTransformerData((SVC as TSVC).HasTransformer);
end;

procedure TSVCForm.ShowHideTransformerData(HT: boolean);
begin
  V1Edit.Enabled := HT;
  V2Edit.Enabled := HT;
  R_Edit.Enabled := HT;
  X_Edit.Enabled := HT;
  TapStepEdit.Enabled := HT;
  MaxTap_Edit.Enabled := HT;
  MinTap_Edit.Enabled := HT;
  TAP_Edit.Enabled := HT;
  NormalTap_Edit.Enabled := Ht;
end;

procedure TSVCForm.UpdateForm;
var
  T: TSVC;
begin
  T := SVC as TSVC;
  SetStrProp(T, 'MaxCompensationVar', '60');
  BASEMVA_Edit.Text := T.BaseVA;
  BaseVoltEdit.Text := T.BaseVolt;
  WithTransformerCheckBox.Checked := T.HasTransformer;
  ShowHideTransformerData(T.HasTransformer);
  V1Edit.Text := T.TransformerPrimeryBaseolt;
  V2Edit.Text := T.TransformerSecendryBaseVolt;
  R_Edit.Text := T.TransformerPerUnitR;
  X_Edit.Text := T.TransformerPerUnitX;
  MaxVAREdit.Text := T.MaxCompensationVar;
  MinVarEdit.Text := T.MinCompensationVar;
  TAP_Edit.Text := IntToStr(T.TransformerTap);
  MinTap_Edit.Text := IntToStr(T.TransformerMinTap);
  NormalTap_Edit.Text := IntToStr(T.TransformerMedTap);
  MaxTap_Edit.Text := IntToStr(T.TransformerMAXTap);
  TapStepEdit.Text := T.TransformerTapStep;
  CB1_CheckBox.Checked := T.CB1Close;
            CB2_CheckBox.Checked := T.CB2Close;
  NameEdit.Text:=T.Name;
end;

function TSVCForm.SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
begin
  SVC := E;
  ElementNaming:=aElementNaming;
  UpdateForm;
  Result := Self;
end;

end.
