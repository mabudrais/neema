unit ShuntFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditingFormIUnit,ElementNamingUnit;

type
  { TShuntForm }
  TShuntForm = class(TForm, IEditingForm)
    BASEMVA_Edit: TEdit;
    BaseVoltEdit: TEdit;
    CBCheckBox: TCheckBox;
    NameEdit: TEdit;
    Label4: TLabel;
    VarEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    VarLabel: TLabel;
    SaveButton: TButton;
    procedure SaveButtonClick(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    Shunt: TObject;
    procedure UpdateForm;
  public
    function SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
  end;

var
  ShuntForm: TShuntForm;

implementation

uses ShuntUnit;

{$R *.lfm}

{ TShuntForm }

procedure TShuntForm.SaveButtonClick(Sender: TObject);
var
  SH: TShunt;
  CError: boolean;
begin
  SH := Shunt as TShunt;
  CError := False;
  SPFE(SH, 'BaseVA', BASEMVA_Edit, 'va', CError, True, 'FSSP');
  SPFE(SH, 'BaseVolt', BaseVoltEdit, 'v', CError, True, 'FSSP');
  SPFE(SH, 'ShuntVAR', VarEdit, 'var', CError, True, 'FSSP');
 SEN(SH,NameEdit.Text,CError,ElementNaming);
  SH.CBClose:=CBCheckBox.Checked;
  if not CError then
    Exit;
end;

procedure TShuntForm.UpdateForm;
var
  SH: TShunt;
begin
  SH := Shunt as TShunt;
  BaseVoltEdit.Text := SH.BaseVolt;
  BASEMVA_Edit.Text := Sh.BaseVA;
  VarEdit.Text := SH.ShuntVAR;
  NameEdit.Text := SH.Name;
  CBCheckBox.Checked:=SH.CBClose;
end;

function TShuntForm.SetElement(E: TObject; aElementNaming: IElementNaming
  ): TForm;
begin
  Shunt := E;
  ElementNaming:=aElementNaming;
  Result := Self;
  UpdateForm;
end;

end.
