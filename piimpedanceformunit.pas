unit PiImpedanceFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditingFormIUnit, PiImpedanceUnit, UtilUnit, UComplex,ElementNamingUnit;

type

  { TPiImpedanceForm }

  TPiImpedanceForm = class(TForm, IEditingForm)
    NameEdit: TEdit;
    Label9: TLabel;
    SetAButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SaveButton: TButton;
    VBaseEdit: TEdit;
    VABaseEdit: TEdit;
    R1Edit: TEdit;
    X1Edit: TEdit;
    R2Edit: TEdit;
    X2Edit: TEdit;
    R3Edit: TEdit;
    X3Edit: TEdit;
    procedure SaveButtonClick(Sender: TObject);
    procedure SetAButtonClick(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    PiImpedance: TObject;
  public
    function SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
  end;

var
  PiImpedanceForm: TPiImpedanceForm;

implementation

{$R *.lfm}

{ TPiImpedanceForm }

procedure TPiImpedanceForm.SaveButtonClick(Sender: TObject);
var
  PIImp: TPiImpedance;
  cError: boolean;
begin
  PIImp := PiImpedance as TPiImpedance;
  cError := False;
  SPFE(PiImpedance, 'PIImp.BaseVolt', VBaseEdit, 'v', cError, False, 'FSSP');
  SPFE(PiImpedance, 'PIImp.BaseVA', VABaseEdit, 'va', cError, False, 'FSSP');

  SPFE(PiImpedance, 'PIImp.ImpedanceR1', R1Edit, '', cError, False, 'FSSP');
  SPFE(PiImpedance, 'PIImp.ImpedanceR2', R2Edit, '', cError, False, 'FSSP');
  SPFE(PiImpedance, 'PIImp.ImpedanceR3', R3Edit, '', cError, False, 'FSSP');

  SPFE(PiImpedance, 'PIImp.ImpedanceX1', X1Edit, '', cError, False, 'FSSP');
  SPFE(PiImpedance, 'PIImp.ImpedanceX2', X2Edit, '', cError, False, 'FSSP');
  SPFE(PiImpedance, 'PIImp.ImpedanceX3', X3Edit, '', cError, False, 'FSSP');
 SEN(PIImp,NameEdit.Text,cError,ElementNaming);
  if not cError then
    Close;
end;

procedure TPiImpedanceForm.SetAButtonClick(Sender: TObject);
var
  StringA: string;
  a, RD, XD: extended;
  Leg1, Leg2: double;
  Oz1, Oz2, Oz3: complex;
begin
  StringA := InputBox('transformer tap', 'enter transformer tap', '1');
  if not TryStrToFloat(StringA, a) then
  begin
    ShowMessage('wrong tap');
    Exit;
  end;
  Leg1 := infto13(a, (a - 1));
  Leg2 := infto13((a * a), (1 - a));
  if (not TryStrToFloat(R2Edit.Text, RD)) or (not TryStrToFloat(X2Edit.Text, XD)) then
  begin
    ShowMessage('wrong impedance');
    Exit;
  end;
  Oz1 := cinit(RD, XD) * Leg1;
  Oz2 := cinit(RD, XD) * a;
  Oz3 := cinit(RD, XD) * Leg2;
  R1Edit.Text := FloatToStr(Oz1.re);
  X1Edit.Text := FloatToStr(Oz1.im);
  R2Edit.Text := FloatToStr(Oz2.re);
  X2Edit.Text := FloatToStr(Oz2.im);
  R3Edit.Text := FloatToStr(Oz3.re);
  X3Edit.Text := FloatToStr(Oz3.im);
end;

function TPiImpedanceForm.SetElement(E: TObject; aElementNaming: IElementNaming
  ): TForm;
var
  PIImp: TPiImpedance;
begin
  PiImpedance := E;
  ElementNaming:=aElementNaming;
  PIImp := PiImpedance as TPiImpedance;
  VABaseEdit.Text := PIImp.BaseVA;
  VBaseEdit.Text := PIImp.BaseVolt;
  R1Edit.Text := PIImp.ImpedanceR1;
  R2Edit.Text := PIImp.ImpedanceR2;
  R3Edit.Text := PIImp.ImpedanceR3;
  X1Edit.Text := PIImp.ImpedanceX1;
  X2Edit.Text := PIImp.ImpedanceX2;
  X3Edit.Text := PIImp.ImpedanceX3;
  NameEdit.Text := PIImp.Name;
  Result := Self;
end;

end.
