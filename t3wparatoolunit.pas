unit t3wparatoolunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UtilUnit;

type

  { TT3wParaToolForm }

  TT3wParaToolForm = class(TForm)
    BaseVAEdit: TEdit;
    BaseVEdit: TEdit;
    PerUnitGroupBox: TGroupBox;
    BaseVALabel: TLabel;
    BaseVLabel: TLabel;
    X1PerUnitLabel: TLabel;
    X1PerUnitEdit: TEdit;
    X2PerUnitLabel: TLabel;
    X3PerUnitLabel2: TLabel;
    X2PerUnitEdit: TEdit;
    X3PerUnitEdit: TEdit;
    X1Label: TLabel;
    X2Label: TLabel;
    X3Label: TLabel;
    V2Label: TLabel;
    V3Label: TLabel;
    X12PercentLabel: TLabel;
    X13PercentLabel: TLabel;
    X23PercentLabel: TLabel;
    VA12Label: TLabel;
    VA13Label: TLabel;
    VA23Label: TLabel;
    V1Label: TLabel;
    V1Edit: TEdit;
    V2Edit: TEdit;
    V3Edit: TEdit;
    ErrorLabel: TLabel;
    R12Edit: TEdit;
    X1Edit: TEdit;
    X2Edit: TEdit;
    X3Edit: TEdit;
    R13Edit: TEdit;
    R23Edit: TEdit;
    X12Edit: TEdit;
    X13Edit: TEdit;
    X23Edit: TEdit;
    MVA12Edit: TEdit;
    MVA13Edit: TEdit;
    MVA23Edit: TEdit;
    procedure R12EditChange(Sender: TObject);
  private
    function CFSV(Vs: array of extended; Names: array of string): string;
    function ReadTexTedits(out Z12, R23, Z13, Z23, VA12, VA13, VA23,
      V1, V2, V3, aBaseVA, BaseV: extended): string;

  public

  end;

var
  T3wParaToolForm: TT3wParaToolForm;

implementation

{$R *.lfm}
function TT3wParaToolForm.CFSV(Vs: array of extended; Names: array of string): string;
var
  k: integer;
begin
  Result := '';
  if Length(Vs) <> Length(Names) then raise Exception.Create('<>');
  for k := 0 to Length(Vs) - 1 do
  begin
    if Abs(Vs[k]) < 1e-3 then
      Exit(Names[k] + ' is very small');
  end;
end;

{ TT3wParaToolForm }
function TT3wParaToolForm.ReadTexTedits(
  out Z12, R23, Z13, Z23, VA12, VA13, VA23, V1, V2, V3, aBaseVA,
  BaseV: extended): string;
var
  ErrorMsg: string;
begin
  Result := '';
  if not StringToVolt(V1Edit.Text, V1) then
    Exit('V1 rror');
  if not StringToVolt(V2Edit.Text, V2) then
    Exit('V2 error');
  if not StringToVolt(V3Edit.Text, V3) then
    Exit('V3 error');
  //R and X
  {if not TryStrToFloat(R12Edit.Text, R12) then
    ErrorLabel.Caption := 'R12 error';
  if not TryStrToFloat(R13Edit.Text, R13) then
    ErrorLabel.Caption := 'R13 error';
  if not TryStrToFloat(R23Edit.Text, R23) then
    ErrorLabel.Caption := 'R23 error';}
  if not TryStrToFloat(X12Edit.Text, Z12) then
    Exit('X12 error');
  if not TryStrToFloat(X13Edit.Text, Z13) then
    Exit('X13 error');
  if not TryStrToFloat(X23Edit.Text, Z23) then
    Exit('X23 error');
  //VA
  if not StringToVA(MVA12Edit.Text, VA12) then
    Exit('VA12 error');
  if not StringToVA(MVA13Edit.Text, VA13) then
    Exit('VA13 error');
  if not StringToVA(MVA23Edit.Text, VA23) then
    Exit('VA23 error');
  if not StringToVA(BaseVAEdit.Text, aBaseVA) then
    Exit('baseVA error');
  if not StringToVolt(BaseVEdit.Text, BaseV) then
    Exit('base Volt error');
  ErrorMsg := CFSV([V1, V2, V3, aBaseVA, VA12, VA13, VA23],
    ['V1', 'V2', 'V3', 'aBaseVA', 'VA12', 'VA13', 'VA23']);
  if ErrorMsg.Length > 0 then
    Exit(ErrorMsg);
end;

procedure TT3wParaToolForm.R12EditChange(Sender: TObject);
var
  R12, R13, Z12, R23, Z13, Z23, VA12, VA13, VA23, V1, V2, V3, ZBase,
  I12, I13, I23, TR, Z1, Z2PSide, Z3PSide, aBaseVA, BaseV: extended;
  ErrorMessage: string;
begin
  ErrorMessage := ReadTexTedits(Z12, R23, Z13, Z23, VA12, VA13, VA23,
    V1, V2, V3, aBaseVA, BaseV);
  ErrorLabel.Caption := ErrorMessage;
  if ErrorMessage.Length > 0 then Exit;
  I12 := VA12 / V1;
  Z12 := (V1 * Z12 / 100) / I12;

  I13 := VA13 / V1;
  Z13 := (V1 * Z13 / 100) / I13;

  I23 := VA23 / V2;
  Z23 := (V2 * Z23 / 100) / I23;
  TR := V1 / V2;
  //moving Z23 to primery side
  Z23 := Z23 * TR * TR;

  Z1 := 0.5 * (z12 + z13 - z23);
  Z2PSide := 0.5 * (z12 + z23 - Z13);
  Z3PSide := 0.5 * (z13 + z23 - Z12);
  X1Edit.Text := FloatToStr(Z1);
  X2Edit.Text := FloatToStr(Z2PSide);
  X3Edit.Text := FloatToStr(Z3PSide);

  Zbase := BaseV * BaseV / aBaseVA;
  X1PerUnitEdit.Text := FloatToStr(Z1 / ZBase);
  X2PerUnitEdit.Text := FloatToStr(Z2PSide / ZBase);
  X3PerUnitEdit.Text := FloatToStr(Z3PSide / ZBase);
end;

end.
