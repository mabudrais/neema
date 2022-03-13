unit transimissionlineformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ComCtrls, UtilUnit, EditingFormIUnit, UnusedVarUnit, Types, ElementNamingUnit;

type

  { TTransimissionLineForm }

  TTransimissionLineForm = class(TForm, IEditingForm)
    SearchEdit: TEdit;
    SESaveButton: TButton;
    GetFromDBButton: TButton;
    NameEdit: TEdit;
    Label9: TLabel;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    BASEMVA_Edit: TEdit;
    Length_Edit: TEdit;
    R_Edit: TEdit;
    MeasureStringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    SETabSheet: TTabSheet;
    X_Edit: TEdit;
    C_Edit: TEdit;
    RC1_Edit: TEdit;
    RC2_Edit: TEdit;
    CB1_CheckBox: TCheckBox;
    CB2_CheckBox: TCheckBox;
    RC1CB_CheckBox: TCheckBox;
    RC2CB_CheckBox: TCheckBox;
    BaseVoltEdit: TEdit;
    SaveButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure SearchEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GetFromDBButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SESaveButtonClick(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    Line: TObject;
    procedure UpdateForm;
    procedure UpdateLine();
  public
    function SetElement(E: TObject; aElementNaming: IElementNaming): TForm;
  end;

var
  TransimissionLineForm: TTransimissionLineForm;

implementation

uses TransmissionLineUnit;

{$R *.lfm}

{ TTransimissionLineForm }

procedure TTransimissionLineForm.FormCreate(Sender: TObject);
begin
  //setNominalVoltages(ComboBox1.Items);
  StringGrid1.LoadFromCSVFile('data' + PathDelim + 'csvDB' + PathDelim +
    'LINSSCADANAMES2.csv');
end;

procedure TTransimissionLineForm.SearchEditChange(Sender: TObject);
var
  Targets: TStringArray;
  Match, k, MaxMatch, m: integer;
  Cell: string;
begin
  Targets := LowerCase(SearchEdit.Text).Split(' ');
  MaxMatch := 0;
  for k := 1 to StringGrid1.RowCount - 1 do
  begin
    Match := 0;
    Cell := LowerCase(StringGrid1.Cells[1, k]);
    for m := 0 to Length(Targets) - 1 do
    begin
      if (not Targets[m].IsEmpty) and (Cell.Contains(Targets[m])) then
        Inc(Match);
      if Match > MaxMatch then
      begin
        StringGrid1.Row := k;
        MaxMatch := Match;
      end;
    end;
  end;
end;

procedure TTransimissionLineForm.GetFromDBButtonClick(Sender: TObject);
begin
  BASEMVA_Edit.Text := '100 MVA';
  BaseVoltEdit.Text := StringGrid1.Cells[6, StringGrid1.Row] + 'kv';
  Length_Edit.Text := StringGrid1.Cells[4, StringGrid1.Row];
  R_Edit.Text := StringGrid1.Cells[9, StringGrid1.Row];
  X_Edit.Text := StringGrid1.Cells[10, StringGrid1.Row];
  C_Edit.Text := StringGrid1.Cells[17, StringGrid1.Row];
end;

procedure TTransimissionLineForm.UpdateForm;
var
  cLine: TLine;
begin
  cLine := Line as TLine;
  BASEMVA_Edit.Text := cLine.BaseVA;
  BaseVoltEdit.Text := cLine.BaseVolt;
  Length_Edit.Text := cLine.Length;
  R_Edit.Text := cLine.R;
  X_Edit.Text := cLine.x;
  C_Edit.Text := cLine.C;
  RC1_Edit.Text := cLine.RC1;
  RC2_Edit.Text := cLine.RC2;
  CB1_CheckBox.Checked := cLine.CB1Close;
  CB2_CheckBox.Checked := cLine.CB2Close;
  RC1CB_CheckBox.Checked := cLine.RC1Close;
  RC2CB_CheckBox.Checked := cLine.RC2Close;
  NameEdit.Text := cLine.Name;
  EditingFormIUnit.UpdateGridFromMeasurment(cLine, MeasureStringGrid);
end;

procedure TTransimissionLineForm.UpdateLine();
var
  cLine: TLine;
  cError: boolean;
begin
  cLine := Line as TLine;
  cError := False;
  SPFE(cLine, 'BaseVolt', BaseVoltEdit, 'v', cError, True, 'FSSP');
  SPFE(cLine, 'BaseVA', BASEMVA_Edit, 'va', cError, True, 'FSSP');
  SPFE(cLine, 'cLine.Length', Length_Edit, '', cError, False, 'FSSP');
  SPFE(cLine, 'cLine.R', R_Edit, '', cError, False, 'FSSP');
  SPFE(cLine, 'cLine.x', X_Edit, '', cError, False, 'FSSP');
  SPFE(cLine, 'cLine.C', C_Edit, '', cError, False, 'FSSP');
  SPFE(cLine, 'cLine.RC1', RC1_Edit, 'var', cError, True, 'FSSP', True);
  SPFE(cLine, 'cLine.RC2', RC2_Edit, 'var', cError, True, 'FSSP', True);
  cLine.CB1Close := CB1_CheckBox.Checked;
  cLine.CB2Close := CB2_CheckBox.Checked;
  cLine.RC1Close := RC1CB_CheckBox.Checked;
  cLine.RC2Close := RC2CB_CheckBox.Checked;
  SEN(cLine, NameEdit.Text, cError, ElementNaming);
  if not cError then
  begin
    Line := nil;
    Close;
  end;
end;

function TTransimissionLineForm.SetElement(E: TObject;
  aElementNaming: IElementNaming): TForm;
begin
  Line := e;
  ElementNaming := aElementNaming;
  UpdateForm;
  Result := Self;
end;

procedure TTransimissionLineForm.SaveButtonClick(Sender: TObject);
begin
  UpdateLine();
end;

procedure TTransimissionLineForm.SESaveButtonClick(Sender: TObject);
begin
  UpdateMeasurmentFromGrid(Line, MeasureStringGrid);
end;

end.
