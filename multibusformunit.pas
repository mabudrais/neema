unit multibusformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Grids, EditingFormIUnit, UtilUnit, BusUnit
  , contnrs,ElementNamingUnit;

type

  { TMultiBusForm }

  TMultiBusForm = class(TForm, IEditingForm)
    BCCheckBox: TCheckBox;
    BusTypeComboBox1: TComboBox;
    BusTypeComboBox2: TComboBox;
    BaseVoltEdit: TEdit;
    BaseVAEdit: TEdit;
    Label8: TLabel;
    SESaveButton: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BaseVoltLabel: TLabel;
    PageControl1: TPageControl;
    MeasureStringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    SETabSheet: TTabSheet;
    VoltIEdit1: TEdit;
    VoltIEdit2: TEdit;
    VoltREdit1: TEdit;
    MinVAREdit1: TEdit;
    MaxVAREdit1: TEdit;
    MaxVAREdit2: TEdit;
    MinVAREdit2: TEdit;
    VAREdit1: TEdit;
    VAREdit2: TEdit;
    VoltREdit2: TEdit;
    WEdit1: TEdit;
    NameEdit: TEdit;
    Label1: TLabel;
    SaveButton: TButton;
    WEdit2: TEdit;
    procedure BusTypeComboBox1Change(Sender: TObject);
    procedure BusTypeComboBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SESaveButtonClick(Sender: TObject);
  private
    ElementNaming: IElementNaming;
    MultiBus: TObject;
    RGList: TObjectList;
    procedure UpdateForm();
    procedure AddIso(const k, NumOfSubBus, SubBusIndex: integer);
    procedure UpdateMutiBus();
  public
    function SetElement(E:TObject;aElementNaming: IElementNaming): TForm;
  end;

var
  MultiBusForm: TMultiBusForm;

implementation

uses multibusunit;

{$R *.lfm}

{ TMultiBusForm }


procedure TMultiBusForm.FormCreate(Sender: TObject);
begin
  RGList := TObjectList.Create(True);
end;

procedure TMultiBusForm.FormDestroy(Sender: TObject);
begin
  RGList.Free;
end;

procedure TMultiBusForm.BusTypeComboBox1Change(Sender: TObject);
begin
  MaxVAREdit1.Enabled := BusTypeComboBox1.Text = BusTypeComboBox1.Items[1];
  MinVAREdit1.Enabled := BusTypeComboBox1.Text = BusTypeComboBox1.Items[1];
end;

procedure TMultiBusForm.BusTypeComboBox2Change(Sender: TObject);
begin
  MaxVAREdit2.Enabled := BusTypeComboBox2.Text = BusTypeComboBox2.Items[1];
  MinVAREdit2.Enabled := BusTypeComboBox2.Text = BusTypeComboBox2.Items[1];
end;

procedure TMultiBusForm.UpdateForm();
var
  MB: TMultiBus;
   k: integer;
begin
  MB := MultiBus as TMultiBus;
  if multibusunit.MaxSubBusNum <> 2 then
    raise Exception.Create('only double bus suported');
  BCCheckBox.Checked := MB.BC12Closed;
  //NumOfSubBus := MB.ActiveSubBusNum();
  k := 1;
  RGList.Free;
  RGList := TObjectList.Create(True);
  for k := 0 to MB.ConectionCount - 1 do
    AddIso(k + 1, MaxSubBusNum, Mb.GetSubBusIndexByIndex(k));
  NameEdit.Text := MB.Name;
  BaseVoltEdit.Text := ((MB.BaseVolt));
  BaseVAEdit.Text:=MB.BaseVA;
  //bus1 data
  VoltREdit1.Text := MB.Bus1VoltageR;
  VoltIEdit1.Text := MB.Bus1VoltageI;
  if MB.Bus1Type = slackbus then
    BusTypeComboBox1.Text := BusTypeComboBox1.Items[0]
  else if MB.Bus1Type = regulatingbus then
    BusTypeComboBox1.Text := BusTypeComboBox1.Items[1]
  else if MB.Bus1Type = loadbus then
    BusTypeComboBox1.Text := BusTypeComboBox1.Items[2];
  WEdit1.Text := MB.Injected1P;
  VAREdit1.Text := MB.Injected1Q;
  MaxVAREdit1.Text := MB.MaxRegulation1Q;
  MinVAREdit1.Text := MB.MinRegulation1Q;
  //bus2 data
  VoltREdit2.Text := MB.Bus2VoltageR;
  VoltIEdit2.Text := MB.Bus2VoltageI;
  if MB.Bus2Type = slackbus then
    BusTypeComboBox2.Text := BusTypeComboBox2.Items[0]
  else if MB.Bus2Type = regulatingbus then
    BusTypeComboBox2.Text := BusTypeComboBox2.Items[1]
  else if MB.Bus2Type = loadbus then
    BusTypeComboBox2.Text := BusTypeComboBox2.Items[2];
  WEdit2.Text := MB.Injected2P;
  VAREdit2.Text := MB.Injected2Q;
  MaxVAREdit2.Text := MB.MaxRegulation2Q;
  MinVAREdit2.Text := MB.MinRegulation2Q;
  BusTypeComboBox1Change(nil);
  BusTypeComboBox2Change(nil);
  EditingFormIUnit.UpdateGridFromMeasurment(MB,MeasureStringGrid);
end;

procedure TMultiBusForm.UpdateMutiBus();
var
  k: integer;
  MB: TMultiBus;
  cError: boolean;
begin
  MB := MultiBus as TMultiBus;
  MB.BC12Closed := BCCheckBox.Checked;
  for k := 0 to RGList.Count - 1 do
  begin
    MB.SetSubBusIndexByIndex(k, (RGList[k] as TRadioGroup).ItemIndex);
  end;
  CError := False;
  SPFE(MB, 'BaseVolt', BaseVoltEdit, 'v', cError, True, 'FSSP');
    SPFE(MB,'MB.BaseVA' , BaseVAEdit, 'va', cError, True, 'FSSP');
  //bus1
  SPFE(MB, 'MB.Bus1VoltageR', VoltREdit1, 'v', CError, True, 'FSSP');
  SPFE(MB, 'MB.Bus1VoltageI', VoltIEdit1, 'v', CError, True, 'FSSP');
  SPFE(MB, 'MB.Injected1P', WEdit1, 'w', CError, True, 'FSSP');
  SPFE(MB, 'MB.Injected1Q', VAREdit1, 'var', CError, True, 'FSSP');
  if BusTypeComboBox1.Text = BusTypeComboBox1.Items[0] then
    MB.Bus1Type := slackbus
  else if BusTypeComboBox1.Text = BusTypeComboBox1.Items[1] then
    MB.Bus1Type := regulatingbus
  else if BusTypeComboBox1.Text = BusTypeComboBox1.Items[2] then
    MB.Bus1Type := loadbus
  else
  begin
    ShowMessage('wrong bus1 type');
    Exit;
  end;
  SPFE(MB, 'MB.MaxRegulation1Q', MaxVAREdit1, 'var', CError, True, 'FSSP');
  SPFE(MB, 'MB.MinRegulation1Q', MinVAREdit1, 'var', CError, True, 'FSSP');
  //bus2 data
  SPFE(MB, 'MB.Bus2VoltageR', VoltREdit2, 'v', CError, True, 'FSSP');
  SPFE(MB, 'MB.Bus2VoltageI', VoltIEdit2, 'v', CError, True, 'FSSP');
  SPFE(MB, 'MB.Injected2P', WEdit2, 'w', CError, True, 'FSSP');
  SPFE(MB, 'MB.Injected2Q', VAREdit2, 'var', CError, True, 'FSSP');
  if BusTypeComboBox2.Text = BusTypeComboBox2.Items[0] then
    MB.Bus2Type := slackbus
  else if BusTypeComboBox2.Text = BusTypeComboBox2.Items[1] then
    MB.Bus2Type := regulatingbus
  else if BusTypeComboBox2.Text = BusTypeComboBox2.Items[2] then
    MB.Bus2Type := loadbus
  else
  begin
    ShowMessage('wrong bus2 type');
    Exit;
  end;
  SPFE(MB, 'MB.MaxRegulation2Q', MaxVAREdit2, 'var', CError, True, 'FSSP');
  SPFE(MB, 'MB.MinRegulation2Q', MinVAREdit2, 'var', CError, True, 'FSSP');
SEN( MB, NameEdit.Text,cError,ElementNaming);
  if not cError then
    Close;
end;

function TMultiBusForm.SetElement(E: TObject; aElementNaming: IElementNaming
  ): TForm;
begin
  MultiBus := E;
  ElementNaming:=aElementNaming;
  UpdateForm();
  Result := Self;
end;

procedure TMultiBusForm.AddIso(const k, NumOfSubBus, SubBusIndex: integer);
var
  RG: TRadioGroup;
  M: integer;
begin
  RG := TRadioGroup.Create(nil);
  RG.Parent := Self.TabSheet1;
  RG.Top := 0;
  RG.Left := k * 50;
  RG.Width := 49;
  RG.Height := 80;
  for M := 0 to NumOfSubBus - 1 do
  begin
    RG.Items.Add(IntToStr(k + 1));
  end;
  RG.ItemIndex := SubBusIndex;
  RGList.Add(RG);
end;

procedure TMultiBusForm.SaveButtonClick(Sender: TObject);
begin
  UpdateMutiBus();
  UpdateForm();
  Close;
end;

procedure TMultiBusForm.SESaveButtonClick(Sender: TObject);
begin
    UpdateMeasurmentFromGrid(Self.MultiBus,MeasureStringGrid);
end;

end.
