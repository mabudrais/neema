unit NeemaLableFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  EditingFormIUnit, elementUnit, LabelUnit,ElementNamingUnit;

type

  { TNeemaLableForm }

  TNeemaLableForm = class(TForm, IEditingForm)
    SaveButton: TButton;
    ColorBox1: TColorBox;
    TextEdit: TEdit;
    SizeEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure SaveButtonClick(Sender: TObject);
  private
    Element: TObject;
    ElementNaming: IElementNaming;
  public
    function SetElement(E: TObject;aElementNaming: IElementNaming): TForm;
  end;

var
  NeemaLableForm: TNeemaLableForm;

implementation

{$R *.lfm}

{ TNeemaLableForm }

procedure TNeemaLableForm.SaveButtonClick(Sender: TObject);
var
  l: TNeemaLabel;
  TextSize: longint;
begin
  l := Element as TNeemaLabel;
  l.Text := TextEdit.Text;
  if not TryStrToInt(SizeEdit.Text, TextSize) then
  begin
    SizeEdit.Text := IntToStr(l.Size);
    ShowMessage('wrong txt size');
    Exit;
  end;
  l.Size := TextSize;
  l.Colour := ColorBox1.Colors[ColorBox1.ItemIndex];
  Close;
end;

function TNeemaLableForm.SetElement(E: TObject; aElementNaming: IElementNaming
  ): TForm;
var
  l: TNeemaLabel;
  k: integer;
begin
  Element := E;
  ElementNaming:=aElementNaming;
  Result := Self;
  l := Element as TNeemaLabel;
  TextEdit.Text := l.Text;
  SizeEdit.Text := IntToStr(l.Size);
  for k := 0 to ColorBox1.Items.Count - 1 do
  begin
    if ColorBox1.Colors[k] = l.Colour then
      ColorBox1.ItemIndex := k;
  end;
end;

end.

