unit StateEstimationOptionFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker;

type

  { TStateEstimationOptionForm }

  TStateEstimationOptionForm = class(TForm)
    UOEORadioGroup: TRadioGroup;
    RunSEButton: TButton;
    DateTimePicker1: TDateTimePicker;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RunSEButtonClick(Sender: TObject);
  private

  public
    NeemaGui: TObject;
  end;

var
  StateEstimationOptionForm: TStateEstimationOptionForm;

implementation

uses GuiAppUnit;

{$R *.lfm}

{ TStateEstimationOptionForm }

procedure TStateEstimationOptionForm.RunSEButtonClick(Sender: TObject);
var
  NGui: TNeemaGUI;
begin
  //  App.UpdateMeasurmentFromDataString(EncodeDateTime(2018, 6, 7, 1, 0, 0, 0));
  NGui := (NeemaGui as TNeemaGUI);
  NGui.UpdateMeasurmentFromDataString(DateTimePicker1.DateTime);
  NGui.RunStateEstimator();
  if UOEORadioGroup.ItemIndex = 1 then
    NGui.IsolatedAllElementsWichIsNotObservableByES()
  else if UOEORadioGroup.ItemIndex = 2 then
    NGui.CopyActiveDrawing();
  Close;
end;

procedure TStateEstimationOptionForm.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if UOEORadioGroup.ItemIndex<0 then
    UOEORadioGroup.ItemIndex:=0;
end;

end.
