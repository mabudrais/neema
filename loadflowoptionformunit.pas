unit LoadFlowOptionFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLoadFlowOptionForm }

  TLoadFlowOptionForm = class(TForm)
    StartTypeComboBox: TComboBox;
    RunButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
  private

  public
    NeemaGui: TObject;
  end;

var
  LoadFlowOptionForm: TLoadFlowOptionForm;

implementation

uses GuiAppUnit;

{$R *.lfm}

{ TLoadFlowOptionForm }

procedure TLoadFlowOptionForm.FormCreate(Sender: TObject);
begin

end;

procedure TLoadFlowOptionForm.RunButtonClick(Sender: TObject);
var
  StartType: string;
begin
  StartType := StartTypeComboBox.Text;
  if StartType = 'FlatStart' then
    (NeemaGui as TNeemaGUI).Solve('StartMode=FlatStart')
  else if StartType = 'CalculatedVoltages' then
    (NeemaGui as TNeemaGUI).Solve('StartMode=CalCulatedVoltge')
  else if StartType = 'EnterdVoltages' then
    (NeemaGui as TNeemaGUI).Solve('');
  Close;
end;

end.
