program neematest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestCase1, NECGridTestUnit, FFunit,
  StateEstimationCase, CartizianNrsolverUnit, JuliaUnit, ShuntUnit,
  ErrorMessagesUnit, TopolgyTestUnit, ObservityUnit, DrawingByCodeUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

