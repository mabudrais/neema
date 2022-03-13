program neema;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, Unit1, ConsolAppUnit, GuiAppUnit, neemacanvasUnit,
  GuiCanvasUnit, GenircDataUnit, elementUnit, DrawingUnit, BusUnit,
  multibusunit, PersistentObjectListUnit, ElementFactoryUnit,
  LoadFlowSolverUnit, ConectionUnit, UtilUnit, NrsolverUnit, ModifyedSleUnit,
  TransFormer2WUnit, TransFormer3WShuntUnit, WritlnUnit, stateestimationunit,
  TransFormer3WUnit, LoadUnit, transimissionlineformunit, drawingutilunit,
  EditingFormIUnit, transformer2wformunit, multibusformunit,
  transformer3wformunit, ttransformer3wshuntformunit, StringSplitPatchUnit,
  SVCUnit, BusEditeFormUnit, LabelUnit, NeemaLableFormUnit, ElementColoringUnit,
  SVCFormUnit, ShuntFormUnit, UnusedVarUnit, SplashFormUnit, PiImpedanceUnit,
  PiImpedanceFormUnit, NeemaDialogMessageUnit, IeeeImporUnit,
  LoadFlowOptionFormUnit, StateEstimationOptionFormUnit, ObservityUnit,
  resultformunit, busCalculatedvoltageintefaceunit, PiInterfaceUnit,
  ElementNamingUnit, t3wparatoolunit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TTransimissionLineForm, TransimissionLineForm);
  Application.CreateForm(TTransFormer2wForm, TransFormer2wForm);
  Application.CreateForm(TMultiBusForm, MultiBusForm);
  Application.CreateForm(TTransformer3wForm, Transformer3wForm);
  Application.CreateForm(TTransformer3wShuntForm, Transformer3wShuntForm);
  Application.CreateForm(TBusEditeForm, BusEditeForm);
  Application.CreateForm(TNeemaLableForm, NeemaLableForm);
  Application.CreateForm(TSVCForm, SVCForm);
  Application.CreateForm(TShuntForm, ShuntForm);
  Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TPiImpedanceForm, PiImpedanceForm);
  Application.CreateForm(TLoadFlowOptionForm, LoadFlowOptionForm);
  Application.CreateForm(TStateEstimationOptionForm, StateEstimationOptionForm);
  Application.CreateForm(TResultForm, ResultForm);
  Application.CreateForm(TT3wParaToolForm, T3wParaToolForm);
  Application.Run;
end.

