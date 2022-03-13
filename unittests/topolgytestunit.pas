unit TopolgyTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ConsolAppUnit, BusUnit
  , multibusunit, TransmissionLineUnit, consolcanvasunit, UComplex, LoadFlowSolverUnit;

type

  TopolgyTests = class(TTestCase)
  published
    procedure MultiBusBCOpen;
  end;

implementation

procedure TopolgyTests.MultiBusBCOpen;
var
  Neema: TNeema;
  MWPBus: TBus;
  MWPMRK1, MWPMRK2: TLine;
  MRkMultiBus: TMultiBus;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MRkMultiBus := Neema.AddElement(50, 50, TMultiBus) as TMultiBus;
  MRkMultiBus.BaseVA := '100e6';
  MRkMultiBus.BaseVolt := '500e3';
  MRkMultiBus.SetActiveBusData(0, loadbus, 500 * 1000, cinit(-500e6, -300e6),
    'Mrk500Bus1');
  MRkMultiBus.SetActiveBusData(1, loadbus, 500 * 1000, cinit(0, 0), 'Mrk500Bus2');
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MRkMultiBus.ID);
  MRkMultiBus.SetSubBusIndex(MWPMRK1.ID, 1);

  MWPMRK2 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK2.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK2.ID);
  Neema.CDrawing.ConectElements(MWPMRK2.ID, MRkMultiBus.ID);
  MRkMultiBus.SetSubBusIndex(MWPMRK2.ID, 1);
  MRkMultiBus.BC12Closed := False;
  Neema.Solve('');
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].re, 0, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].im, 0, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].re, 535975, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].im, -3868, 100);
  MRkMultiBus.BC12Closed := True;
  Neema.Solve('');
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].re, 496236, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].im, -47255, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].re, 496236, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].im, -47255, 100);
end;



initialization

  RegisterTest(TopolgyTests);
end.

