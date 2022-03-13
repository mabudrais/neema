unit StateEstimationCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ConsolAppUnit, BusUnit
  , TransmissionLineUnit, consolcanvasunit, UComplex, dateutils,
  StateEstimationUnit, SEMeasurementUnit, FFunit, DrawingByCodeUnit,
  TransFormer3WShuntUnit, TransFormer3WUnit, multibusunit;

type

  { TstateEstimationTest }

  TstateEstimationTest = class(TTestCase)
  published
    procedure Test1;
    procedure Transformer3windingRc;
    procedure Transformer3windingEE;
    procedure ObservablityTest;
    procedure ServerDataSE;
  end;

implementation

procedure TstateEstimationTest.Test1;
var
  Neema: TNeema;
  ATBBus, PORBus: TBus;
  ATBPOR: TLine;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ATBBus := Neema.AddElement(50, 50, TBus) as TBus;
  ATBBus.SetData(229 * 1000, 0, 220 * 1000, 100e6, slackbus);
  ATBBus.M.V := 229 * 1000;
  PORBus := Neema.AddElement(50, 50, TBus) as TBus;
  PORBus.SetData(220 * 1000, 0, 220 * 1000, 100e6, loadbus);
  ATBPOR := Neema.AddElement(100, 100, TLine) as TLine;
  ATBPOR.setData(0.076, 0.403, 352893.443662739, 448.92, 220 * 1000, 100e6);
  ATBPOR.RC1 := '30e6';
  ATBPOR.RC1Close := True;
  ATBPOR.M.p1 := 94e6;
  ATBPOR.M.q1 := 29e6;
  {ATBPOR.M.Measurep2 := -88.12e6;
  ATBPOR.M.Measureq2 := -25.1e6; }
{  PORBus.M.MeasureV1[0] := 221e3;
  PORBus.M.MeasureV1[1] := 220e3;
  PORBus.M.MeasureV1[2] := 221e3;}
  Neema.CDrawing.ConectElements(ATBBus.ID, ATBPOR.ID);
  Neema.CDrawing.ConectElements(ATBPOR.ID, PORBus.ID);
  Neema.CSolver := StateEstimationUnit.TES;
  Neema.Solve('');
  AssertEquals(PORBus.CalculatedVoltage.re, 192800, 100);
  AssertEquals(PORBus.CalculatedVoltage.im, -67300, 100);
end;

procedure TstateEstimationTest.Transformer3windingRc;
var
  Neema: TNeema;
  Bus220, Bus110: TBus;
  RCTrans: TTransFormer3WShunt;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  Bus220 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus220.SetData(220e3, 0, 220e3, 100e6, slackbus);
  Bus220.M.V := 220e3;
  Bus110 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus110.SetData(110e3, cinit(-50e6, -30e6), 110e3, 100e6, loadbus);
  RCTrans := Neema.AddElement(150, 150, TTransFormer3WShunt) as TTransFormer3WShunt;
  //40,20,30 obselet
  RCTrans.SetData('0', '0.0826446', '0', '0.04132231', '0', '0.0619834',
    '220e3', '110e3', '11e3');
  RCTrans.SetTapData('-2.5', 9, 1, 9, 19);
  RCTrans.ShuntCB := True;
  RCTrans.ShuntVA := '15e6';
  RCTrans.BaseVolt := '220e3';
  RCTrans.BaseVA := '100e6';
  RCTrans.M.p2 := -50e6;
  RCTrans.M.q2 := -30e6;
  Neema.CDrawing.ConectElements(Bus220.ID, RCTrans.ID);
  Neema.CDrawing.ConectElements(RCTrans.ID, Bus110.ID);
  Neema.CSolver := StateEstimationUnit.TES;
  Neema.Solve('');
  //WriteLn(Bus110.CalculatedVoltage.re,' ',Bus110.CalculatedVoltage.im);
  AssertEquals(Bus110.CalculatedVoltage.re, 103919, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6846, 100);
  RCTrans.ShuntCB := False;
  Neema.Solve('');
  // WriteLn(Bus110.CalculatedVoltage.re,' ',Bus110.CalculatedVoltage.im);
  AssertEquals(Bus110.CalculatedVoltage.re, 105284, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6818, 100);
end;

procedure TstateEstimationTest.Transformer3windingEE;
var
  Neema: TNeema;
  Bus220, Bus110, Bus33: TBus;
  Trans: TTransFormer3W;
  secw, Tercw: string;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  Bus220 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus220.SetData(220e3, 0, 220e3, 100e6, slackbus);
  Bus220.M.V := 220e3;
  Bus110 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus110.SetData(110e3, cinit(-44e6, -14e6), 110e3, 100e6, loadbus);
  Bus33 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus33.SetData(33e3, cinit(-20e6, -8e6), 33e3, 100e6, loadbus);
  Trans := Neema.AddElement(150, 150, TTransFormer3W) as TTransFormer3W;
  secw := FloatToStr(2.7 * 100 / 220 / 220);
  Tercw := FloatToStr(34.7 * 100 / 220 / 220);
  Trans.SetData('0', '0.09173553', '0', secw, '0', Tercw, '220e3', '110e3', '11e3');
  Trans.SetTapData('-2750', 11, 1, 9, 19);
  Trans.BaseVolt := '220e3';
  Trans.BaseVA := '100e6';
  Trans.M.p1 := 64e6;
  Trans.M.q1 := 28e6;
  Trans.M.p2 := -44e6;
  Trans.M.q2 := -14e6;
  Trans.M.p3 := -20e6;
  Trans.M.q3 := -8e6;
  Neema.CDrawing.ConectElements(Bus220.ID, Trans.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus110.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus33.ID);
  Neema.CSolver := Tes;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 111.1E3, 1000);
  AssertEquals(Bus110.CalculatedVoltage.im, -3.29e3, 1000);
  AssertEquals(Bus33.CalculatedVoltage.re, 33.23e3, 1000);
  AssertEquals(Bus33.CalculatedVoltage.im, -1.29e3, 1000);
  //110 open
  Trans.M.p1 := NAM;
  Trans.M.q1 := NAM;
  Trans.CB3Close := False;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 111e3, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -4.8e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 0, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, 0, 100);
  //33 open
  Trans.CB2Close := False;
  Trans.CB3Close := True;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 0, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, 0, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 33.4e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, -1.1e3, 100);
end;

procedure TstateEstimationTest.ObservablityTest;
var
  MRK220Bus, GAM220Bus, JAS220Bus, MSH220Bus: TMultiBus;
  MRKGAM1, MRKGAM2, GAMJAS1, GAMJAS2, JASMSH1, JASMSH2: TLine;
  Neema: TNeema;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MRK220Bus', 220 * 1000, slackbus, 0, MRK220Bus, Neema);
  MRK220Bus.M.V1 := 220e3;
  ADB('GAM', 220 * 1000, loadbus, 0, GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  MRKGAM1.M.p1 := 54e6;
  MRKGAM1.M.q1 := 103e6;
  ADB('JAS', 220 * 1000, loadbus, 0, JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  GAMJAS1.M.p1 := -44.2e6;
  GAMJAS1.M.q1 := 63e6;
  ADB('MSH', 220 * 1000, loadbus, 0, MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  //JASMSH1.m.Measurep1 := -133.4e6;
  //JASMSH1.m.Measureq1 := -3e6;
  Neema.CSolver := TES;
  Neema.Solve('');
  Neema.IsolatedAllElementsWichIsNotObservableByES();
  Neema.Solve('');
  AssertEquals(cmod(GAM220Bus.CalculatedVoltage[0]) / 1000, 213, 1);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[1]) / 1000, 211, 1);
  AssertEquals(JASMSH1.Dead, True);
end;

procedure TstateEstimationTest.ServerDataSE;
var
  MRK220Bus, GAM220Bus, JAS220Bus, MSH220Bus: TMultiBus;
  MRKGAM1, MRKGAM2, GAMJAS1, GAMJAS2, JASMSH1, JASMSH2: TLine;
  Neema: TNeema;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MRK220Bus', 220 * 1000, slackbus, 0, MRK220Bus, Neema);
  MRK220Bus.M.V1 := 220e3;
  ADB('GAM', 220 * 1000, loadbus, 0, GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  MRKGAM1.ServerData := UpdateServerdataStringFirstValue(
    MRKGAM1.ServerData, 'P1', 'G(''02420007850020'')*1e6');
  MRKGAM1.M.p1 := 54e6;
  MRKGAM1.M.q1 := 103e6;
  ADB('JAS', 220 * 1000, loadbus, 0, JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  GAMJAS1.M.p1 := -44.2e6;
  GAMJAS1.M.q1 := 63e6;
  ADB('MSH', 220 * 1000, loadbus, 0, MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  //JASMSH1.m.Measurep1 := -133.4e6;
  //JASMSH1.m.Measureq1 := -3e6;
  Neema.UpdateMeasurmentFromDataString(EncodeDateTime(2018, 6, 7, 1, 0, 0, 0));
  Neema.CSolver := TES;
  Neema.Solve('');
  Neema.IsolatedAllElementsWichIsNotObservableByES();
  Neema.Solve('');
  AssertEquals(cmod(GAM220Bus.CalculatedVoltage[0]) / 1000, 213, 1);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[1]) / 1000, 211, 1);
  AssertEquals(JASMSH1.Dead, True);
end;

initialization

  RegisterTest(TstateEstimationTest);
end.
