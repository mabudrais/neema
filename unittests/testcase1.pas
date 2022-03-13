unit TestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ConsolAppUnit,
  consolcanvasunit, BusUnit, StateEstimationUnit,
  TransmissionLineUnit, TransFormer2WUnit, TransFormer3WShuntUnit,
  TransFormer3WUnit, multibusunit, CartizianNrsolverUnit
  , FFunit, ImpedanceUnit, SVCUnit, UComplex, JuliaUnit, ShuntUnit;

type

  { TTestCase1 }

  TTestCase1 = class(TTestCase)
  published
    procedure Transformer3winding;
    procedure Transformer3windingSNJTR02;
    procedure MWPMRKTest1;
    procedure MWPMRKTRansformer;
    procedure MWPMRKTMultiBus;
    procedure LineReactor;
    procedure Transformer3windingRc;
    procedure LineCharging;
    procedure PVTest;
    procedure SVCTest;
    procedure ShuntTest;
  end;

implementation

procedure TTestCase1.MWPMRKTest1;
var
  Neema: TNeema;
  MWPBus, MRkBus: TBus;
  MWPMRK1: TLine;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MRkBus := Neema.AddElement(450, 50, TBus) as TBus;
  MRkBus.SetData(500 * 1000, cinit(-500e6, -300e6), 5e5, 100e6, loadbus);
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MRkBus.ID);
  Neema.Solve('');
  AssertEquals(MRkBus.CalculatedVoltage.re, 437236, 100);
  AssertEquals(MRkBus.CalculatedVoltage.im, -90503, 100);
end;
//hadi sadat ex
procedure TTestCase1.PVTest;
var
  Neema: TNeema;
  Bus1, Bus2, Bus3: TBus;
  MWPMRK1: TLine;
  Line12, Line23, Line13: TImpedance;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  Bus1 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus1.SetData(1.05, 0, 1, 1, slackbus);
  Bus1.Name := 'bus1';
  Bus2 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus2.SetData(1, cinit(-2.566, -1.102), 1, 1, loadbus);
  Bus2.Name := 'bus2';
  Bus3 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus3.SetData(1, cinit(-1.386, -0.452), 1, 1, loadbus);
  Bus3.Name := 'bus3';
  Line12 := Neema.AddElement(100, 100, TImpedance) as TImpedance;
  Line12.Impedance := 0.02 + i * 0.04;
  Line13 := Neema.AddElement(100, 100, TImpedance) as TImpedance;
  Line13.Impedance := 0.01 + i * 0.03;
  Line23 := Neema.AddElement(100, 100, TImpedance) as TImpedance;
  Line23.Impedance := 0.0125 + i * 0.025;
  Neema.CDrawing.ConectElements(Bus1.ID, Line12.ID);
  Neema.CDrawing.ConectElements(Bus2.ID, Line12.ID);
  Neema.CDrawing.ConectElements(Bus1.ID, Line13.ID);
  Neema.CDrawing.ConectElements(Bus3.ID, Line13.ID);
  Neema.CDrawing.ConectElements(Bus2.ID, Line23.ID);
  Neema.CDrawing.ConectElements(Bus3.ID, Line23.ID);
  // Neema.CSolver:=TJulia;
  Neema.Solve('');
 { WriteLn(FFunit.FA(Bus2.CalculatedVoltage, 3));
  WriteLn(FFunit.FA(Bus3.CalculatedVoltage, 3));}
  bus2.SetData(1, cinit(-4, -2.5), 1, 1, loadbus);
  bus3.SetData(1.04, cinit(2, 0), 1, 1, regulatingbus);
  Bus3.MaxRegulationQ := '10';
  bus3.MinRegulationQ := '-10';
  Neema.Solve('');
{  WriteLn(FFunit.FA(Bus2.CalculatedVoltage, 3));
  WriteLn(FFunit.FA(Bus3.CalculatedVoltage, 3));}
  AssertEquals(Bus2.CalculatedVoltage.re, 0.97, 0.01);
  AssertEquals(Bus2.CalculatedVoltage.im, -0.04, 0.01);
  AssertEquals(Bus3.CalculatedVoltage.re, 1.04, 0.01);
  AssertEquals(Bus3.CalculatedVoltage.im, -0.009, 0.01);
end;

procedure TTestCase1.MWPMRKTRansformer;
var
  Neema: TNeema;
  MWPBus, MR500Bus, MR220Bus: TBus;
  MWPMRK1: TLine;
  MRKTR01: TTransFormer2W;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MR500Bus := Neema.AddElement(50, 50, TBus) as TBus;
  MR500Bus.SetData(500 * 1000, cinit(0, 0), 5e5, 100e6, loadbus);
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MR500Bus.ID);
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 6, 1, 9, 21);
  MR220Bus := Neema.AddElement(50, 50, TBus) as TBus;
  MR220Bus.SetData(220 * 1000, cinit(-100e6, -50e6), 220 * 1000, 100e6, loadbus);
  Neema.CDrawing.ConectElements(MR500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MR220Bus.ID);
  Neema.Solve('');
end;

procedure TTestCase1.MWPMRKTMultiBus;
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
  MRkMultiBus.SetSubBusIndex(MWPMRK1.ID, 0);

  MWPMRK2 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK2.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK2.ID);
  Neema.CDrawing.ConectElements(MWPMRK2.ID, MRkMultiBus.ID);
  MRkMultiBus.SetSubBusIndex(MWPMRK1.ID, 1);

  MRkMultiBus.BC12Closed := False;
  Neema.Solve('');
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].re, 437236, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].im, -90503, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].re, 535975, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].im, -3868, 100);

  MRkMultiBus.BC12Closed := True;
  Neema.Solve('');
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].re, 496236, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[0].im, -47255, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].re, 496236, 100);
  AssertEquals(MRkMultiBus.CalculatedVoltage[1].im, -47255, 100);
end;

procedure TTestCase1.LineReactor;
var
  Neema: TNeema;
  MWPBus, MRkBus: TBus;
  MWPMRK1: TLine;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MRkBus := Neema.AddElement(50, 50, TBus) as TBus;
  MRkBus.SetData(500 * 1000, cinit(-300e6, -150e6), 5e5, 100e6, loadbus);
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  MWPMRK1.RC1 := '125e6';
  MWPMRK1.RC1Close := True;
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MRkBus.ID);
  Neema.Solve('');
  AssertEquals(MRkBus.CalculatedVoltage.re, 493010, 100);
  AssertEquals(MRkBus.CalculatedVoltage.im, -56562, 100);

  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  Neema.Solve('');
  AssertEquals(MRkBus.CalculatedVoltage.re, 467519, 100);
  AssertEquals(MRkBus.CalculatedVoltage.im, -54179, 100);
end;

procedure TTestCase1.Transformer3windingRc;
var
  Neema: TNeema;
  Bus220, Bus110: TBus;
  RCTrans: TTransFormer3WShunt;
  S1, S2, S3: complex;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  Bus220 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus220.SetData(220e3, 0, 220e3, 100e6, slackbus);
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
  Neema.CDrawing.ConectElements(Bus220.ID, RCTrans.ID);
  Neema.CDrawing.ConectElements(RCTrans.ID, Bus110.ID);
  Neema.Solve('');
  RCTrans.GetSPerUnit(1.0, Bus110.CalculatedVoltage / 110 / 1000, S1, S2, S3);
  //WriteLn(FFunit.F(S2.re,3),' ',FFunit.F(S2.im,3));
  // WriteLn(Bus110.CalculatedVoltage.re, ' ', Bus110.CalculatedVoltage.im);
  AssertEquals(Bus110.CalculatedVoltage.re, 103919, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6846, 100);
  RCTrans.ShuntCB := False;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 105284, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6818, 100);
end;

procedure TTestCase1.Transformer3winding;
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
  Neema.CDrawing.ConectElements(Bus220.ID, Trans.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus110.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus33.ID);
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 109.9e3, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6.8e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 32.7e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, -2.4e3, 100);
  Trans.CB3Close := False;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 111e3, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -4.8e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 0, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, 0, 100);
  Trans.CB2Close := False;
  Trans.CB3Close := True;
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 0, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, 0, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 33.4e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, -1.1e3, 100);
  //WriteLn(FFunit.FA(Bus110.CalculatedVoltage, 3));
  //WriteLn(FFunit.FA(Bus33.CalculatedVoltage, 3));
end;
procedure TTestCase1.Transformer3windingSNJTR02;
var
  Neema: TNeema;
  Bus220, Bus110, Bus33: TBus;
  Trans: TTransFormer3W;
  secw, Tercw, priw: string;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  Bus220 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus220.SetData(220e3, 0, 220e3, 100e6, slackbus);
  Bus110 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus110.SetData(110e3, cinit(-44e6, -14e6), 110e3, 100e6, loadbus);
  Bus33 := Neema.AddElement(50, 50, TBus) as TBus;
  Bus33.SetData(33e3, cinit(-20e6, -8e6), 33e3, 100e6, loadbus);
  Trans := Neema.AddElement(150, 150, TTransFormer3W) as TTransFormer3W;
  priw:=FloatToStr(76.34*100/220/220);
  secw := FloatToStr(-5.94 * 100 / 220 / 220);
  Tercw := FloatToStr(275.37 * 100 / 220 / 220);
    //Z1:=76.34;
   //Z2:=-5.93999999999999926670E+0000;
   //Z3:=275.37;
  Trans.SetData('0',priw, '0', secw, '0', Tercw, '220e3', '110e3', '11e3');
  Trans.SetTapData('-2750', 11, 1, 9, 19);
  Trans.BaseVolt := '220e3';
  Trans.BaseVA := '100e6';
  Neema.CDrawing.ConectElements(Bus220.ID, Trans.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus110.ID);
  Neema.CDrawing.ConectElements(Trans.ID, Bus33.ID);
  Neema.Solve('');
  AssertEquals(Bus110.CalculatedVoltage.re, 109.9e3, 100);
  AssertEquals(Bus110.CalculatedVoltage.im, -6.8e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.re, 32.7e3, 100);
  AssertEquals(Bus33.CalculatedVoltage.im, -2.4e3, 100);
  Trans.CB3Close := False;
  //WriteLn(FFunit.FA(Bus110.CalculatedVoltage, 3));
  //WriteLn(FFunit.FA(Bus33.CalculatedVoltage, 3));
end;

procedure TTestCase1.LineCharging;
var
  Neema: TNeema;
  MWPBus, MRkBus: TBus;
  MWPMRK1: TLine;
  s12, s21: complex;
  V: integer;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MRkBus := Neema.AddElement(50, 50, TBus) as TBus;
  MRkBus.SetData(500 * 1000, cinit(0, 0), 5e5, 100e6, loadbus);
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  MWPMRK1.RC1 := '125e6';
  MWPMRK1.RC1Close := True;
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MRkBus.ID);
  Neema.Solve('');
  //WriteLn('MRK ', cmod(MRkBus.CalculatedVoltage));

  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := False;
 { V := 200;
  while v < 550 do
  begin
    MWPBus.SetData(v * 1000, 0, 5e5, 100e6, slackbus);
    Neema.Solve;
    Write('MWP ', FormatFloat('###.##', cmod(MWPBus.CalculatedVoltage) / 1000), 'KV');
    Write(' MRK ', FormatFloat('###.##', cmod(MRkBus.CalculatedVoltage) / 1000), 'KV');
    MWPMRK1.GetSPerUnit(v / 500, MRkBus.CalculatedVoltage / 500 / 1000, s12, s21);
    WriteLn('    MWP ', FormatFloat('###.##', S12.re * 100), 'MW ', FormatFloat('###.##', S12.im * 100), 'MVAR');
    Inc(V, 25);
  end;    }
end;

procedure TTestCase1.SVCTest;
var
  Neema: TNeema;
  ATBBus, PORBus: TBus;
  ATBPOR: TLine;
  SVC: TSVC;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ATBBus := Neema.AddElement(50, 50, TBus) as TBus;
  ATBBus.SetData(229 * 1000, 0, 220 * 1000, 100e6, slackbus);
  PORBus := Neema.AddElement(50, 50, TBus) as TBus;
  PORBus.SetData(220 * 1000, cinit(-88.12e6, -25.1e6), 220 * 1000, 100e6, loadbus);
  ATBPOR := Neema.AddElement(100, 100, TLine) as TLine;
  ATBPOR.setData(0.076, 0.403, 352893.443662739, 448.92, 220 * 1000, 100e6);
  ATBPOR.RC1 := '30e6';
  ATBPOR.RC1Close := True;
  SVC := Neema.AddElement(50, 50, TSVC) as TSVC;
  SVC.SetTransformerData('0', '0.0532', '220000', '33000');
  SVC.SetTransformerTapData('-250', 9, 1, 9, 21);
  SVC.MinCompensationVar := '-20e6';
  SVC.MaxCompensationVar := '20e6';
  SVC.BaseVolt := '220000';
  SVC.BaseVA := '100E6';
  SVC.TargetVolt := '215000';
  Neema.CDrawing.ConectElements(ATBBus.ID, ATBPOR.ID);
  Neema.CDrawing.ConectElements(ATBPOR.ID, PORBus.ID);
  Neema.CDrawing.ConectElements(PORBus.ID, SVC.ID);
  Neema.Solve('');
  //WriteLn('por volt ', FFunit.F(cmod(PORBus.CalculatedVoltage / 1000), 0));
  AssertEquals(cmod(PORBus.CalculatedVoltage), 215000, 1000);
end;

procedure TTestCase1.ShuntTest;
var
  Neema: TNeema;
  MWPBus, MRkBus: TBus;
  MWPMRK1: TLine;
  Shunt: TShunt;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  MWPBus := Neema.AddElement(50, 50, TBus) as TBus;
  MWPBus.SetData(500 * 1000, 0, 5e5, 100e6, slackbus);
  MRkBus := Neema.AddElement(50, 50, TBus) as TBus;
  MRkBus.SetData(500 * 1000, cinit(-300e6, -150e6), 5e5, 100e6, loadbus);
  MWPMRK1 := Neema.AddElement(100, 100, TLine) as TLine;
  Shunt := Neema.AddElement(100, 100, TShunt) as TShunt;
  Shunt.ShuntVAR := '125e6';
  Shunt.BaseVolt := '500e3';
  Shunt.CBClose := False;
  MWPMRK1.setData(0.028, 0.276, 243300.37925842, 346, 500 * 1000, 100e6);
  Neema.CDrawing.ConectElements(MWPBus.ID, MWPMRK1.ID);
  Neema.CDrawing.ConectElements(MWPMRK1.ID, MRkBus.ID);
  Neema.CDrawing.ConectElements(Shunt.ID, MRkBus.ID);
  Neema.Solve('');
  AssertEquals(MRkBus.CalculatedVoltage.re, 493010, 100);
  AssertEquals(MRkBus.CalculatedVoltage.im, -56562, 100);
  Shunt.CBClose := True;
  Neema.Solve('');
  AssertEquals(MRkBus.CalculatedVoltage.re, 467519, 100);
  AssertEquals(MRkBus.CalculatedVoltage.im, -54179, 100);
end;

initialization

  RegisterTest(TTestCase1);
end.
