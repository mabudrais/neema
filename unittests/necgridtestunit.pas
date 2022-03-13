unit NECGridTestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ConsolAppUnit,
  consolcanvasunit, BusUnit, Math, DrawingByCodeUnit,
  TransmissionLineUnit, TransFormer2WUnit, TransFormer3WShuntUnit,
  multibusunit, SolverUnit, CartizianNrsolverUnit, SVCUnit,
  UComplex, FFunit, StateEstimationUnit, NrsolverUnit;

type

  { TNECGridTest }

  TNECGridTest = class(TTestCase)
  private
    procedure setGrid220RingEastSEPos(MWPBus, MRK500Bus, MRK220Bus,
      MHD220Bus, GAM220Bus, JAS220Bus, MSH220Bus, GAD220Bus, RBK220Bus,
      NHS220Bus, MAR220Bus, SNJ220Bus, SNG220Bus, ROS220Bus, RNK220Bus,
      KBA500Bus, KBA220Bus, IBA220Bus, KLX220Bus, SOB220Bus, ATB500Bus,
      ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus, HWT220Bus, GDF220Bus,
      SHK220Bus: TMultiBus);
  published
    procedure Grid220;
    procedure Grid220V2;
    procedure Grid220Ring;
    procedure Grid220RingROSRegulated;
    procedure Grid220MARSVC;
    procedure Grid220RingSE;
    procedure Grid220RingEastSE;
    procedure Grid220RingEastWestRCSE;
    procedure Grid220RingMRKRCOpen;
  end;

implementation

{ TNECGridTest }

procedure TNECGridTest.Grid220;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, RBK220Bus, GAD220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, MSHRBK1, MSHRBK2, JASGAD1, JASGAD2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK2, RBKRNK1: TLine;
  MRKTR01, MRKTR02, MRKTR03: TTransFormer2W;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, cinit(-158e6, -134e6), MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                    MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                    MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                      MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-264.76e6, -134.58e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-194.88e6, -88.26e6), GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.8e6, -89.76e6), JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-14.04e6, -25.62e6), MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(223.7e6{+66.82e6}, -18.98e6{+14e6}),
    RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-81.89e6, 3.58e6), GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-71.3e6, -39.54e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-99.52e6, -84.98e6), MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-37.85e6{+148.6e6*2}, 4.02e6{+16.2e6*2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(146.65e6{+74.5e6*2}, 5.02e6{+16e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(227.8e6{-68.03e6}, -34.7e6{+39.3e6}),
    ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(0.9e6, -7.7e6), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  ROSRNK2.Dead := True;
  // S := Neema.Solve as TDMathSolver;
  Neema.Solve('');
  {S.PowerError();
  WriteLn('mrk220 ', FormatFloat('###.#', cmod(MRK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('jas220 ', FormatFloat('###.#', cmod(jas220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('mar220 ', FormatFloat('###.#', cmod(mar220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ros220 ', FormatFloat('###.#', cmod(ROS220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn();       }
end;

procedure TNECGridTest.Grid220V2;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2: TLine;
  MRKTR02, MRKTR03, MRKTR01: TTransFormer2W;
  PE: complex;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, cinit(-162.03e6, -134.56e6), MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  // RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180)
  // ROSRNK2.Dead := True;
  { S :=}{ as TDMathSolver};
  Neema.Solve('');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
 { WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FormatFloat('###.#', cmod(MRK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('jas220 ', FormatFloat('###.#', cmod(JAS220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('rbk220 ', FormatFloat('###.#', cmod(RBK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
 } AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 1.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 1.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 1.0);
end;

procedure TNECGridTest.Grid220Ring;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;
  PE: complex;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
              MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
              MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
              MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);
  {MRKTR01.CB1Close := False;
  MRKTR01.CB2Close := False;}
  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
              KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  // RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  ADL('GADSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MRkKBA500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);

  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);

  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);

  // ROSRNK2.Dead := True;
  { S :=}{ as TDMathSolver;}
  Neema.Solve('');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
{  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FormatFloat('###.#', cmod(MRK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('jas220 ', FormatFloat('###.#', cmod(JAS220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('rbk220 ', FormatFloat('###.#', cmod(RBK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));                      }
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 1.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 1.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 1.0);
end;

procedure TNECGridTest.Grid220RingMRKRCOpen;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;

  PE, MRK500BusVolt, OldMRK500BusVolt: complex;
  k: integer;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);
  MRKTR01.CB1Close := True;
  MRKTR01.CB2Close := True;
  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                    KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  // RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  ADL('GADSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MRkKBA500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                    ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
                    ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);

  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);

  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);

  // ROSRNK2.Dead := True;
  // Neema.CSolver:= TMarquardtSolver;
  ROS220Bus.Bus1Type := regulatingbus;
  ROS220Bus.Bus1VoltageR := '223000';
  ROS220Bus.Bus1VoltageI := '0';
  ROS220Bus.MaxRegulation1Q := '50e6';
  ROS220Bus.MinRegulation1Q := '-50e6';
  Neema.Solve('');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
{  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(MRK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('rbk220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mar220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));  }
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 1.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 1.0);
  OldMRK500BusVolt := MRK500Bus.CalculatedVoltage[0];
  MWPMRK2.RC2Close := False;
  Neema.Solve('');
  AssertEquals(Sign(cmod(MRK500Bus.CalculatedVoltage[0]) - cmod(OldMRK500BusVolt)), 1);
  AssertEquals(abs(cmod(MRK500Bus.CalculatedVoltage[0]) - cmod(OldMRK500BusVolt)) <
    15000, True);
end;

procedure TNECGridTest.Grid220RingROSRegulated;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;
  PE: complex;
begin
  //strange case increase the var in ROS decrease voltage!! which make the system unsolvable
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);
  {MRKTR01.CB1Close := False;
  MRKTR01.CB2Close := False;}
  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR02.BaseVA := '100MVA';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  // RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  ADL('GADSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MRkKBA500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);

  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);

  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);

  // ROSRNK2.Dead := True;
  // Neema.CSolver := TDMathSolver;
  ROS220Bus.Bus1Type := regulatingbus;
  ROS220Bus.Bus1VoltageR := '223000';
  ROS220Bus.Bus1VoltageI := '0';
  ROS220Bus.MaxRegulation1Q := '500e6';
  ROS220Bus.MinRegulation1Q := '-500e6';
  Neema.Solve('');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
{  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FormatFloat('###.#', cmod(MRK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('jas220 ', FormatFloat('###.#', cmod(JAS220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('rbk220 ', FormatFloat('###.#', cmod(RBK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));}
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 1.0);
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
end;

procedure TNECGridTest.Grid220MARSVC;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;
  PE: complex;
  SVC: TSVC;
begin
  //strange case increase the var in ROS decrease voltage!! which make the system unsolvable
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);
  {MRKTR01.CB1Close := False;
  MRKTR01.CB2Close := False;}
  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 29.5, MRKMHD1, MRKMHD2, Neema);
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  // RBKRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  ADL('GADSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MRkKBA500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);

  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);

  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);

  // ROSRNK2.Dead := True;
  // Neema.CSolver := TDMathSolver;
  ROS220Bus.Bus1Type := regulatingbus;
  ROS220Bus.Bus1VoltageR := '223000';
  ROS220Bus.Bus1VoltageI := '0';
  ROS220Bus.MaxRegulation1Q := '500e6';
  ROS220Bus.MinRegulation1Q := '-500e6';
  SVC := Neema.AddElement(200, 200, TSVC) as TSVC;
  SVC.SetTransformerData('0', '0.0532', '220000', '33000');
  SVC.SetTransformerTapData('-250', 9, 1, 9, 21);
  SVC.MinCompensationVar := '-30e6';
  SVC.MaxCompensationVar := '30e6';
  SVC.BaseVolt := '220000';
  SVC.BaseVA := '100E6';
  SVC.TargetVolt := '200000';
  SVC.CB1Close := False;
  SVC.CB2Close := False;
  Neema.CDrawing.ConectElements(MAR220Bus.ID, SVC.ID);
  Neema.Solve('');
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 1.0);
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204, 1.0);
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();         
  SVC.CB1Close := True;
  SVC.CB2Close := True;
  SVC.VoltEpsilon := '0.002';
  Neema.Solve('');
 { WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FormatFloat('###.#', cmod(MRK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('jas220 ', FormatFloat('###.#', cmod(JAS220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('rbk220 ', FormatFloat('###.#', cmod(RBK220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));   }
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 200, 2);
  SVC.TargetVolt := '209000';
  Neema.Solve('');
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 209, 2.0);
end;

procedure TNECGridTest.Grid220RingSE;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;

  PE: complex;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  MWPBus.M.V1 := 535e3;
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  MWPMRK1.M.p2 := -267e6;
  MWPMRK1.M.q2 := -263e6;
  MWPMRK2.M.p2 := -267e6;
  MWPMRK2.M.q2 := -265e6;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);
  MRKTR01.M.p1 := 133e6;
  MRKTR01.M.q1 := 123e6;
 { MRKTR01.M.p2 := -155e6;
  MRKTR01.M.q2 := -131e6; }

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);

  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  MRKKBA500.M.p1 := 158e6;
  MRKKBA500.M.q1 := 146e6;
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);
  KBATR01.M.p1 := 81e6;
  KBATR01.M.q1 := 86e6;

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 20.5, MRKMHD1, MRKMHD2, Neema);
  MRKMHD1.M.p1 := 133e6;
  MRKMHD1.M.q1 := 66e6;
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  MRKGAM1.M.p1 := 54e6;
  MRKGAM1.M.q1 := 103e6;
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  GAMJAS1.M.p1 := -44.2e6;
  GAMJAS1.M.q1 := 63e6;
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  JASMSH1.M.p1 := -133.4e6;
  JASMSH1.M.q1 := -3e6;
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  MSHRBK1.M.p1 := -143.7e6;
  MSHRBK1.M.q1 := -5.1e6;
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  AddMToL(JASGAD1, 0, 27e6);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  AddMToL(GADNHS1, -40e6, 35.6e6);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  AddMToL(NHSMAR1, -76.3e6, 23.6e6);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  AddMToL(MARSNJ1, -126.4e6, -15.6e6);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  AddMToL(SNJSNG1, -148.5e6, -16.8e6);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  AddMToL(SNGROS1, -74.5e6, -16.3e6);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  RBKRNK2.Dead := True;
  ROSRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  AddMToL(ROSRNK1, 70.3e6, -38.6e6);
  AddMToL(RBKRNK1, -69e6, -14.2e6);
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  AddMToL(IBAKBA1, 82e6, 87e6);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  AddMToL(IBAKLX1, 61.8e6, 48.5e6);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  AddMToL(KLXSOB1, -4.9e6, 17.3e6);
  ADL('GADSOB', 222, GAD220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MWPATB500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.M.p2 := -201e6;
  MWPATB500.M.q2 := -36e6;
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);
  ATBTR01.M.p1 := 103e6;
  ATBTR01.M.q1 := 17e6;
  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ATBSHN1.M.p1 := 31e6;
  ATBSHN1.M.q1 := -0.8e6;
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  SHNFRZ1.M.p1 := 12.5e6;
  SHNFRZ1.M.q1 := 13.7e6;
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);
  GARFRZ1.M.p1 := -7.3e6;
  GARFRZ1.M.q1 := -32.2e6;
  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);

  Neema.CSolver := TES;
  Neema.Solve('');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
 { WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('IBA220 ', FA(IBA220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));}

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  Neema.CSolver := TNRSolver3;
  // SNGROS1.CB1Close:=False;
  Neema.Solve('StartMode=CalCulatedVoltge');
  {WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2)); }
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);

  Neema.CSolver := TNRSolver3;
  Neema.Solve('StartMode=FlatStart;');
 { WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));   }

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  Neema.CSolver := TNRSolver3;
  Neema.Solve('');//stringvolt sadly it fails
  {
   AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);}
  {WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2)); }
  {WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));}
end;

procedure TNECGridTest.setGrid220RingEastSEPos(MWPBus, MRK500Bus,
  MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus, MSH220Bus, GAD220Bus,
  RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus, SNG220Bus, ROS220Bus,
  RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus, KLX220Bus, SOB220Bus,
  ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus, GAR220Bus, HWT220Bus,
  GDF220Bus, SHK220Bus: TMultiBus);
var
  sq: integer;
begin
  sq := 200;
  MRK500Bus.SetXY(100, 100);
  MRK220Bus.SetXY(100, 100 + sq);
  MWPBus.SetXY(100, 100 + 4 * sq);
  GAM220Bus.SetXY(100 + sq * 2, 100);
  KBA500Bus.SetXY(100 + sq * 2, 100 + sq * 2);
  KBA220Bus.SetXY(100 + sq * 2, 100 + sq * 4);
  GAR220Bus.SetXY(100 + sq * 2, 100 + sq * 6);
  SHN220Bus.SetXY(100 + sq * 2, 100 + sq * 8);
  ATB500Bus.SetXY(100 + sq * 2, 100 + sq * 10);
  ATB220Bus.SetXY(100 + sq * 2, 100 + sq * 12);
  JAS220Bus.SetXY(100 + sq * 4, 100 + sq * 0);
  GAD220Bus.SetXY(100 + sq * 4, 100 + sq * 2);
  KLX220Bus.SetXY(100 + sq * 4, 100 + sq * 4);
  IBA220Bus.SetXY(100 + sq * 4, 100 + sq * 6);
  MSH220Bus.SetXY(100 + sq * 6, 100 + sq * 0);
  NHS220Bus.SetXY(100 + sq * 6, 100 + sq * 2);
  RBK220Bus.SetXY(100 + sq * 8, 100 + sq * 0);
  MAR220Bus.SetXY(100 + sq * 8, 100 + sq * 2);
  SNJ220Bus.SetXY(100 + sq * 8, 100 + sq * 4);
  ROS220Bus.SetXY(100 + sq * 10, 100 + sq * 2);
  SNG220Bus.SetXY(100 + sq * 10, 100 + sq * 4);
  HWT220Bus.SetXY(100 + sq * 10, 100 + sq * 6);
  GDF220Bus.SetXY(100 + sq * 10, 100 + sq * 8);
  SHK220Bus.SetXY(100 + sq * 10, 100 + sq * 10);
end;

procedure TNECGridTest.Grid220RingEastSE;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus,
  GAR220Bus, HWT220Bus, GDF220Bus, SHK220Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1, SNGHWT1,
  SNGHWT2, HWTGDF1, HWTGDF2, GDFSHK1, GDFSHK2: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;
  PE: complex;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  MWPBus.M.V1 := 535e3;
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  MWPMRK1.M.p2 := -267e6;
  MWPMRK1.M.q2 := -263e6;
  MWPMRK2.M.p2 := -267e6;
  MWPMRK2.M.q2 := -265e6;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);
  MRKTR01.M.p1 := 133e6;
  MRKTR01.M.q1 := 123e6;
 { MRKTR01.M.p2 := -155e6;
  MRKTR01.M.q2 := -131e6; }

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);

  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  MRKKBA500.M.p1 := 158e6;
  MRKKBA500.M.q1 := 146e6;
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);
  KBATR01.M.p1 := 81e6;
  KBATR01.M.q1 := 86e6;

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 20.5, MRKMHD1, MRKMHD2, Neema);
  MRKMHD1.M.p1 := 133e6;
  MRKMHD1.M.q1 := 66e6;
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  MRKGAM1.M.p1 := 54e6;
  MRKGAM1.M.q1 := 103e6;
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  GAMJAS1.M.p1 := -44.2e6;
  GAMJAS1.M.q1 := 63e6;
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  JASMSH1.M.p1 := -133.4e6;
  JASMSH1.M.q1 := -3e6;
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  MSHRBK1.M.p1 := -143.7e6;
  MSHRBK1.M.q1 := -5.1e6;
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  AddMToL(JASGAD1, 0, 27e6);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  AddMToL(GADNHS1, -40e6, 35.6e6);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  AddMToL(NHSMAR1, -76.3e6, 23.6e6);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  AddMToL(MARSNJ1, -126.4e6, -15.6e6);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  AddMToL(SNJSNG1, -148.5e6, -16.8e6);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  AddMToL(SNGROS1, -74.5e6, -16.3e6);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);
  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  RBKRNK2.Dead := True;
  ROSRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  AddMToL(ROSRNK1, 70.3e6, -38.6e6);
  AddMToL(RBKRNK1, -69e6, -14.2e6);
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  AddMToL(IBAKBA1, 82e6, 87e6);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  AddMToL(IBAKLX1, 61.8e6, 48.5e6);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  AddMToL(KLXSOB1, -4.9e6, 17.3e6);
  ADL('GADSOB', 222, GAD220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MWPATB500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.M.p2 := -201e6;
  MWPATB500.M.q2 := -36e6;
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);
  ATBTR01.M.p1 := 103e6;
  ATBTR01.M.q1 := 17e6;
  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ATBSHN1.M.p1 := 31e6;
  ATBSHN1.M.q1 := -0.8e6;
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  SHNFRZ1.M.p1 := 12.5e6;
  SHNFRZ1.M.q1 := 13.7e6;
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);
  GARFRZ1.M.p1 := -7.3e6;
  GARFRZ1.M.q1 := -32.2e6;
  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);
  //east
  ADB('HWT', 220 * 1000, loadbus, cinit(-6e6, -4e6), HWT220Bus, Neema);
  ADL('SNGHWT', 221, SNG220Bus.ID, HWT220Bus.ID, 90, SNGHWT1, SNGHWT2, Neema);
  SNGHWT1.M.p1 := -88.1e6;
  SNGHWT1.M.q1 := -11.1e6;
  ADB('GDF', 220 * 1000, loadbus, cinit(-6e6, -4e6), GDF220Bus, Neema);
  ADL('HWTGDF', 221, HWT220Bus.ID, GDF220Bus.ID, 90, HWTGDF1, HWTGDF2, Neema);
  HWTGDF2.M.p1 := -89.3e6;
  HWTGDF2.M.q1 := 4.9e6;
  //  when we add shk flat start divarge
  ADB('SHK', 220 * 1000, loadbus, cinit(-6e6, -4e6), SHK220Bus, Neema);
  ADL('GDFSHK', 221, GDF220Bus.ID, SHK220Bus.ID, 75.12, GDFSHK1, GDFSHK2, Neema);
  GDFSHK1.M.p1 := -74.2e6;
  GDFSHK1.M.q1 := 1.7e6;
  setGrid220RingEastSEPos(MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus,
    JAS220Bus, MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus,
    SNJ220Bus, SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus,
    IBA220Bus, KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus,
    FRZ220Bus, GAR220Bus, HWT220Bus, GDF220Bus, SHK220Bus);
  Neema.CSolver := TES;
  Neema.Solve('');
  //Neema.SaveToFile('..\nemfiles\GRid220RingEast.nem');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
 { WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('IBA220 ', FA(IBA220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));}

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);
  Neema.CSolver := TNRSolver3;
  // SNGROS1.CB1Close:=False;//this fail
  //SNGROS1.CB2Close:=False;
  // ROSRNK1.CB1Close:=False;
  // ROSRNK1.CB2Close:=False;
  {ROS220Bus.Bus1Type:=regulatingbus;
  ROS220Bus.Bus1VoltageR:='224000';
  ROS220Bus.MaxRegulation1Q:='5000e6';
    ROS220Bus.MinRegulation1Q:='-5000e6'; }
  //Neema.CSolver:=TCNRSolver;
  Neema.Solve('StartMode=CalCulatedVoltge;');
{  Neema.CDrawing.PrintTransimissinLineData();
  WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));}
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);

  Neema.CSolver := TNRSolver3;
  Neema.Solve('StartMode=FlatStart;');
 { WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);   }
  Neema.CSolver := TNRSolver3;
  Neema.Solve('');//stringvolt
  {WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2)); }
  {WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));}
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);
end;

procedure TNECGridTest.Grid220RingEastWestRCSE;
var
  Neema: TNeema;
  MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus, JAS220Bus,
  MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus, SNJ220Bus,
  SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus, IBA220Bus,
  KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus, FRZ220Bus,
  GAR220Bus, HWT220Bus, GDF220Bus, SHK220Bus, MSH110Bus: TMultiBus;
  MWPMRK1, MWPMRK2, MRKMHD1, MRKMHD2, MRKGAM1, MRKGAM2, GAMJAS1,
  GAMJAS2, JASMSH1, JASMSH2, JASGAD2, JASGAD1, MSHRBK1, MSHRBK2,
  GADNHS1, GADNHS2, NHSMAR1, NHSMAR2, MARSNJ1, MARSNJ2, SNJSNG1,
  SNJSNG2, SNGROS1, SNGROS2, ROSRNK1, ROSRNK2, RBKRNK1, RBKRNK2,
  MRKKBA500, IBAKBA1, IBAKBA2, IBAKLX1, IBAKLX2, KLXSOB1, KLXSOB2,
  GADSOB1, GADSOB2, MWPATB500, ATBSHN1, ATBSHN2, SHNFRZ1, SHNFRZ2,
  GARFRZ1, GARFRZ2, KBAFRZ1, KBAFRZ2, IBAGAR2, IBAGAR1, SNGHWT1,
  SNGHWT2, HWTGDF1, HWTGDF2, GDFSHK1, GDFSHK2: TLine;
  MRKTR02, MRKTR03, MRKTR01, KBATR01, KBATR02, ATBTR01, ATBTR02: TTransFormer2W;
  PE: complex;
  MSHTR01, MSHTR02: TTransFormer3WShunt;
begin
  Neema := TNeema.Create(TConsolCanvas.Create());
  Neema.CreateEmptyDrawing();
  ADB('MWP500Bus', 500 * 1000, slackbus, 0, MWPBus, Neema);
  ADB('MRK500Bus', 500 * 1000, loadbus, {cinit(-162.03e6, -134.56e6) * }0,
    MRK500Bus, Neema);
  MWPBus.Bus1VoltageR := '535000';
  MWPBus.Bus1VoltageI := '0';
  MWPBus.M.V1 := 535e3;
  ADL('MWPMRK', 501, MWPBus.ID, MRK500Bus.ID, 346, MWPMRK1, MWPMRK2, Neema);
  MWPMRK1.RC2 := '125e6';
  MWPMRK1.RC2Close := True;
  MWPMRK2.RC2 := '125e6';
  MWPMRK2.RC2Close := True;
  MWPMRK1.M.p2 := -267e6;
  MWPMRK1.M.q2 := -263e6;
  MWPMRK2.M.p2 := -267e6;
  MWPMRK2.M.q2 := -265e6;
  ADB('MRK220Bus', 220 * 1000, loadbus, 0{+ cinit(-123e6*3,-112e6*3)}, MRK220Bus, Neema);

  MRKTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR01.BaseVA := '100e6';
  MRKTR01.SetData('0', '0.0532', '500000', '220000');
  MRKTR01.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR01.ID);
  Neema.CDrawing.ConectElements(MRKTR01.ID, MRK220Bus.ID);
  MRKTR01.M.p1 := 133e6;
  MRKTR01.M.q1 := 123e6;
 { MRKTR01.M.p2 := -155e6;
  MRKTR01.M.q2 := -131e6; }

  MRKTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR02.BaseVA := '100e6';
  MRKTR02.SetData('0', '0.0532', '500000', '220000');
  MRKTR02.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR02.ID);
  Neema.CDrawing.ConectElements(MRKTR02.ID, MRK220Bus.ID);

  MRKTR03 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  MRKTR03.BaseVA := '100e6';
  MRKTR03.SetData('0', '0.0532', '500000', '220000');
  MRKTR03.SetTapData('-6250', 17, 1, 9, 21);
  Neema.CDrawing.ConectElements(MRK500Bus.ID, MRKTR03.ID);
  Neema.CDrawing.ConectElements(MRKTR03.ID, MRK220Bus.ID);

  ADB('KBA500Bus', 500 * 1000, loadbus, 0, KBA500Bus, Neema);
  AL('MRkKBA500', MRK500Bus.ID, KBA500Bus.ID, 501, 36.8, MRKKBA500, Neema);
  MRKKBA500.M.p1 := 158e6;
  MRKKBA500.M.q1 := 146e6;
  ADB('KBA220Bus', 220 * 1000, loadbus, 0, KBA220Bus, Neema);

  KBATR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR01.BaseVA := '100e6';
  KBATR01.SetData('0', '0.0532', '500000', '220000');
  KBATR01.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR01.ID);
  Neema.CDrawing.ConectElements(KBATR01.ID, KBA220Bus.ID);
  KBATR01.M.p1 := 81e6;
  KBATR01.M.q1 := 86e6;

  KBATR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  KBATR02.BaseVA := '100e6';
  KBATR02.SetData('0', '0.0532', '500000', '220000');
  KBATR02.SetTapData('-6250', 15, 1, 9, 21);
  Neema.CDrawing.ConectElements(KBA500Bus.ID, KBATR02.ID);
  Neema.CDrawing.ConectElements(KBATR02.ID, KBA220Bus.ID);

  ADB('MHD', 220 * 1000, loadbus, cinit(-261.61e6, -136.91e6), MHD220Bus, Neema);
  ADL('MRKMHD', 221, MRK220Bus.ID, MHD220Bus.ID, 20.5, MRKMHD1, MRKMHD2, Neema);
  MRKMHD1.M.p1 := 133e6;
  MRKMHD1.M.q1 := 66e6;
  ADB('GAM', 220 * 1000, loadbus, cinit(-196.96e6 {+ 44.3e6 * 2}, -86.63e6{ - 63e6 * 2}),
    GAM220Bus, Neema);
  ADL('MRKGAM', 221, MRK220Bus.ID, GAM220Bus.ID, 37.968, MRKGAM1, MRKGAM2, Neema);
  MRKGAM1.M.p1 := 54e6;
  MRKGAM1.M.q1 := 103e6;
  ADB('JAS', 220 * 1000, loadbus, cinit(-174.6e6 {+ 263.4e6}, -91.5e6 {- 48.3e6}),
    JAS220Bus, Neema);
  ADL('GAMJAS', 221, GAM220Bus.ID, JAS220Bus.ID, 39.88, GAMJAS1, GAMJAS2, Neema);
  GAMJAS1.M.p1 := -44.2e6;
  GAMJAS1.M.q1 := 63e6;
  ADB('MSH', 220 * 1000, loadbus, cinit(-16.39e6 {+ 284e6}, -25.8e6 {+ 14.62e6}),
    MSH220Bus, Neema);
  ADL('JASMSH', 221, JAS220Bus.ID, MSH220Bus.ID, 147.7, JASMSH1, JASMSH2, Neema);
  JASMSH1.M.p1 := -133.4e6;
  JASMSH1.M.q1 := -3e6;
  ADB('RBK', 220 * 1000, loadbus, cinit(226.33e6, -16.06e6), RBK220Bus, Neema);
  ADL('MSHRBK', 221, MSH220Bus.ID, RBK220Bus.ID, 107.2, MSHRBK1, MSHRBK2, Neema);
  MSHRBK1.M.p1 := -143.7e6;
  MSHRBK1.M.q1 := -5.1e6;
  ADB('GAD', 220 * 1000, loadbus, cinit(-82.64e6{+78.89e6}, 4.59e6{-60e6}),
    GAD220Bus, Neema);
  ADL('JASGAD', 221, JAS220Bus.ID, GAD220Bus.ID, 37, JASGAD1, JASGAD2, Neema);
  AddMToL(JASGAD1, 0, 27e6);
  ADB('NHS', 220 * 1000, loadbus, cinit(-69.08e6, -39.92e6), NHS220Bus, Neema);
  ADL('GADNHS', 222, GAD220Bus.ID, NHS220Bus.ID, 77, GADNHS1, GADNHS2, Neema);
  AddMToL(GADNHS1, -40e6, 35.6e6);
  ADB('MAR', 220 * 1000, loadbus, cinit(-97.93e6 {+ 126e6 * 2}, -83.93e6 {+ 16e6 * 2}),
    MAR220Bus, Neema);
  ADL('NHSMAR', 222, NHS220Bus.ID, MAR220Bus.ID, 63, NHSMAR1, NHSMAR2, Neema);
  AddMToL(NHSMAR1, -76.3e6, 23.6e6);
  ADB('SNJ', 220 * 1000, loadbus, cinit(-39.78e6 {+ 148.6e6 * 2}, 1.51e6{ + 16.2e6 * 2}),
    SNJ220Bus, Neema);
  ADL('MARSNJ', 222, MAR220Bus.ID, SNJ220Bus.ID, 84, MARSNJ1, MARSNJ2, Neema);
  AddMToL(MARSNJ1, -126.4e6, -15.6e6);
  ADB('SNG', 220 * 1000, loadbus, cinit(142.05e6{+74.5e6*2}, 8e6{+19e6*2}),
    SNG220Bus, Neema);
  ADL('SNJSNG', 222, SNJ220Bus.ID, SNG220Bus.ID, 50, SNJSNG1, SNJSNG2, Neema);
  AddMToL(SNJSNG1, -148.5e6, -16.8e6);
  ADB('ROS', 220 * 1000, loadbus, cinit(230.32e6, -35.2e6), ROS220Bus, Neema);
  ADL('SNGROS', 222, SNG220Bus.ID, ROS220Bus.ID, 178, SNGROS1, SNGROS2, Neema);
  AddMToL(SNGROS1, -74.5e6, -16.3e6);
  ADB('RNK', 220 * 1000, loadbus, cinit(876545.52, -7146640.5), RNK220Bus, Neema);
  ADL('ROSRNK', 221, ROS220Bus.ID, RNK220Bus.ID, 172.8, ROSRNK1, ROSRNK2, Neema);
  ADL('RBKRNK', 221, RBK220Bus.ID, RNK220Bus.ID, 163.3, RBKRNK1, RBKRNK2, Neema);

  RBKRNK2.CB1Close := False;
  RBKRNK2.CB2Close := False;
  RBKRNK2.Dead := True;
  ROSRNK2.Dead := True;
  ROSRNK2.CB1Close := False;
  ROSRNK2.CB2Close := False;
  AddMToL(ROSRNK1, 70.3e6, -38.6e6);
  AddMToL(RBKRNK1, -69e6, -14.2e6);
  SetPolarVolt(ROS220Bus, 220 * 1000, 30 * pi / 180);

  //IBA KLX SOB
  ADB('IBA', 220 * 1000, loadbus, cinit(-48 * 3e6{GAR + 53 * 2e5},
    -60 * 3e6{Gar + 53 * 2e6}), IBA220Bus, Neema);
  ADL('IBAKBA', 221, KBA220Bus.ID, IBA220Bus.ID, 37, IBAKBA1, IBAKBA2, Neema);
  AddMToL(IBAKBA1, 82e6, 87e6);
  ADB('KLX', 220 * 1000, loadbus, cinit(-41 * 3e6, -21 * 3e6), KLX220Bus, Neema);
  ADL('IBAKLX', 221, KLX220Bus.ID, IBA220Bus.ID, 14, IBAKLX1, IBAKLX2, Neema);
  AddMToL(IBAKLX1, 61.8e6, 48.5e6);
  ADB('SOB', 220 * 1000, loadbus, cinit(-4 * 3e6, -2 * 3e6), SOB220Bus, Neema);
  ADL('KLXSOB', 222, KLX220Bus.ID, SOB220Bus.ID, 9.5, KLXSOB1, KLXSOB2, Neema);
  AddMToL(KLXSOB1, -4.9e6, 17.3e6);
  ADL('GADSOB', 222, GAD220Bus.ID, SOB220Bus.ID, 32, GADSOB1, GADSOB2, Neema);
  //ATB Path
  ADB('ATB500Bus', 500 * 1000, loadbus, 0, ATB500Bus, Neema);
  AL('MWPATB500', MWPBus.ID, ATB500Bus.ID, 501, 236.7, MWPATB500, Neema);
  MWPATB500.M.p2 := -201e6;
  MWPATB500.M.q2 := -36e6;
  MWPATB500.RC2 := '125e6';
  MWPATB500.RC2Close := True;
  ADB('ATB220Bus', 220 * 1000, loadbus, cinit(-99e6, -26.9e6), ATB220Bus, Neema);

  ATBTR01 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR01.BaseVA := '100e6';
  ATBTR01.SetData('0', '0.0532', '500000', '220000');
  ATBTR01.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR01.ID);
  Neema.CDrawing.ConectElements(ATBTR01.ID, ATB220Bus.ID);

  ATBTR02 := Neema.AddElement(200, 200, TTransFormer2W) as TTransFormer2W;
  ATBTR02.BaseVA := '100e6';
  ATBTR02.SetData('0', '0.0532', '500000', '220000');
  ATBTR02.SetTapData('-6250', 8, 1, 9, 21);
  Neema.CDrawing.ConectElements(ATB500Bus.ID, ATBTR02.ID);
  Neema.CDrawing.ConectElements(ATBTR02.ID, ATB220Bus.ID);
  ATBTR01.M.p1 := 103e6;
  ATBTR01.M.q1 := 17e6;
  ADB('SHN', 220 * 1000, loadbus, cinit(-36e6, -43e6), SHN220Bus, Neema);
  ADL('ATBSHN', 221, ATB220Bus.ID, SHN220Bus.ID, 140, ATBSHN1, ATBSHN2, Neema);
  ATBSHN1.M.p1 := 31e6;
  ATBSHN1.M.q1 := -0.8e6;
  ADB('FRZ', 220 * 1000, loadbus, cinit(-32e6, -36e6), FRZ220Bus, Neema);
  ADL('SHNFRZ', 221, SHN220Bus.ID, FRZ220Bus.ID, 115, SHNFRZ1, SHNFRZ2, Neema);
  SHNFRZ1.M.p1 := 12.5e6;
  SHNFRZ1.M.q1 := 13.7e6;
  ADB('GAR', 220 * 1000, loadbus, cinit(95e6, 30e6), GAR220Bus, Neema);
  ADL('GARFRZ', 221, GAR220Bus.ID, FRZ220Bus.ID, 5, GARFRZ1, GARFRZ2, Neema);
  GARFRZ1.M.p1 := -7.3e6;
  GARFRZ1.M.q1 := -32.2e6;
  //close the ring
  ADL('KBAFRZ', 221, KBA220Bus.ID, FRZ220Bus.ID, 26, KBAFRZ1, KBAFRZ2, Neema);
  ADL('IBAGAR', 221, IBA220Bus.ID, GAR220Bus.ID, 60, IBAGAR1, IBAGAR2, Neema);
  //east
  ADB('HWT', 220 * 1000, loadbus, cinit(-6e6, -4e6), HWT220Bus, Neema);
  ADL('SNGHWT', 221, SNG220Bus.ID, HWT220Bus.ID, 90, SNGHWT1, SNGHWT2, Neema);
  SNGHWT1.M.p1 := -88.1e6;
  SNGHWT1.M.q1 := -11.1e6;
  ADB('GDF', 220 * 1000, loadbus, cinit(-6e6, -4e6), GDF220Bus, Neema);
  ADL('HWTGDF', 221, HWT220Bus.ID, GDF220Bus.ID, 90, HWTGDF1, HWTGDF2, Neema);
  HWTGDF2.M.p1 := -89.3e6;
  HWTGDF2.M.q1 := 4.9e6;
  //  when we add shk flat start divarge
  ADB('SHK', 220 * 1000, loadbus, cinit(-6e6, -4e6), SHK220Bus, Neema);
  ADL('GDFSHK', 221, GDF220Bus.ID, SHK220Bus.ID, 75.12, GDFSHK1, GDFSHK2, Neema);
  GDFSHK1.M.p1 := -74.2e6;
  GDFSHK1.M.q1 := 1.7e6;

  ADB('MSH110', 220 * 1000, loadbus, 0, MSH110Bus, Neema);
  MSHTR01 := Neema.AddElement(200, 200, TTransFormer3WShunt) as TTransFormer3WShunt;
  MSHTR01.SetData('0', '0.183884', '0', '0.00206611', '0', '0.714876033',
    '220000', '110000', '11000');
  MSHTR01.SetTapData('-2250', 6, 1, 9, 21);
  MSHTR01.ShuntCB := False;
  Neema.CDrawing.ConectElements(MSH220Bus.ID, MSHTR01.ID);
  Neema.CDrawing.ConectElements(MSHTR01.ID, MSH110Bus.ID);
  MSHTR01.BaseVA := '100e6';
  MSHTR01.ShuntVA := '13e6';
  MSHTR01.M.p1 := 8.4e6;
  MSHTR01.M.q1 := 10.5e6;

  MSHTR02 := Neema.AddElement(200, 200, TTransFormer3WShunt) as TTransFormer3WShunt;
  MSHTR02.SetData('0', '0.183884', '0', '0.00206611', '0', '0.714876033',
    '220000', '110000', '11000');
  MSHTR02.SetTapData('-2250', 6, 1, 9, 21);
  MSHTR02.ShuntCB := False;
  Neema.CDrawing.ConectElements(MSH220Bus.ID, MSHTR02.ID);
  Neema.CDrawing.ConectElements(MSHTR02.ID, MSH110Bus.ID);
  MSHTR02.BaseVA := '100e6';
  MSHTR02.ShuntVA := '13e6';
  MSHTR02.M.p1 := 8.4e6;
  MSHTR02.M.q1 := 10.5e6;

  setGrid220RingEastSEPos(MWPBus, MRK500Bus, MRK220Bus, MHD220Bus, GAM220Bus,
    JAS220Bus, MSH220Bus, GAD220Bus, RBK220Bus, NHS220Bus, MAR220Bus,
    SNJ220Bus, SNG220Bus, ROS220Bus, RNK220Bus, KBA500Bus, KBA220Bus,
    IBA220Bus, KLX220Bus, SOB220Bus, ATB500Bus, ATB220Bus, SHN220Bus,
    FRZ220Bus, GAR220Bus, HWT220Bus, GDF220Bus, SHK220Bus);
  Neema.CSolver := TES;
  Neema.Solve('');
  //Neema.SaveToFile('..\nemfiles\GRid220RingEast.nem');
  //PE := Neema.PowerError(Neema.Solve);
  // Neema.CDrawing.PrintTransimissinLineData();
  //WriteLn('power error ', pe.re, ' MW ', PE.im, ' MVAR');
  // S.PowerError();
 { WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('IBA220 ', FA(IBA220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));}

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);
  WriteLn('MSH110 ', Fa(MSH110Bus.CalculatedVoltage[0], 2));
  Neema.CSolver := TNRSolver3;
  // SNGROS1.CB1Close:=False;//this fail
  //SNGROS1.CB2Close:=False;
  // ROSRNK1.CB1Close:=False;
  // ROSRNK1.CB2Close:=False;
  {ROS220Bus.Bus1Type:=regulatingbus;
  ROS220Bus.Bus1VoltageR:='224000';
  ROS220Bus.MaxRegulation1Q:='5000e6';
    ROS220Bus.MinRegulation1Q:='-5000e6'; }
  //Neema.CSolver:=TCNRSolver;
  Neema.Solve('StartMode=CalCulatedVoltge;');
{  Neema.CDrawing.PrintTransimissinLineData();
  WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));}
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);

  Neema.CSolver := TNRSolver3;
  Neema.Solve('StartMode=FlatStart;');
 { WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2));

  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);   }
  Neema.CSolver := TNRSolver3;
  Neema.Solve('');//stringvolt
  {WriteLn();
  WriteLn('mrk500 ', FA(MRK500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mrk220 ', FA(Mrk220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('mhd220 ', FA(MHD220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('kba500 ', FA(KBA500Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('gam220 ', FA(GAM220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('jas220 ', FA(JAS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('RBK220 ', FA(RBK220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('MAR220 ', FA(MAR220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('KLX220 ', FA(KLX220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('SHN220 ', FA(SHN220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('GAR220 ', FA(GAR220Bus.CalculatedVoltage[0] / 1000, 2)); }
  {WriteLn('mar220 ', FormatFloat('###.#', cmod(MAR220Bus.CalculatedVoltage[0] / 1000)));
  WriteLn('ROS220 ', FA(ROS220Bus.CalculatedVoltage[0] / 1000, 2));
  WriteLn('ATB500 ', FA(ATB500Bus.CalculatedVoltage[0] / 1000, 2));}
  AssertEquals(cmod(MRK500Bus.CalculatedVoltage[0] / 1000), 486.0, 2.0);
  AssertEquals(cmod(JAS220Bus.CalculatedVoltage[0] / 1000), 213.0, 2.0);
  AssertEquals(cmod(MAR220Bus.CalculatedVoltage[0] / 1000), 204.5, 2.0);
  AssertEquals(cmod(ROS220Bus.CalculatedVoltage[0] / 1000), 224.0, 2.0);
  AssertEquals(cmod(ATB220Bus.CalculatedVoltage[0] / 1000), 226.0, 2.0);
  AssertEquals(cmod(GDF220Bus.CalculatedVoltage[0] / 1000), 220.0, 2.0);
end;


initialization

  RegisterTest(TNECGridTest);
end.
