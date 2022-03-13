unit DrawingByCodeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BusUnit, multibusunit, ConsolAppUnit, TransmissionLineUnit,
  UComplex;

 procedure SetPolarVolt(mBus: TMultiBus; r, Del: extended);
 procedure AddMToL(L: TLine; P, Q: extended);
procedure ADL(Name: string; paraIndex, B1, B2: integer;
  length: double; out Line1, Line2: TLine; Neema: TNeema);
procedure AL(Name: string; B1, B2, paraIndex: integer;
  length: double; out Line: TLine; Neema: TNeema);
procedure ADB(Name: string; Volt: double; BusType: TBusType;
  PQ: complex; out MBus: TMultiBus; Neema: TNeema);
implementation
  
procedure SetPolarVolt(mBus: TMultiBus; r, Del: extended);
begin
  mBus.Bus1VoltageR := FloatToStr(r * cos(Del));
  mBus.Bus1VoltageI := FloatToStr(r * sin(Del));
end;

procedure AddMToL(L: TLine; P, Q: extended);
begin
  L.m.p1 := P;
  L.m.q1 := Q;
end;

procedure ADL(Name: string; paraIndex, B1, B2: integer;
  length: double; out Line1, Line2: TLine; Neema: TNeema);
begin
  AL(Name + '1', B1, B2, paraIndex, length, Line1, Neema);
  AL(Name + '2', B1, B2, paraIndex, length, Line2, Neema);
end;

procedure AL(Name: string; B1, B2, paraIndex: integer;
  length: double; out Line: TLine; Neema: TNeema);
begin
  Line := Neema.AddElement(100, 100, TLine) as TLine;
  Line.Name := Name;
  Neema.CDrawing.ConectElements(B1, Line.ID);
  Neema.CDrawing.ConectElements(Line.ID, B2);
  if paraIndex = 501 then
    Line.setData(0.028, 0.276, 243300.37925842, length, 500 * 1000, 100e6)
  else if paraIndex = 221 then
    Line.setData(0.067, 0.302, 243728.856189733, length, 220 * 1000, 100e6)
  else if paraIndex = 222 then
    Line.setData(0.076, 0.403, 352893.443662739, length, 220 * 1000, 100e6)
  else
    raise Exception.Create('wrong paraIndex');
end;

procedure ADB(Name: string; Volt: double; BusType: TBusType;
  PQ: complex; out MBus: TMultiBus; Neema: TNeema);
var
  Bus2Type: TBusType;
begin
  MBus := Neema.AddElement(50, 50, TMultiBus) as TMultiBus;
  MBus.Name := Name;
  Bus2Type:=BusType;
  if Bus2Type =slackbus then
    Bus2Type:=loadbus;
  MBus.SetActiveBusData(0, BusType, Volt, PQ, Name + '1');
  MBus.SetActiveBusData(1, Bus2Type, Volt, 0, Name + '2');
  MBus.BC12Closed := True;
  MBus.BaseVA := '100e6';
  MBus.BaseVolt := Volt.ToString();
end;

end.

