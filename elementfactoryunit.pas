unit ElementFactoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, elementUnit,ConectionUnit, BusUnit, multibusunit, TransmissionLineUnit,TransFormer2WUnit,TransFormer3WUnit
  ,TransFormer3WShuntUnit,SVCUnit,LabelUnit, fpjsonrtti,ShuntUnit,PiImpedanceUnit;

type
  TClassArray = array of TElementClass;

function GetClassType(ElementClassName: string): TElementClass;
function StringToElement(ElementClassName,Data:String;DeStreamer: TJSONDeStreamer):TElement;
procedure IninFactory();
procedure FinalizeFactory();
var
  ClassArray: TClassArray;
  ClassName: TStringList;
implementation

procedure IninFactory();
var
  N1, N2: string;
  k: integer;
begin
  ClassArray := TClassArray.Create(TConection,TBus, TMultiBus, TLine,TTransFormer2W,TTransFormer3W,TTransFormer3WShunt,TSVC,TNeemaLabel
  ,TShunt,TPiImpedance);
  ClassName := TStringList.Create;
  ClassName.AddStrings(['TConection','TBus', 'TMultiBus', 'TLine','TTransFormer2W',
  'TTransFormer3W','TTransFormer3WShunt','TSVC','TNeemaLabel','TShunt','TPiImpedance']);
  if Length(ClassArray) <> ClassName.Count then
    raise Exception.Create('Length(ClassArray) <> ClassName.Count');
  for k := 0 to ClassName.Count - 1 do
  begin
    N1 := ClassArray[k].ClassName;
    N2 := ClassName[k];
    if N1 <> N2 then
      raise Exception.Create('');
  end;
end;

procedure FinalizeFactory();
begin
  ClassName.Free;
end;

function GetClassType(ElementClassName: string): TElementClass;
begin
  if ClassName.IndexOf(ElementClassName) < 0 then
    raise Exception.Create('faile to fiind class ' + ElementClassName);
  Result := ClassArray[ClassName.IndexOf(ElementClassName)];
end;

function StringToElement(ElementClassName, Data: String; DeStreamer: TJSONDeStreamer): TElement;
var
  EClass: TElementClass;
begin
  EClass:=GetClassType(ElementClassName);
  Result:=EClass.Create;
  DeStreamer.JSONToObject(Data,Result);
end;
end.
