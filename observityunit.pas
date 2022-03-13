unit ObservityUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, elementUnit, GenircDataUnit, multibusunit,
  BusUnit, TransmissionLineUnit, TransFormer2WUnit, TransFormer3WShuntUnit,
  TransFormer3WUnit, contnrs, SEMeasurementUnit, ConectionUnit, WritlnUnit, LabelUnit;

function FillConectorArray(const SlackBusList: TObjectList;
  var IsConnector: array of boolean; const EL: TIntegerObjectMap): boolean;

implementation

function IsTwoPortConector(const cM: TCustemPimeasurmen;
  const CB1, CB2: boolean): boolean;
var
  IsQ2, IsQ1, IsP2, IsP1: boolean;
begin
  IsP1 := CB1 and (IsAm(cM.p1));
  IsP2 := CB2 and IsAm(cM.p2);
  IsQ1 := CB1 and IsAm(cM.q1);
  IsQ2 := CB2 and IsAm(cM.q2);
  Result := (IsP1 or isP2) and (IsQ1 or isQ2);
end;

function IsLineConector(L: TLine): boolean;
var
  CB1, CB2: boolean;
  cM: Tpimeasurmen;
begin
  CB1 := L.CB1Close;
  CB2 := L.CB2Close;
  cM := L.M;
  Result := IsTwoPortConector(cM, CB1, CB2);
  //equatoin can be solved by two P or two Q, but p has weak effect in V and Q has weak effect in angele
end;

function IsT2WConector(T: TTransFormer2W): boolean;
var
  CB1, CB2: boolean;
  cM: TTrans2wMeasurmen;
begin
  CB1 := T.CB1Close;
  CB2 := T.CB2Close;
  cM := T.M;
  Result := IsTwoPortConector(cM, CB1, CB2);
  //equatoin can be solved by two P or two Q, but p has weak effect in V and Q has weak effect in angele
end;

function IsT3WShuntConector(T: TTransFormer3WShunt): boolean;
var
  CB1, CB2: boolean;
  cM: TTrans3wMeasurmen;
begin
  CB1 := T.CB1Close;
  CB2 := T.CB2Close;
  cM := T.M;
  Result := IsTwoPortConector(cM, CB1, CB2);
  //equatoin can be solved by two P or two Q but V has weak effect in V and Q has weak effect in angele
end;

function IsT3WConector(T: TTransFormer3W): boolean;
var
  CB1, CB2, CB3: boolean;
  cTemP: Tpimeasurmen;
  cM: TTrans3wMeasurmen;
begin
  CB1 := T.CB1Close;
  CB2 := T.CB2Close;
  CB3 := T.CB3Close;
  if not CB1 then
    Exit(False);//need to be corrected
  cM := T.M;
  if not CB3 then
  begin
    cTemP := Tpimeasurmen.Create('');
    cM.Set12(cTemP);
    Result := IsTwoPortConector(cTemP, CB1, CB2);
    Exit;
  end
  else if not CB2 then
  begin
    cTemP := Tpimeasurmen.Create('');
    cM.Set13(cTemP);
    Result := IsTwoPortConector(cTemP, CB1, CB3);
    Exit;
  end
  else
  begin
    cTemP := Tpimeasurmen.Create('');
    cM.Set12(cTemP);
    Result := IsTwoPortConector(cTemP, CB1, CB2);
    cM.Set13(cTemP);
    Result := Result and IsTwoPortConector(cTemP, CB1, CB3);
    cTemP.Free;
    Exit;
  end;
end;

function FillConectorArray(const SlackBusList: TObjectList;
  var IsConnector: array of boolean; const EL: TIntegerObjectMap): boolean;
var
  k: integer;
  M: integer;
  cMultiBus: TMultiBus;
  E: TObject;
begin
  for k := 0 to EL.Count - 1 do
  begin
    E := EL.Data[k];
    if E is TBus then
    begin
      IsConnector[k] := True;
      if ((E as TBus).BusType = slackbus) then
        SlackBusList.Add(E);
    end
    else if E is TMultiBus then
    begin
      cMultiBus := E as TMultiBus;
      if (cMultiBus.Bus1Type = slackbus) and (cMultiBus.Bus2Type = slackbus) then
        raise Exception.Create('two sub bus can not be slack');
      if (cMultiBus.Bus1Type = slackbus) or (cMultiBus.Bus2Type = slackbus) then
        SlackBusList.Add(E);
      for m := 0 to multibusunit.MaxSubBusNum - 1 do
        IsConnector[m * EL.Count + k] := True;
    end
    else if E is TLine then
      IsConnector[k] := IsLineConector(E as TLine)
    else if E is TTransFormer2W then
      IsConnector[k] := IsT2WConector(E as TTransFormer2W)
    else if E is TTransFormer3W then
      IsConnector[k] := IsT3WConector(E as TTransFormer3W)
    else if E is TTransFormer3WShunt then
      IsConnector[k] := IsT3WShuntConector(E as TTransFormer3WShunt)
    else if E is TConection then
    else if E is TNeemaLabel then
    else
    begin
      WritlnUnit.WL('unsuported element ' + E.ClassName);
      Exit(False);
    end;
  end;
  Result := True;
end;

function IsCBCloseTOBus(E, ToE: TElement; SBI: integer): boolean;
var
  L: TLine;
  W2: TTransFormer2W;
begin
  if not ((TOE is TBus) or (ToE is TMultiBus)) then
    raise Exception.Create('E of type' + E.ClassName + 'is conected to ' +
      ToE.ClassName);
  if E is TLine then
  begin
    L := E as TLine;
    if L.GetConection(0) = ToE.ID then
      Exit(l.CB1Close)
    else
    if L.GetConection(1) = ToE.ID then
      Exit(L.CB2Close)
    else
      raise
      Exception.Create('');
  end
  else if E is TTransFormer2W then
  begin
    W2 := E as TTransFormer2W;
    if W2.GetConection(0) = ToE.ID then
      Exit(l.CB1Close)
    else
    if W2.GetConection(1) = ToE.ID then
      Exit(l.CB2Close)
    else
      raise
      Exception.Create('');
  end;
end;

procedure ConectToSlack(E: TElement; ParentIndex, ElementIndex: integer;
  EL: TIntegerObjectMap; var IsConnected: array of boolean;
  const IsConnector: array of boolean; IsBus, IsMultiBus: boolean; SBI: integer);
var
  ConectedElement: TElement;
  ConectedElementIndex, k, CSBI: integer;
  MultiBus: TMultiBus;
begin
  // is not multibus
  if E is TMultiBus then
    raise Exception.Create('E is multi bus');
  IsConnected[ElementIndex] := True;
  for k := 0 to E.ConectionCount - 1 do
  begin
    ConectedElementIndex := El.IndexOf(E.GetConection(k));
    if ConectedElementIndex <> ParentIndex then
    begin
      ConectedElement := EL.Data[ConectedElementIndex] as TElement;
      if ConectedElement is TMultiBus then
      begin
        MultiBus := ConectedElement as TMultiBus;
        CSBI := MultiBus.GetSubBusIndex(E.ID);
      end
      else
      begin
        if (IsConnector[ConectedElementIndex]) and
          (IsCBCloseTOBus(ConectedElement, E, SBI)) then
          ConectToSlack(ConectedElement, ElementIndex, ConectedElementIndex,
            EL, IsConnected, IsConnector, False, False, -1);
      end;
    end;
  end;
end;

procedure FindElementsConectedToSlack(EL: TIntegerObjectMap);
var
  IsConnected, IsConnector: array of boolean;
  SlackBusList: TObjectList;
  k: integer;
begin
  SetLength(IsConnector, EL.Count * multibusunit.MaxSubBusNum);
  SlackBusList := TObjectList.Create(False);
  SetLength(IsConnected, EL.Count * multibusunit.MaxSubBusNum);
  if FillConectorArray(SlackBusList, IsConnector, EL) then
  begin
    for k := 0 to SlackBusList.Count - 1 do
    begin

    end;
  end;
end;

end.
