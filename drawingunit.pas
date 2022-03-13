unit DrawingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, GenircDataUnit, elementUnit, neemacanvasUnit,
  ElementFactoryUnit, ConectionUnit, contnrs, multibusunit,
  TransmissionLineUnit, UComplex, WritlnUnit, fpjsonrtti, ElementNamingUnit,
  DrawingPresistanceUnit, ElementColoringUnit, FileUtil, ElectricElentUnit,
  BusUnit, NeemaDialogMessageUnit, SEMeasurementUnit, UtilUnit, fpexprpars;

type
  { TDrawing }

  TDrawing = class(IElementNaming)
    constructor Create;
    constructor Create(FileName: string);
  private
    EL: TIntegerObjectMap;
    XT, YT: integer;
    FName: string;
    PanMove: boolean;
    PanX: integer;
    PanY: integer;
    procedure EvaluteMeasurements(CalcType: integer;
      IDList: TGeneralInteger64ObjectMap; T: TDateTime);
    procedure LoadFromFile(FileName: string);
    procedure ReadDrawingOption(const DeStreamer: TJSONDeStreamer;
      OptionStr: string);
    procedure setName(AValue: string);
    procedure UnConectElement(ID1, ID2: integer);
  public
    FBackgroundColor: integer;
    NewElementClass: TElementClass;
    function IsPossableName(aElementName: string): boolean;
    procedure UpdateDrawing(Canvas: INeemaCanvas; Tick: boolean);
    procedure UpdateDrawingConectivity();
    procedure UpdateConectionPos();
    procedure SetNewElementClass(NewClassName: string);
    procedure AddElement(X, Y: integer);
    procedure MouseDown(X, Y: integer);
    procedure MouseUp(X, Y: integer);
    procedure MouseMove(X, Y: integer);
    procedure MousePan(X, Y: integer);
    procedure ConectElements(ID1, ID2: integer);
    procedure GetSelectedElements(var temp: TObjectList);
    procedure ConectSelectedElement();
    procedure DeletSelectedElement();
    procedure DeletElement(ID: integer);
    procedure ClearSelection();
    function FindElementWhichNameContain(aSubStr: string;
      IDLimt: integer): TElement;
    procedure CenterElement(aElement: TElement);
    function AddElement(X, Y: integer; ElementClass: TElementClass): TElement;
    function GetElement(ElementName: string): TElement;
    procedure IsolatedAllElementsWichIsNotObservableByES();
    procedure PrintTransimissinLineData();
    procedure UpdateMeasurmentFromDataString(T: TDateTime);
    procedure CenterTransimissionLine();
    function GetElementList: TIntegerObjectMap;
    function SaveToFile(FileName: string): string;
    destructor Destroy; override;
  published
    property Name: string read FName write setName;
  end;

implementation

{ TDrawing }

constructor TDrawing.Create;
begin
  EL := TIntegerObjectMap.Create(True);
  Self.FName := 'Drawing1';
end;

procedure TDrawing.UpdateDrawing(Canvas: INeemaCanvas; Tick: boolean);
var
  ele: TElement;
  O: TObject;
begin
  Canvas.SetTransform(0, 0);
  Canvas.setBrushColor(ClNemSilver);
  Canvas.setPenColor(ClNemSilver);
  Canvas.Rectangle(0, 0, Canvas.GetWidth(), Canvas.GetHeight());
  Canvas.SetTransform(XT, YT);
  for  O in Self.el do
  begin
    ele := O as TElement;
    //clname := ele.ClassName;
    //type_of_ele := ele.get_element_type;
    ele.Draw(Canvas, Tick);
    //ImageList1.Draw(Image1.Canvas, ele.posx, ele.posy, ele.imageindex);
  end;
end;

function TDrawing.IsPossableName(aElementName: string): boolean;
var
  O: TObject;
begin
  Result := True;
  aElementName := LowerCase(aElementName);
  for  O in Self.el do
  begin
    if LowerCase((O as TElement).Name) = aElementName then
      Exit(False);
  end;
end;

procedure TDrawing.UpdateDrawingConectivity();
var
  Coloring: TColoring;
begin
  Coloring := TColoring.Create;
  Coloring.ModifyColor(Self);
  Coloring.Free;
end;

procedure TDrawing.UpdateConectionPos();
var
  O: TObject;
  ele: TElement;
  cConection: TConection;
  Posx1, Posx2, Posy1, Posy2: integer;
begin
  for  O in Self.el do
  begin
    ele := O as TElement;
    if o is TConection then
    begin
      cConection := ele as TConection;
      if not (EL[cConection.ID1] as TElement).GetConectionPos(
        cConection.ID2, Posx1, Posy1) then
        raise Exception.Create('error');
      cConection.X1 := Posx1;
      cConection.y1 := Posy1;
      if not (EL[cConection.ID2] as TElement).GetConectionPos(
        cConection.ID1, Posx2, Posy2) then
        raise Exception.Create('error');
      cConection.X2 := Posx2;
      cConection.y2 := Posy2;
    end;
  end;
end;

procedure TDrawing.SetNewElementClass(NewClassName: string);
begin
  NewElementClass := ElementFactoryUnit.GetClassType(NewClassName);
end;

procedure TDrawing.AddElement(X, Y: integer);
begin
  AddElement(X - XT, Y - YT, NewElementClass);
end;

function TDrawing.AddElement(X, Y: integer; ElementClass: TElementClass): TElement;
var
  NewElementName: string;
  k: integer;
begin
  Result := ElementClass.Create;
  Result.SetXY(X, Y);
  EL.Addw(Result);
  k := 0;
  NewElementName := Result.ClassName + IntToStr(Result.ID + k);
  while not IsPossableName(NewElementName) do
  begin
    Inc(k);
    NewElementName := Result.ClassName + IntToStr(Result.ID + k);
  end;
  Result.Name := NewElementName;
end;

function TDrawing.GetElement(ElementName: string): TElement;
var
  O: TObject;
begin
  Result := nil;
  ElementName := LowerCase(ElementName);
  for  O in Self.el do
  begin
    if LowerCase((O as TElement).Name) = ElementName then
      Exit(o as TElement);
  end;
end;

procedure TDrawing.IsolatedAllElementsWichIsNotObservableByES();
var
  E: TObject;
  Elec: TElicment;
begin
  for E in EL do
  begin
    if E is TElicment then
    begin
      Elec := E as TElicment;
      if not (Elec.IsObservableByEstateEstimator) and
        (not ((Elec is TBus) or (Elec is TMultiBus) or (Elec is TConection))) then
      begin
        if not Elec.Dead then
        begin
          Elec.OpenAllCB();
          //you should open cb directly you need to send massage for undo redo
        end;
      end;
    end;
  end;
end;

function TDrawing.GetElementList: TIntegerObjectMap;
begin
  Result := EL;
end;

destructor TDrawing.Destroy;
begin
  inherited Destroy;
  El.Free;
end;

procedure TDrawing.MouseDown(X, Y: integer);
var
  O: TObject;
  cE: TElement;
begin
  if self.NewElementClass <> nil then
    self.AddElement(X, Y)
  else
  begin
    for  O in Self.el do
    begin
      cE := O as TElement;
      if cE.SetSelected(X - XT, Y - YT) then
        Exit;
    end;
  end;
end;

procedure TDrawing.MouseUp(X, Y: integer);
begin
  PanMove := False;
end;

procedure TDrawing.ConectElements(ID1, ID2: integer);
var
  E1, E2: TElement;
  Y2, X2, Y1, X1: integer;
  Conection: TConection;
  isE1Bus, isE2Bus: boolean;
begin
  E1 := EL[ID1] as TElement;
  E2 := EL[ID2] as TElement;
  if E1.IsConectedTo(E2.ID) then
  begin
    UnConectElement(ID1, ID2);
    Exit;
  end;
  isE1Bus := (E1 is TBus) or (E1 is TMultiBus);
  isE2Bus := (E2 is TBus) or (E2 is TMultiBus);
  if (isE1Bus and isE2Bus) then
  begin
    ShowNeemaDialogMessage('conecting two bus is not suported');
    Exit;
  end;
  if (not (isE1Bus)) and (not (isE2Bus)) then
  begin
    ShowNeemaDialogMessage('one element must be bus');
    Exit;
  end;
  if (not E1.GetNextConectionPos(X1, Y1)) or (not E2.GetNextConectionPos(X2, Y2)) then
    Exit;
  Conection := TConection.Create;
  Conection.Ini(ID1, ID2, X1, Y1, X2, Y2);
  EL.Addw(Conection);
  E1.AddConection(ID2);
  E2.AddConection(ID1);
end;

procedure TDrawing.UnConectElement(ID1, ID2: integer);
var
  k: integer;
  O: TObject;
  Con: TConection;
begin
  (EL[ID1] as TElement).RemoveConection(ID2);
  (EL[ID2] as TElement).RemoveConection(ID1);
  k := 0;
  while k < EL.Count do
  begin
    O := EL.Data[k];
    if o is TConection then
    begin
      Con := O as TConection;
      if ((Con.ID1 = ID1) and (Con.ID2 = ID2)) or
        ((Con.ID1 = ID2) and (Con.ID2 = ID1)) then
      begin
        EL.Delete(k);
        Exit;//multi conection are not allowed
      end;
    end;
    Inc(k);
  end;
end;

procedure TDrawing.ConectSelectedElement();
var
  temp: TObjectList;
  tempCount: integer;
begin
  temp := TObjectList.Create(False);
  GetSelectedElements(temp);
  tempCount := temp.Count;
  if temp.Count = 2 then
    ConectElements((temp[0] as TElement).ID, (temp[1] as TElement).ID);
  temp.Free;
 { if tempCount <> 2 then
    raise Exception.Create('two elemnt must be selected'); }
end;

procedure TDrawing.DeletElement(ID: integer);
var
  k: integer;
  O: TObject;
  Con: TConection;
  E: TElement;
begin
  E := EL[ID] as TElement;
  for k := 0 to e.ConectionCount - 1 do
    (EL[e.GetConection(k)] as TElement).RemoveConection(e.ID);
  k := 0;
  while k < EL.Count do
  begin
    O := EL.Data[k];
    if o is TConection then
    begin
      Con := O as TConection;
      if (Con.ID1 = e.ID) or (Con.ID2 = e.ID) then
      begin
        EL.Delete(k);
        Dec(k);
      end;
    end;
    Inc(k);
  end;
  EL.Remove(ID);
  UpdateConectionPos();
end;

procedure TDrawing.DeletSelectedElement();
var
  SelectedElements: TObjectList;
  k: integer;
begin
  SelectedElements := TObjectList.Create(False);
  self.GetSelectedElements(SelectedElements);
  for k := 0 to SelectedElements.Count - 1 do
    DeletElement((SelectedElements[k] as TElement).ID);
  SelectedElements.Free;
end;

procedure TDrawing.ClearSelection();
var
  O: TObject;
begin
  for  O in Self.el do
    (O as TElement).IsSelected := False;
end;

function TDrawing.FindElementWhichNameContain(aSubStr: string; IDLimt: integer): TElement;
var
  O: TObject;
  E: TElement;
begin
  aSubStr := LowerCase(aSubStr);
  Result := nil;
  for  O in Self.el do
  begin
    E := (O as TElement);
    if (LowerCase(E.Name).Contains(aSubStr)) then
    begin
      if (E.ID > IDLimt) then
        Exit(o as TElement);
    end;
  end;
end;

procedure TDrawing.PrintTransimissinLineData();
var
  O: TObject;
  Line: TLine;
  Bus1, Bus2: TMultiBus;
  S1, S2: complex;
begin
  for  O in Self.el do
  begin
    if o is TLine then
    begin
      Line := o as TLine;
      if Line.Name.Contains('ROS') then
        wl('ROs');
      if Line.ConectionCount <> 2 then
        raise Exception.Create('Line.ConectionCount<>2');
      if (not (el[Line.GetConection(0)] is TMultiBus)) or
        (not (el[Line.GetConection(0)] is TMultiBus)) then
        Continue;
      Bus1 := el[Line.GetConection(0)] as TMultiBus;
      Bus2 := el[Line.GetConection(1)] as TMultiBus;
      Line.GetSPerUnit(Bus1.CalculatedVoltage[0] / StringToVolt(Bus1.BaseVolt),
        Bus2.CalculatedVoltage[0] / StringToVolt(Bus2.BaseVolt), S1, S2);
      WL(Bus1.Name + Bus2.Name + ' ' + FormatFloat('#.#', S1.re * 100) +
        ' MW, ' + FormatFloat('#.#', S1.im * 100) + ' MVar');
      WL(Bus2.Name + Bus1.Name + ' ' + FormatFloat('#.#', S2.re * 100) +
        ' MW, ' + FormatFloat('#.#', S2.im * 100) + ' MVar');
    end;
  end;
end;

procedure TDrawing.UpdateMeasurmentFromDataString(T: TDateTime);
var
  O: TObject;
  M: TMeasurmen;
  ELicEl: TElicment;
  EPF: TEPF;
  EV: TFPExpressionParser;
  IDList: TGeneralInteger64ObjectMap;
begin
  IDList := TGeneralInteger64ObjectMap.Create(True);
  EvaluteMeasurements(1, IDList, T);
  GetDataFromSampleServer(IDList, T);
  EvaluteMeasurements(2, IDList, T);
  IDList.Free;
end;

procedure TDrawing.CenterTransimissionLine();
var
  cE, B1, B2: TElement;
  O: TObject;
  Line: TLine;
begin
  for  O in Self.el do
  begin
    if o is TLine then
    begin
      Line := O as TLine;
      if Line.ConectionCount = 2 then
      begin
        B1 := Self.EL[Line.GetConection(0)] as TElement;
        B2 := Self.EL[Line.GetConection(1)] as TElement;
        Line.XPos := round((B1.XPos + B2.XPos) / 2);
        Line.yPos := round((B1.yPos + B2.yPos) / 2);
      end;
    end;
  end;
end;

procedure TDrawing.GetSelectedElements(var temp: TObjectList);
var
  cE: TElement;
  O: TObject;
begin
  for  O in Self.el do
  begin
    cE := O as TElement;
    if cE.IsSelected then
      temp.Add(cE);
  end;
end;

function TDrawing.SaveToFile(FileName: string): string;
var
  O: TObject;
  E: TElement;
  Slist: TStringList;
  Streamer: TJSONStreamer;
  DP: TDrawingPresistance;
  Exten: string;
begin
  Slist := TStringList.Create;
  Streamer := TJSONStreamer.Create(nil);
  DP := TDrawingPresistance.Create.INI(Self);
  Exten := ExtractFileExt(FileName);
  if Exten <> '.nem' then
  begin
    if (Exten.IsEmpty) and (not (FileName.Contains('.'))) then
      FileName := FileName + '.nem'
    else
      Exit('wrong file name');
  end;
  DP.Name := ExtractFileNameWithoutExt(ExtractFileName(FileName));
  Slist.Add(DP.ClassName);
  Slist.Add(Streamer.ObjectToJSONString(DP));
  Dp.Free;
  for  O in Self.el do
  begin
    E := o as TElement;
    Slist.Add(e.ClassName);
    Slist.Add(Streamer.ObjectToJSONString(e));
  end;
  Slist.SaveToFile(FileName);
  Slist.Free;
  Streamer.Free;
  Result := '';
end;

procedure TDrawing.LoadFromFile(FileName: string);
var
  Slist: TStringList;
  DeStreamer: TJSONDeStreamer;
  k: integer;
  E: TElement;
begin
  Slist := TStringList.Create;
  Slist.LoadFromFile(FileName);
  if (Slist.Count mod 2) <> 0 then
    raise Exception.Create('file has odd num of line');
  DeStreamer := TJSONDeStreamer.Create(nil);
  k := 0;
  //drawing option
  if Slist[k] = 'TDrawingPresistance' then
  begin
    ReadDrawingOption(DeStreamer, Slist[k + 1]);
    Inc(k, 2);
  end;
  while k < Slist.Count do
  begin
    E := StringToElement(Slist[k], Slist[k + 1], DeStreamer);
    EL.Add(E.ID, E);
    Inc(k, 2);
  end;
  Slist.Free;
  DeStreamer.Free;
  //CenterTransimissionLine();
  UpdateConectionPos();
end;

procedure TDrawing.EvaluteMeasurements(CalcType: integer;
  IDList: TGeneralInteger64ObjectMap; T: TDateTime);
var
  EV: TFPExpressionParser;
  EPF: TEPF;
  ELicEl: TElicment;
  M: TMeasurmen;
  o: TObject;
begin
  EV := TFPExpressionParser.Create(nil);
  EPF := TEPF.Create;
  EPF.IDList := IDList;
  if CalcType = 1 then
    EV.Identifiers.AddFunction('G', 'F', 'S', @EPF.ExtractIDsfunction)
  else
    EV.Identifiers.AddFunction('G', 'F', 'S', @EPF.GfunctionFromIDList);
  for  O in Self.el do
  begin
    if o is TElicment then
    begin
      ELicEl := (o as TElicment);
      M := ELicEl.GetMeasurment();
      if Assigned(M) then
        SetMeasermentFromDataString(ELicEl.ServerData, M, T, EV, EPF);
    end;
  end;
  EV.Free;
  EPF.Free;
end;

procedure TDrawing.ReadDrawingOption(const DeStreamer: TJSONDeStreamer;
  OptionStr: string);
var
  DP: TDrawingPresistance;
begin
  DP := TDrawingPresistance.Create;
  DeStreamer.JSONToObject(OptionStr, DP);
  FName := DP.Name;
  FBackgroundColor := Dp.BackgroundColor;
  DP.Free;
end;

procedure TDrawing.setName(AValue: string);
begin
  FName := AValue;
end;

constructor TDrawing.Create(FileName: string);
begin
  Create;
  LoadFromFile(FileName);
end;

procedure TDrawing.MouseMove(X, Y: integer);
begin

end;

procedure TDrawing.CenterElement(aElement: TElement);
begin
  XT := 100 - aElement.XPos;
  YT := 100 - aElement.YPos;
end;

procedure TDrawing.MousePan(X, Y: integer);
var
  k: integer;
  O: TObject;
  SelectedElement: TElement;
begin
  SelectedElement := nil;
  for  O in Self.el do
  begin
    if (O as TElement).IsSelected then
    begin
      SelectedElement := (O as TElement);
      Break;
    end;
  end;
  if Assigned(SelectedElement) then
  begin
    SelectedElement.XPos := SelectedElement.XPos - X;
    SelectedElement.YPos := SelectedElement.YPos - Y;
    UpdateConectionPos();
  end
  else
  begin
    XT := XT - X;
    YT := YT - Y;
  end;
end;

end.
