unit ConsolAppUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, neemacanvasUnit, contnrs, DrawingUnit, ElementFactoryUnit,
  elementUnit, SolverUnit, LoadFlowSolverUnit, NrsolverUnit, WritlnUnit,
  StateEstimationUnit,
  UComplex, IeeeImporUnit, CmdUnit;

type

  { TNeema }

  TNeema = class
  private
    function GetDrawing(Name: string): TDrawing;
  protected
    DrawingList: TObjectList;
    Canvas: INeemaCanvas;
  public
    CDrawing: TDrawing;
    CSolver: TSolverClass;
    constructor Create(C: INeemaCanvas);
    destructor Destroy; override;
    function Solve(Option: string; AdditionData: TObject = nil): TSolver;
    procedure SolveAndFreeSolver(Option: string; AdditionData: TObject = nil);
    procedure RunStateEstimator();
    function PowerError(S: TSolver): complex;
    procedure UpdateMeasurmentFromDataString(T: TDateTime);
    function CreateEmptyDrawing: Integer;
    procedure CopyActiveDrawing();
    procedure SetCanvasCanvas(c: TObject; W, H: integer);
    function AddElement(X, Y: integer; ElementClass: TElementClass): TElement;
    function GetElement(ElementName: string): TElement;
    procedure DeletSelectedElement();
    procedure IsolatedAllElementsWichIsNotObservableByES();
    procedure OpenFile(FileName: string);
    function SaveToFile(FileName: string): string;
    procedure ImportPython();
    procedure GetDrawingNames(Names: TStrings);
    function GetActiveDrawingIndex(): integer;
  end;

implementation

constructor TNeema.Create(C: INeemaCanvas);
begin
  CSolver := nil;
  if c = nil then
    Exception.Create('canvas is nil');
  DrawingList := TObjectList.Create;
  //LastSolver := gssolver;
  //  CSolver:=TDMathSolver;
  CSolver := TNRSolver3;
  //CSolver := TMarquardtSolver;
  //CSolver := gridcalsolverunit.TGridCal;
  Canvas := C;
  ElementFactoryUnit.IninFactory();
  //neemaoption := TNeemaoption.Create;
end;

destructor TNeema.Destroy;
begin
  DrawingList.Free;
  FinalizeFactory();
  inherited Destroy;
end;

function TNeema.CreateEmptyDrawing():Integer;
var
  CDrawingName: string;
  k: integer;
begin
  CDrawing := TDrawing.Create;
  k := 1;
  CDrawingName := 'Drawing' + IntToStr(k);
  while Assigned(GetDrawing(CDrawingName)) do
  begin
    Inc(k);
    CDrawingName := 'Drawing' + IntToStr(k);
  end;
  CDrawing.Name := CDrawingName;
  DrawingList.Add(CDrawing);
  Result:=DrawingList.Count-1;
end;

procedure TNeema.CopyActiveDrawing;
var
  CoD: TDrawing;
begin
 CoD:=DrawingList[ CreateEmptyDrawing()] as TDrawing;

end;

function TNeema.GetDrawing(Name: string): TDrawing;
var
  k: integer;
begin
  Result := nil;
  for k := 0 to DrawingList.Count - 1 do
  begin
    if (DrawingList[k] as TDrawing).Name = Name then
      Exit(DrawingList[k] as TDrawing);
  end;
end;

function TNeema.Solve(Option: string; AdditionData: TObject): TSolver;
var
  S: TSolver;
begin
  S := CSolver.Create;
  try
    S.Solve(CDrawing, Option, AdditionData);
  except
    on E: Exception do
    begin
      CmdUnit.ShowDialogText('Failed to solve');
      CmdUnit.SetMemoText(e.Message);
    end;
  end;
  Result := S;
end;

procedure TNeema.SolveAndFreeSolver(Option: string; AdditionData: TObject);
begin
  Solve(Option, AdditionData).Free;
end;

procedure TNeema.RunStateEstimator();
var
  S: TSolver;
  OldSolver: TSolverClass;
begin
  OldSolver := CSolver;
  CSolver := TES;
  S := CSolver.Create;
  try
    S.Solve(CDrawing, '', nil);
  except
    on E: Exception do
    begin
      CmdUnit.ShowDialogText('Failed to solve');
      CmdUnit.SetMemoText(e.Message);
    end;
  end;
  S.Free;
  CSolver := OldSolver;
end;

function TNeema.PowerError(S: TSolver): complex;
begin
  Result := (S as TLoadFlowSolver).PowerError;
end;

procedure TNeema.UpdateMeasurmentFromDataString(T: TDateTime);
begin
  CDrawing.UpdateMeasurmentFromDataString(T);
end;

function TNeema.AddElement(X, Y: integer; ElementClass: TElementClass): TElement;
begin
  Result := CDrawing.AddElement(X, Y, ElementClass);
end;

function TNeema.GetElement(ElementName: string): TElement;
begin
  Result := CDrawing.GetElement(ElementName);
end;

procedure TNeema.DeletSelectedElement();
begin
  CDrawing.DeletSelectedElement();
end;

procedure TNeema.IsolatedAllElementsWichIsNotObservableByES();
begin
  CDrawing.IsolatedAllElementsWichIsNotObservableByES();
end;

procedure TNeema.SetCanvasCanvas(c: TObject; W, H: integer);
begin
  Canvas.SetCanvas(c);
  Canvas.SetBitmapSize(W, H);
end;

procedure TNeema.ImportPython();
begin
  IeeeImporUnit.ImportCase('C:\Python27\Lib\site-packages\pypower\case14.py', Self);
end;

procedure TNeema.OpenFile(FileName: string);
begin
  CDrawing := TDrawing.Create(FileName);
  DrawingList.Add(CDrawing);
end;

function TNeema.SaveToFile(FileName: string): string;
begin
  Result := CDrawing.SaveToFile(FileName);
end;

procedure TNeema.GetDrawingNames(Names: TStrings);
var
  k: integer;
  DName:String;
begin
  Names.Clear;
  for k := 0 to DrawingList.Count - 1 do
  begin
    DName:=(DrawingList[k] as TDrawing).Name ;
        Names.Add(DName);
  end;
end;

function TNeema.GetActiveDrawingIndex(): integer;
begin
  Result := DrawingList.IndexOf(CDrawing);
end;

end.
