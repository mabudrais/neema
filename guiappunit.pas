unit GuiAppUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Dialogs, ConsolAppUnit, DrawingUnit, elementUnit,
  ExtCtrls, CmdUnit, LoadFlowOptionFormUnit, StateEstimationOptionFormUnit,
  resultformunit,t3wparatoolunit,
  ElementFactoryUnit, neemacanvasUnit, contnrs, LCLType, EditingFormIUnit, UnusedVarUnit;

type
  { TNeemaGUI }

  TNeemaGUI = class(TNeema)
  private
    LastSearchElemntID: integer;
    LastSearchTerm: string;
    procedure Search(aSearchTerm: string; const aID: integer);
  public
    procedure UpdateDrawing(Tick: boolean);
    procedure UpdateDrawingConectivity();
    procedure SetActiveDrawing(Index: integer);
    procedure SetNewElementClass(NewClassName: string);
    procedure Resize(Image1: TImage; DrawingPanel: TPanel);
    procedure DrawingDoublClick(X, Y: integer);
    procedure DrawingClick(X, Y: integer);
    procedure DrawingMouseUP(X, Y: integer);
    procedure DrawingMouseMove(X, Y: integer; IsShift: boolean);
    procedure KeyPress(Key: char);
    procedure ShowSolveOption();
    procedure ShowSEOption();
    procedure ShowResultForm();
    procedure ShowT3WTools;
    procedure ConectSelectedElement();
    procedure ClearSelection();
    procedure ClearPutElement();
    procedure Search;
    procedure SearchNext;
    constructor Create(C: INeemaCanvas);
    procedure SetMemo(M: TMemo);
  end;

implementation

procedure TNeemaGUI.Search(aSearchTerm: string; const aID: integer);
var
  Element: TElement;
begin
  Element := CDrawing.FindElementWhichNameContain(aSearchTerm, aID);
  if Assigned(Element) then
  begin
    CDrawing.CenterElement(Element);
    LastSearchTerm:=aSearchTerm;
    LastSearchElemntID := Element.ID;
  end
  else
    ShowMessage('not found');
end;

procedure TNeemaGUI.UpdateDrawing(Tick: boolean);
begin
  CDrawing.UpdateDrawing(Canvas, Tick);
end;

procedure TNeemaGUI.UpdateDrawingConectivity();
begin
  CDrawing.UpdateDrawingConectivity();
end;

procedure TNeemaGUI.SetActiveDrawing(Index: integer);
begin
  CDrawing := DrawingList[Index] as TDrawing;
end;

procedure TNeemaGUI.SetNewElementClass(NewClassName: string);
begin
  CDrawing.SetNewElementClass(NewClassName);
end;

procedure TNeemaGUI.Resize(Image1: TImage; DrawingPanel: TPanel);
var
  w: integer;
begin
  w := DrawingPanel.Width;
  Image1.Width := w;
  Image1.Picture.Bitmap.SetSize(w, DrawingPanel.Height);
end;

procedure TNeemaGUI.DrawingDoublClick(X, Y: integer);
begin
  UseInt(X);
  UseInt(y);
end;

procedure TNeemaGUI.DrawingClick(X, Y: integer);
begin
  CDrawing.MouseDown(X, Y);
end;

procedure TNeemaGUI.ConectSelectedElement();
begin
  CDrawing.ConectSelectedElement();
  CDrawing.ClearSelection();
end;

procedure TNeemaGUI.ClearSelection();
begin
  CDrawing.ClearSelection();
end;

procedure TNeemaGUI.ClearPutElement();
begin
  CDrawing.NewElementClass := nil;
end;

procedure TNeemaGUI.Search;
var
  aID: integer;
  SearchTerm: string;
begin
  SearchTerm := InputBox('search element', 'enter string', '');
  if SearchTerm.Length < 2 then
    ShowMessage('please enter 2 letter or more')
  else
    Search(SearchTerm, -1);
end;

procedure TNeemaGUI.SearchNext;
begin
  Search(LastSearchTerm, LastSearchElemntID);
end;

constructor TNeemaGUI.Create(C: INeemaCanvas);
begin
  inherited Create(c);
end;

procedure TNeemaGUI.SetMemo(M: TMemo);
begin
  CmdUnit.Memo := M;
end;

procedure TNeemaGUI.KeyPress(Key: char);
var
  SelectedElements: TObjectList;
  EForm: TForm;
begin
  if (Key = 'e') or (key = 'E') then
  begin
    SelectedElements := TObjectList.Create(False);
    CDrawing.GetSelectedElements(SelectedElements);
    if SelectedElements.Count < 1 then
      ShowMessage('please select an element');
    if SelectedElements.Count = 1 then
    begin
      EForm := GetEditingForm(SelectedElements[0]);
      if EForm <> nil then
        (EForm as IEditingForm).SetElement(SelectedElements[0], Self.CDrawing).ShowModal
      else
        ShowMessage('you can not edit this element');
    end
    else
      ShowMessage('you can not edit more than element');
    SelectedElements.Free;
  end
  else if (Key = 'c') or (key = 'C') then
    ConectSelectedElement();
end;

procedure TNeemaGUI.ShowSolveOption();
begin
  LoadFlowOptionForm.NeemaGui := Self;
  LoadFlowOptionForm.ShowModal;
end;

procedure TNeemaGUI.ShowSEOption();
begin
  StateEstimationOptionForm.NeemaGui := Self;
  StateEstimationOptionForm.ShowModal;
end;

procedure TNeemaGUI.ShowResultForm;
begin
  ResultForm.Update(CDrawing.GetElementList);
  ResultForm.ShowModal;
end;

procedure TNeemaGUI.ShowT3WTools;
begin
  t3wparatoolunit.T3wParaToolForm.ShowModal;
end;

procedure TNeemaGUI.DrawingMouseMove(X, Y: integer; IsShift: boolean);
begin
  if IsShift then
    CDrawing.MousePan(X, Y);
end;

procedure TNeemaGUI.DrawingMouseUP(X, Y: integer);
begin
  UseInt(X);
  UseInt(y);
end;

end.
