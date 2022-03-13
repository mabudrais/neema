unit Unit1;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, IntfGraphics, GraphType, GuiAppUnit, GuiCanvasUnit,
  TypInfo, dateutils,   DrawingBoxUnit, UnusedVarUnit, NeemaLogingUnit;

type
  { TForm1 }
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    DeleteMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    LFResultMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    T3wToolMenuItem: TMenuItem;
    SearchNextMenuItem: TMenuItem;
    SearchMenuItem: TMenuItem;
    NewMI: TMenuItem;
    SEMenuItem: TMenuItem;
    SolveMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    FileMI: TMenuItem;
    TBus_TB: TToolButton;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    Memo1: TMemo;
    Timer1: TTimer;
    TabControl1: TTabControl;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    DrawingPanel: TPanel;
    TLine_TB: TToolButton;
    TMultiBus_TB: TToolButton;
    DrawMI: TMenuItem;
    ConectMenuItem: TMenuItem;
    ClearSelectionMenuItem: TMenuItem;
    ClearPutElementMenuItem: TMenuItem;
    ClearBothMenuItem: TMenuItem;
    TNeemaLabel_TB: TToolButton;
    TPiImpedance_TB: TToolButton;
    TShunt_TB: TToolButton;
    TSVC_TB: TToolButton;
    TTransFormer3WShunt_TB: TToolButton;
    TTransFormer2W_TB: TToolButton;
    TTransFormer3W_TB: TToolButton;
    OpenMI: TMenuItem;
    SaveMI: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure LFResultMenuItemClick(Sender: TObject);
    procedure T3wToolMenuItemClick(Sender: TObject);
    procedure NewMIClick(Sender: TObject);
    procedure SearchMenuItemClick(Sender: TObject);
    procedure SearchNextMenuItemClick(Sender: TObject);
    procedure SEMenuItemClick(Sender: TObject);
    procedure SolveMenuItemClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TBus_TBClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ConectMenuItemClick(Sender: TObject);
    procedure ClearSelectionMenuItemClick(Sender: TObject);
    procedure ClearPutElementMenuItemClick(Sender: TObject);
    procedure ClearBothMenuItemClick(Sender: TObject);
    procedure DBPress(Sender: TObject; var Key: char);
    procedure OpenMIClick(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
  private
    App: TNeemaGUI;
    GUICanvas: TGUICanvas;
    LastImage1MouseMoveX: integer;
    LastImage1MouseMoveY: integer;
    LastImage1MouseDownTime: TDateTime;
    DB: TDrawingBox;
    Tick: int64;
    EPresstime: integer;
    procedure UpdateTap();
  public
    procedure DBPaint(Sender: TObject);
    procedure DBMouseUP(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Tick := 0;
  EPresstime := 0;
  DB := TDrawingBox.Create(Self);
  DB.FillParent(DrawingPanel);
  DB.OnPaint := @DBPaint;
  DB.OnMouseDown := @Image1MouseDown;
  DB.OnMouseUp := @DBMouseUP;
  DB.OnMouseMove := @DBMouseMove;
  DB.OnKeyPress := @DBPress;
  GUICanvas := TGUICanvas.Create(DB.Canvas, Memo1);
  App := TNeemaGUI.Create(GUICanvas);
  App.SetMemo(Memo1);
  App.CreateEmptyDrawing();
  UpdateTap();
  //App.ImportPython();
  //SplashForm.Show;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  App.Free;
  GUICanvas.Free;
  WriteMessage('FormDestroy');
end;

procedure TForm1.DeleteMenuItemClick(Sender: TObject);
begin
  WriteLn(Sender.ClassName);
  WriteMessage('DeleteMenuItemClick');
  App.DeletSelectedElement();
end;

procedure TForm1.LFResultMenuItemClick(Sender: TObject);
begin
App.ShowResultForm();
end;

procedure TForm1.T3wToolMenuItemClick(Sender: TObject);
begin
App.ShowT3WTools();
end;

procedure TForm1.NewMIClick(Sender: TObject);
begin
  WriteMessage('newenuItemClick');
  App.CreateEmptyDrawing();
  UpdateTap();
end;

procedure TForm1.SearchMenuItemClick(Sender: TObject);
begin
  App.Search();
end;

procedure TForm1.SearchNextMenuItemClick(Sender: TObject);
begin
  app.SearchNext();
end;

procedure TForm1.SEMenuItemClick(Sender: TObject);
begin
  WriteMessage('SEMenuItemClick');
  App.ShowSEOption();
end;


procedure TForm1.SolveMenuItemClick(Sender: TObject);
begin
  WriteMessage('SolveMenuItemClick');
  App.ShowSolveOption();
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  WriteMessage('TabControl1Change');
  App.SetActiveDrawing(TabControl1.TabIndex);
end;

procedure TForm1.TBus_TBClick(Sender: TObject);
begin
  WriteMessage('TBus_TBClick ' + (Sender as TToolButton).Name);
  App.SetNewElementClass(StringReplace((Sender as TToolButton).Name, '_TB', '', []));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := DB.Width;
  Bitmap.Height := DB.Height;
  Bitmap.SetSize(DB.Width, DB.Height);
  App.SetCanvasCanvas(Bitmap.Canvas, DB.Width, DB.Height);
  App.UpdateDrawingConectivity();
  Inc(Tick);
  App.UpdateDrawing(Tick mod 2 = 0);
  DB.Canvas.Draw(0, 0, Bitmap);
  Bitmap.Free;
  //Invalidate;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // app.Resize(Image1, DrawingPanel);
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  UseInt(X);
  UseInt(Y);
  UseObject(Sender);
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  NewTime: TDateTime;
begin
  WriteMessage('Image1MouseDown ' + X.ToString() + ' ' + y.ToString() +
    ' ' + BoolToStr(ssShift in Shift, True));
  DB.SetFocus;
  NewTime := Now;
  if (MilliSecondsBetween(LastImage1MouseDownTime, NewTime) < 500) and
    (LastImage1MouseMoveX = X) and (LastImage1MouseMoveY = Y) then
    App.DrawingDoublClick(X, Y)
  else
    App.DrawingClick(X, Y);
  LastImage1MouseDownTime := NewTime;
end;

procedure TForm1.ConectMenuItemClick(Sender: TObject);
begin
  WriteMessage('ConectMenuItemClick');
  App.ConectSelectedElement();
end;

procedure TForm1.ClearSelectionMenuItemClick(Sender: TObject);
begin
  WriteMessage('ClearSelectionMenuItemClick');
  App.ClearSelection();
end;

procedure TForm1.ClearPutElementMenuItemClick(Sender: TObject);
begin
  WriteMessage('ClearPutElementMenuItemClick');
  App.ClearPutElement();
end;

procedure TForm1.ClearBothMenuItemClick(Sender: TObject);
begin
  WriteMessage('ClearBothMenuItemClick');
  app.ClearPutElement();
  App.ClearSelection();
end;

procedure TForm1.DBPress(Sender: TObject; var Key: char);
begin
  WriteMessage('DBPress' + key);
  //Memo1.Append(Sender.ClassName);
   if EPresstime > 0 then
    UseObject((Sender));
  Inc(EPresstime);
  App.KeyPress(Key);
end;

procedure TForm1.DBPaint(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := DB.Width;
  Bitmap.Height := DB.Height;
  //Bitmap.Canvas.Pen.Color:=clRed;
  //Bitmap.Canvas.Rectangle(0,0,300,300);
  App.SetCanvasCanvas(Bitmap.Canvas, DB.Width, DB.Height);
  App.UpdateDrawing(False);
  DB.Canvas.Draw(0, 0, Bitmap);
  Bitmap.Free;
end;

procedure TForm1.OpenMIClick(Sender: TObject);
var
  k: integer;
begin
  WriteMessage('OpenMIClick');
  if OpenDialog1.Execute then
  begin
    if OpenDialog1.Files.Count > 50 then
    begin
      ShowMessage('two many files');
      Exit;
    end;
    for k := 0 to OpenDialog1.Files.Count - 1 do
    begin
      App.OpenFile(OpenDialog1.Files[k]);
      WriteMessage(' App.OpenFile ' + OpenDialog1.Files[k]);
    end;
  end;
  UpdateTap();
end;

procedure TForm1.SaveMIClick(Sender: TObject);
var
  ErrorMessage: string;
begin
  WriteMessage('SaveMIClick');
  if SaveDialog1.Execute then
  begin
    WriteMessage('App.SaveToFile ' + SaveDialog1.FileName);
    ErrorMessage := App.SaveToFile(SaveDialog1.FileName);
    if not (ErrorMessage.IsEmpty) then
      ShowMessage(ErrorMessage);
  end;
end;

procedure TForm1.UpdateTap();
begin
  WriteMessage('UpdateTap');
  App.GetDrawingNames(TabControl1.Tabs);
  TabControl1.TabIndex := App.GetActiveDrawingIndex();
end;

procedure TForm1.DBMouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  WriteMessage('DBMouseUP ' + X.ToString() + ' ' + y.ToString() +
    ' ' + BoolToStr(ssShift in Shift, True));
  UseInt(X);
  UseInt(Y);
  UseObject(Sender);
  UseInt(Ord(Button));
end;

procedure TForm1.DBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  //WriteMessage('DBMouseMove ' + X.ToString() + ' ' + y.ToString() +
  //  ' ' + BoolToStr(ssShift in Shift, True));
  if ssShift in Shift then
    App.DrawingMouseMove(LastImage1MouseMoveX - X, LastImage1MouseMoveY - Y, True);
  LastImage1MouseMoveX := X;
  LastImage1MouseMoveY := Y;
end;

end.
