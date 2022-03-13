unit DrawingBoxUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Controls,ComCtrls,ExtCtrls;
type

 { TDrawingBox }

 TDrawingBox = class(TCustomControl)
  private

  protected

  public
   procedure FillParent(P:TPanel);
  published
    property Anchors;
    property DoubleBuffered;
    property OnResize;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
  end;
implementation

{ TDrawingBox }

procedure TDrawingBox.FillParent(P: TPanel);
begin
  Parent:=P;
  DoubleBuffered := True;
  ControlStyle := Self.ControlStyle - [csNoFocus];
  self.AnchorSide[akLeft].Side := asrLeft;
  self.AnchorSide[akLeft].Control := p;
  self.Anchors := self.Anchors + [akLeft];

   self.AnchorSide[akRight].Side := asrRight;
  self.AnchorSide[akRight].Control := p;
  self.Anchors := self.Anchors + [akRight];

    self.AnchorSide[akTop].Side := asrTop;
  self.AnchorSide[akTop].Control := p;
  self.Anchors := self.Anchors + [akTop];

  self.AnchorSide[akBottom].Side := asrBottom;
  self.AnchorSide[akBottom].Control := p;
  self.Anchors := self.Anchors + [akBottom];
end;

end.

