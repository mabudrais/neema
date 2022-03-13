unit LabelUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, elementUnit, neemacanvasUnit;

type

  { TNeemaLabel }

  TNeemaLabel = class(TElement)
  private
    FColour: integer;
    FSize, TextWidth, TextHeight: integer;
    FText: string;
  public
    constructor Create; override;
    procedure Draw(const c: INeemaCanvas; Tick: boolean); override;
    function SetSelected(X, Y: integer): boolean; override;
  published
    property Text: string read FText write FText;
    property Size: integer read FSize write FSize;
    property Colour: integer read FColour write FColour;
  end;

implementation

{ TNeemaLabel }

constructor TNeemaLabel.Create;
begin
  inherited Create;
  Text := 'Text';
  Size := 12;
end;

procedure TNeemaLabel.Draw(const c: INeemaCanvas; Tick: boolean);
var
  OldFontColor: TNeemaColour;
  OldBrushColour: integer;
begin
  OldBrushColour := c.getBrushColor();
  OldFontColor := c.getFontColor;
  if (IsSelected) and (Tick) then
  begin
    c.setBrushColor(clYellow);
    c.FillRect(XPos - 2, YPos - 2, XPos + TextWidth + 2, YPos + TextHeight + 2);
  end;
  c.setBrushColor(clNemSilver);
  c.setFontColor(Colour);
  c.TextOut(XPos, YPos, Text);
  c.GetTextWidtrhHeight(Text, TextWidth, TextHeight);
  c.setBrushColor(OldBrushColour);
  c.setFontColor(OldFontColor);
end;

function TNeemaLabel.SetSelected(X, Y: integer): boolean;
begin
  Result := False;
  if ((X > XPos) and (X < XPos + TextWidth) and (Y < YPos + TextHeight) and
    (Y > YPos)) then
  begin
    IsSelected := True;
    Result := True;
  end;
end;

end.
