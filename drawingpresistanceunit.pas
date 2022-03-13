unit DrawingPresistanceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PersistentObjectListUnit;

type

  { TDrawingPresistance }

  TDrawingPresistance = class(TPersistedJsonObject)
  private
    FBackgroundColor: integer;
    FName: string;
    public
    function INI(D:TObject):TDrawingPresistance;
  published
    property Name: string read FName write FName;
    property BackgroundColor: integer read FBackgroundColor write FBackgroundColor;
  end;

implementation
  uses DrawingUnit;
{ TDrawingPresistance }

function TDrawingPresistance.INI(D: TObject): TDrawingPresistance;
var
  Drawing: TDrawing;
begin
  Drawing:=D as TDrawing;
  BackgroundColor:=Drawing.FBackgroundColor;
  Name:=Drawing.Name;
  Result:=Self;
end;

end.
