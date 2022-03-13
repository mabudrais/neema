unit ImpedanceUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex;

type

  { TImpedance }

  TImpedance = class(TElicment)
  public
    Impedance: complex;
    function GetMaxConectioNum(): integer; override;
  end;

implementation

{ TImpedance }

function TImpedance.GetMaxConectioNum(): integer;
begin
  Result := 3;
end;

end.
