unit SolverUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSolver }

  TSolver = class
  public
    function Solve(Drawing: TObject; Option: string; AdditionData: TObject): boolean;
      virtual;
    procedure GetErrorMassages(ErrorMassages: TStringList); virtual;
  end;

  TSolverClass = class of TSolver;

implementation

{ TSolver }

function TSolver.Solve(Drawing: TObject; Option: string;
  AdditionData: TObject): boolean;
begin

end;

procedure TSolver.GetErrorMassages(ErrorMassages: TStringList);
begin

end;

end.
