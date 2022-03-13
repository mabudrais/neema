unit LoadUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ElectricElentUnit, UComplex;

type

  { TLoad }

  TLoad = class(TElicment)
  private
    FP,FQ: String;
  public
    function GetMaxConectioNum(): Integer; override;
    procedure SetLoad(cP,CQ:Extended);
  published
    property P: String read FP write FP;
    property Q:String read FQ write FQ;
  end;

implementation

{ TLoad }

function TLoad.GetMaxConectioNum(): Integer;
begin
  Result := 1;
end;

procedure TLoad.SetLoad(cP, CQ: Extended);
begin
  P:=cp.ToString();
  Q:=cq.ToString();
end;

end.
