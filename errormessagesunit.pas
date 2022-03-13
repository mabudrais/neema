unit ErrorMessagesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TErrorMessage }

  TErrorMessage = class
    Abstract
    function GetString(): string; virtual;
  end;

  { TBCToBEM }

  TBCToBEM = class(ErrorMessagesUnit.TErrorMessage)// busbar conected to busbar error meassge
  private
    FID1: integer;
    FID2: integer;
  public
    property ID1: integer read FID1;
    property ID2: integer read FID2;
    constructor Create(cID1, cID2: integer);
    function GetString(): string; override;
  end;

  { TErrorMessageList }

  TErrorMessageList = class
  private
    List: TObjectList;
    function GetCount: integer;
  public
    constructor Create;
    property Count: integer read GetCount;
  end;

implementation

{ TBCToBEM }

constructor TBCToBEM.Create(cID1, cID2: integer);
begin
  FID1 := cID1;
  FID2 := cID2;
end;

function TBCToBEM.GetString(): string;
begin
  Result:='Bus '+ID1.ToString()+'conected to bus '+ID2.ToString();
end;

{ TErrorMessage }

function TErrorMessage.GetString(): string;
begin
  raise Exception.Create('not implemented');
  Result := '';
end;

{ TErrorMessageList }

function TErrorMessageList.GetCount: integer;
begin
  Result := List.Count;
end;

constructor TErrorMessageList.Create;
begin
  List := TObjectList.Create;
end;

end.
