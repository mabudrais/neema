unit NeemaLogingUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

procedure WriteMessage(Msg: string);

implementation

var
  List: TFileStream;

procedure WriteMessage(Msg: string);
begin
  Msg:=TimeToStr(Now)+' '+Msg+LineEnding;
  List.Write(Msg[1],Length(Msg));
end;

initialization
  List := TFileStream.Create('log/Log-' + DateTimeToStr(Now).Replace(
    ':', '-').Replace('/', '-'), fmCreate);
end.
