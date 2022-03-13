unit WritlnUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure WL(a: string);
procedure W(a: string);
procedure WL(a: array of string);
procedure W(a: array of string);

implementation

procedure WL(a: string);
begin
    {$ifdef CONSOLE}
  WriteLn(a);
    {$endif}
end;

procedure W(a: string);
begin
      {$ifdef CONSOLE}
  Write(a);
    {$endif}
end;

procedure WL(a: array of string);
var
  k: integer;
begin
    {$ifdef CONSOLE}
  w(a);
  WriteLn();
        {$endif}
end;

procedure W(a: array of string);
var
  k: integer;
begin
     {$ifdef CONSOLE}
  for k := 0 to Length(a) - 1 do
    Write(a[k]);
        {$endif}
end;

end.
