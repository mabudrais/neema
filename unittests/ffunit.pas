unit FFunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UComplex;

function F(V: extended; FO: integer): string;
function FA(V: complex; FO: integer): string;

implementation

function F(V: extended; FO: integer): string;
begin
  Result := FormatFloat('###.##', V);
end;

function FA(V: complex; FO: integer): string;
begin
  Result := FormatFloat('###.##', cmod(V));
  Result := Result + '<' + FormatFloat('###.##', carg(V) * 180 / pi);
end;

end.
