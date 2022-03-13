unit StringSplitPatchUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
    function StringSplitPatch(Self2: string; const Separators: array of string; Options: TStringSplitOptions): TStringArray;
implementation
     //to fix RTL Bug
function StringSplitPatch(Self2: string; const Separators: array of string; AQuoteStart, AQuoteEnd: char;
  ACount: SizeInt; Options: TStringSplitOptions): TStringArray;
const
  BlockSize = 10;

  function NextSep(StartIndex: SizeInt; out Match: SizeInt): SizeInt;

  begin
    if (AQuoteStart <> #0) then
      Result := Self2.IndexOfAnyUnQuoted(Separators, AQuoteStart, AQuoteEnd, StartIndex, Match)
    else
      Result := Self2.IndexOfAny(Separators, StartIndex, Self2.Length, Match);
    if Result <> -1 then;
  end;

  procedure MaybeGrow(Curlen: SizeInt);

  begin
    if System.Length(Result) <= CurLen then
      SetLength(Result, System.Length(Result) + BlockSize);
  end;

var
  Sep, LastSep, Len, Match: SizeInt;
  T: string;

begin
  SetLength(Result, BlockSize);
  Len := 0;
  LastSep := 0;
  Sep := NextSep(0, Match);
  while (Sep <> -1) and ((ACount = 0) or (Len < ACount)) do
  begin
    T := Self2.SubString(LastSep, Sep - LastSep);
    if (T <> '') or (not (TStringSplitOptions.ExcludeEmpty = Options)) then
    begin
      MaybeGrow(Len);
      Result[Len] := T;
      Inc(Len);
    end;
    LastSep := Sep + System.Length(Separators[Match]);
    Sep := NextSep(LastSep, Match);
  end;
  if (LastSep <= Self2.Length) and ((ACount = 0) or (Len < ACount)) then
  begin
    T := Self2.SubString(LastSep);
    //    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    if (T <> '') or (not (TStringSplitOptions.ExcludeEmpty = Options)) then
    begin
      MaybeGrow(Len);
      Result[Len] := T;
      Inc(Len);
    end;
  end;                         //,#0,#0,A.Tokens[k].Length+1
  SetLength(Result, Len);
end;
function StringSplitPatch(Self2: string; const Separators:
  array of string; Options: TStringSplitOptions): TStringArray;
begin
 Result:=StringSplitPatch(Self2,Separators,#0,#0,Self2.Length+1,Options);
end;
end.

