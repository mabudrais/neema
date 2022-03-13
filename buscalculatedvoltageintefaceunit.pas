unit busCalculatedVoltageintefaceunit;

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
interface

uses
  Classes, SysUtils,ucomplex;

type
  IBCVI = interface
    ['{575B9EC4-39AC-4511-A792-70B0C044414B}']
     function GCVPerUnit(aID:Integer):complex;
     function GCVByIndex(aIndex:Integer;out V:complex):Boolean;
     function GIName():String;
  end;

implementation

end.
