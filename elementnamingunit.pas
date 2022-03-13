unit ElementNamingUnit;

{$mode ObjFPC}{$H+}
 {$Interfaces CORBA}
interface

uses
  Classes, SysUtils;
     type
       IElementNaming=interface
       function IsPossableName(aElementName:String):Boolean;
       end;

implementation

end.

