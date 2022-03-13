unit NeemaDialogMessageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs;
         procedure ShowNeemaDialogMessage(m: string);
implementation

procedure ShowNeemaDialogMessage(m: string);
begin
     ShowMessage(m);
end;

end.

