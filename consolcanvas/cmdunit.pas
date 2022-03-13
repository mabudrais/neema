unit CmdUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  procedure SetMemoText(t:String);
  procedure ShowDialogText(Text:String);
implementation

procedure SetMemoText(t: String);
begin
  WriteLn(t);
end;

procedure ShowDialogText(Text: String);
begin
  WriteLn(Text);
end;

end.

