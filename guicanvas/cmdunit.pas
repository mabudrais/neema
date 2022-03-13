unit CmdUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Dialogs,StdCtrls;
  procedure SetMemoText(t:String);
  procedure ShowDialogText(Text:String);
  var
    Memo:TMemo;
implementation

procedure SetMemoText(t: String);
begin
  Memo.Append(T);
end;

procedure ShowDialogText(Text: String);
begin
  ShowMessage(Text);
end;

end.

