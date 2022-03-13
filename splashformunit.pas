unit SplashFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Label1: TLabel;
  private

  public

  end;

var
  SplashForm: TSplashForm;

implementation
  uses Unit1;
{$R *.lfm}

{ TSplashForm }

end.

