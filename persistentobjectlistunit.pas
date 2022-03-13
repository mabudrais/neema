unit PersistentObjectListUnit;

{$mode objfpc}{$H+}
 {$interfaces corba}
interface

uses
  Classes, SysUtils, fpjsonrtti, contnrs;

type
  IObjectsList = interface
    ['{CEBF521E-E7CF-426F-A428-11FBC5AF60C1}']
    function GetNextItem(K: integer): TObject;
    function GetItemsCount(): integer;
    procedure AddNextItem(O: TObject);
  end;

  { TPersistedJsonObject }

  TPersistedJsonObject = class
    constructor Create; virtual;
  end;

  TPersistedJsonObjectClassOf = class of TPersistedJsonObject;
  arrayofTClass = array of TPersistedJsonObjectClassOf;
  { TPersistentObjectList }

  TPersistentObjectList = class(TObject)
  public
    procedure Save(Target: IObjectsList; Slist: TStrings);
    procedure SaveToFile(Target: IObjectsList; FileName: string);
    procedure Load(Slist: TStrings; Target: IObjectsList; T: arrayofTClass);
    procedure LoadFromFile(Target: IObjectsList; T: arrayofTClass; FileName: string);
  end;

implementation

{ TPersistedJsonObject }

constructor TPersistedJsonObject.Create;
begin

end;


procedure TPersistentObjectList.SaveToFile(Target: IObjectsList; FileName: string);
var
  Slist: TStringList;
begin
  Slist := TStringList.Create;
  Save(Target, Slist);
  try
    Slist.SaveToFile(FileName);
  finally
    Slist.Free;
  end;
end;

procedure TPersistentObjectList.LoadFromFile(Target: IObjectsList;
  T: arrayofTClass; FileName: string);
var
  Slist: TStringList;
begin
  Slist := TStringList.Create;
  Slist.LoadFromFile(FileName);
  try
    Load(Slist, Target, T);
  finally
    Slist.Free;
  end;
end;

procedure TPersistentObjectList.Save(Target: IObjectsList; Slist: TStrings);
var
  Streamer: TJSONStreamer;
  k: integer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    for k := 0 to Target.GetItemsCount() - 1 do
    begin
      Slist.Add(Target.GetNextItem(k).ClassName);
      Slist.Add(Streamer.ObjectToJSONString(Target.GetNextItem(k)));
    end;
  finally
    Streamer.Free;
  end;
end;
procedure TPersistentObjectList.Load(Slist: TStrings; Target: IObjectsList;
  T: arrayofTClass);
var
  k, m, TypeIndex: integer;
  TypeFound: boolean;
  DeStreamer: TJSONDeStreamer;
  c: TPersistedJsonObjectClassOf;
  O: TPersistedJsonObject;
begin
  if (Slist.Count mod 2) <> 0 then
    raise Exception.Create('Slist contain odd Number of Lines');
  k := 0;
  DeStreamer := TJSONDeStreamer.Create(nil);
  //DeStreamer.BeforeReadObject:=@Self.BeforeReadingObject;
  while k < Slist.Count do
  begin
    TypeFound := False;
    for m := 0 to Length(T) - 1 do
    begin
      if T[m].ClassName = Slist[k] then
      begin
        TypeFound := True;
        TypeIndex := m;
        Break;
      end;
    end;
    if not TypeFound then
      raise Exception.Create('type ' + Slist[k] + ' not found');
    c := T[TypeIndex];
    O := c.Create;
    DeStreamer.JSONToObject(Slist[k + 1], O);
    Target.AddNextItem(o);
    Inc(k, 2);
    //WriteLn(round(k*100/Slist.count));
  end;
end;

end.
