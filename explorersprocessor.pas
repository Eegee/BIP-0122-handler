unit explorersprocessor;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

interface

uses
  Classes, SysUtils, registry;

type
  IExplorerStorage = interface
    procedure ReadConfig(ExplorerNames: TStrings);
    procedure SaveConfig(ExplorerNames: TStrings);
    procedure ClearConfig;
  end;

  TRegistryExplorerStorage = class(TInterfacedObject, IExplorerStorage)
  private
    FRegistryKey: String;
  public
    procedure ReadConfig(ExplorerNames: TStrings);
    procedure SaveConfig(ExplorerNames: TStrings);
    procedure ClearConfig;
    constructor Create(ARegistryKey: string);
  end;

  TExplorersProcessor = class
  private
    FExplorerStorage: IExplorerStorage;
    procedure CorrectExplorers(const ExplorerNames: TStrings);
  public
    ListOfAllExplorers: TStrings;
    ListOfKnownExplorers: TStringList;
    ListOfUnknownExplorers: TStringList;
    constructor Create(AListOfExplorers: TStringArray; AExplorerStorage: IExplorerStorage);
    destructor Destroy; override;

    procedure GetExplorers;
    procedure SetExplorers(InputExplorers: String);
  end;

function StringReplaceAll(const haystack: string; const needle: string; const replacement: string): string;

const
  SettingExplorers = 'Explorers';

implementation

constructor TExplorersProcessor.Create(AListOfExplorers: TStringArray; AExplorerStorage: IExplorerStorage);
begin
  FExplorerStorage := AExplorerStorage;

  ListOfAllExplorers := TStringList.Create;
  ListOfAllExplorers.AddStrings(AListOfExplorers);
  ListOfKnownExplorers := TStringList.Create;
  ListOfUnknownExplorers := TStringList.Create;
end;

destructor TExplorersProcessor.Destroy;
begin
  FreeAndNil(ListOfUnknownExplorers);
  FreeAndNil(ListOfKnownExplorers);
  FreeAndNil(ListOfAllExplorers);
  inherited Destroy;
end;

procedure TExplorersProcessor.GetExplorers;
var
  ReadExplorers: TStringList;
begin
  ReadExplorers := TStringList.Create;
  try
    FExplorerStorage.ReadConfig(ReadExplorers);
    ReadExplorers.Sort;
    CorrectExplorers(ReadExplorers);
  finally
    ReadExplorers.Free;
  end;
end;

procedure TExplorersProcessor.SetExplorers(InputExplorers: String);
var
  SplitExplorerNames: TStringList;
begin
  if Trim(InputExplorers) = '' then
  begin
    FExplorerStorage.ClearConfig;
  end
  else
  begin
    SplitExplorerNames := TStringList.Create;
    try
      InputExplorers := StringReplaceAll(InputExplorers, ' ,', ',');
      InputExplorers := StringReplaceAll(InputExplorers, ', ', ',');
      SplitExplorerNames.StrictDelimiter := true;
      SplitExplorerNames.DelimitedText := InputExplorers;
      SplitExplorerNames.Sort;
      CorrectExplorers(SplitExplorerNames);
    finally
      FreeAndNil(SplitExplorerNames);
    end;
  end;
end;

procedure TExplorersProcessor.CorrectExplorers(const ExplorerNames: TStrings);
var
  I: integer;
  ExplorerName: String;
  previousKnown: string;
begin
  previousKnown := ListOfKnownExplorers.DelimitedText;
  ListOfUnknownExplorers.Clear;
  ListOfKnownExplorers.Clear;
  for ExplorerName in ExplorerNames do
  begin
    I := ListOfAllExplorers.IndexOf(ExplorerName);
    if I = -1 then
    begin
      ListOfUnknownExplorers.Add(ExplorerName);
    end
    else
    begin
      ListOfKnownExplorers.Add(ListOfAllExplorers[I]);
    end;
  end;
  ListOfUnknownExplorers.Sort;
  ListOfKnownExplorers.Sort;

  if (ListOfUnknownExplorers.Count > 0) or (previousKnown <> ListOfKnownExplorers.DelimitedText) then
  begin
    FExplorerStorage.SaveConfig(ListOfKnownExplorers);
  end;
end;

function StringReplaceAll(const haystack: string; const needle: string; const replacement: string): string;
begin
  Result := haystack;
  repeat
    Result := StringReplace(Result, needle, replacement, [rfReplaceAll]);
  until Pos(needle, Result) = 0;
end;


constructor TRegistryExplorerStorage.Create(ARegistryKey: string);
begin
  FRegistryKey := ARegistryKey;
end;

procedure TRegistryExplorerStorage.ReadConfig(ExplorerNames: TStrings);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly(FRegistryKey) then
    begin
      Registry.ReadStringList(SettingExplorers, ExplorerNames);
    end;
  finally
    Registry.Free;
  end;
end;

procedure TRegistryExplorerStorage.SaveConfig(ExplorerNames: TStrings);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(FRegistryKey, true) then
    begin
      Registry.WriteStringList(SettingExplorers, ExplorerNames, True);
    end;
  finally
    Registry.Free;
  end;end;

procedure TRegistryExplorerStorage.ClearConfig;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey(FRegistryKey, true) then
    begin
      Registry.DeleteValue(SettingExplorers);
      if (not Registry.HasSubKeys) and (Length(Registry.GetValueNames) = 0) then
      begin
        Registry.DeleteKey(FRegistryKey);
      end;
    end;
  finally
    Registry.Free;
  end;
end;

end.

