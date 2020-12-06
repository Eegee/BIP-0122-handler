unit explorersprocessortestcase;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  explorersprocessor;

type
  TExplorersProcessorTestCase = class(TTestCase)
  published
    procedure TestGetExplorersTwoItems;
    procedure TestGetExplorersEmpty;
    procedure TestGetExplorersUnknownExplorerInStorage;
    procedure TestGetExplorersRemovesUnknown;
    procedure TestGetExplorersSorts;
    procedure TestSetExplorersSecondItem;
    procedure TestSetExplorersIgnoresCase;
    procedure TestSetExplorersIgnoresSpaces;
    procedure TestSetExplorersClears;
  end;

  TExplorerStorageMock = class(TInterfacedObject, IExplorerStorage)
  private
    FDummyExplorerNames: TStrings;
  public
    constructor Create(DummyExplorerNames: TStrings);
    destructor Destroy; override;
    procedure ReadConfig(ExplorerNames: TStrings);
    procedure SaveConfig(ExplorerNames: TStrings);
    procedure ClearConfig;
  end;

implementation

constructor TExplorerStorageMock.Create(DummyExplorerNames: TStrings);
begin
  FDummyExplorerNames := TStringList.Create;
  if Assigned(DummyExplorerNames) then
  begin
    FDummyExplorerNames.AddStrings(DummyExplorerNames);
  end;
end;

destructor TExplorerStorageMock.Destroy;
begin
  FreeAndNil(FDummyExplorerNames);
  inherited Destroy;
end;

procedure TExplorerStorageMock.ReadConfig(ExplorerNames: TStrings);
begin
  if FDummyExplorerNames.Count > 0 then
  begin
    ExplorerNames.Clear;
    ExplorerNames.AddStrings(FDummyExplorerNames);
  end;
end;

procedure TExplorerStorageMock.SaveConfig(ExplorerNames: TStrings);
begin
  FDummyExplorerNames.DelimitedText := ExplorerNames.DelimitedText;
end;

procedure TExplorerStorageMock.ClearConfig;
begin
  FDummyExplorerNames.Clear;
end;

procedure TExplorersProcessorTestCase.TestGetExplorersTwoItems;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[0], ExplorersProcessor.ListOfKnownExplorers[0]);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[1]);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestGetExplorersEmpty;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  TestStrings := TStringList.Create;
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 0);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 0);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestGetExplorersUnknownExplorerInStorage;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  TestStrings.Add('Unknown');
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 1);
    AssertEquals(TestStrings[2], ExplorersProcessor.ListOfUnknownExplorers[0]);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestGetExplorersRemovesUnknown;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  TestStrings.Add('Unknown');
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 1);
    AssertEquals(TestStrings[2], ExplorersProcessor.ListOfUnknownExplorers[0]);

    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestGetExplorersSorts;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'Second';
  TestArray[1] := 'First';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[0]);
    AssertEquals(TestArray[0], ExplorersProcessor.ListOfKnownExplorers[1]);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestSetExplorersSecondItem;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
  InputExplorers: TStrings;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    InputExplorers := TStringList.Create;
    InputExplorers.Add('Second');

    ExplorersProcessor.SetExplorers(InputExplorers.CommaText);
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 1);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[0]);

    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 1);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[0]);
  finally
    FreeAndNil(InputExplorers);
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestSetExplorersIgnoresCase;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
  InputExplorers: TStrings;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    InputExplorers := TStringList.Create;
    InputExplorers.Add('SECOND');

    ExplorersProcessor.SetExplorers(InputExplorers.CommaText);
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 1);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[0]);
  finally
    FreeAndNil(InputExplorers);
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestSetExplorersIgnoresSpaces;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.SetExplorers('SECOND,  First');
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[0], ExplorersProcessor.ListOfKnownExplorers[0]);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[1]);

    ExplorersProcessor.SetExplorers('SECOND  ,First');
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
    AssertEquals(TestArray[0], ExplorersProcessor.ListOfKnownExplorers[0]);
    AssertEquals(TestArray[1], ExplorersProcessor.ListOfKnownExplorers[1]);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

procedure TExplorersProcessorTestCase.TestSetExplorersClears;
var
  ExplorersProcessor: TExplorersProcessor;
  TestArray: TStringArray;
  TestStrings: TStrings;
  ExplorerStorage: IExplorerStorage;
begin
  TestArray := TStringArray.Create;
  SetLength(TestArray, 2);
  TestArray[0] := 'First';
  TestArray[1] := 'Second';
  TestStrings := TStringList.Create;
  TestStrings.AddStrings(TestArray);
  ExplorerStorage := TExplorerStorageMock.Create(TestStrings);

  ExplorersProcessor := TExplorersProcessor.Create(TestArray, ExplorerStorage);
  try
    ExplorersProcessor.SetExplorers('');
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 0);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);

    ExplorersProcessor.GetExplorers;
    AssertTrue(ExplorersProcessor.ListOfAllExplorers.Count = 2);
    AssertTrue(ExplorersProcessor.ListOfKnownExplorers.Count = 0);
    AssertTrue(ExplorersProcessor.ListOfUnknownExplorers.Count = 0);
  finally
    FreeAndNil(ExplorersProcessor);
    FreeAndNil(TestStrings);
  end;
end;

initialization

  RegisterTest(TExplorersProcessorTestCase);
end.

