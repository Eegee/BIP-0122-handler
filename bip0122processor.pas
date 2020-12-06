unit Bip0122Processor;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson, jsonparser, jsonscanner, httpprotocol,
  Generics.Collections,
  Bip0122Uriparser;

const
  BitcoinMainNet = '000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f';

type
  TBlockExplorer = record
    Name: String;
    TxUrl: String;
    BlockUrl: String;
    HeightUrl: String;
    AddressUrl: String;
    constructor Create(const name: String; const tx: String; const block: String; const height: String; const address: String);
  end;

  TBlockExplorers = TDictionary<String, TBlockExplorer>;

  TChain = class
  public
    constructor Create(const name: String; const hash: String; const explorers: TBlockExplorers);
    destructor Destroy; override;
  private
    FName: String;
    FHash: String;
    FExplorers: TBlockExplorers;
  end;

  TChains = class(TObjectDictionary<String, TChain>)
  public
    constructor Create; override;
  end;

  TBIP0122Processor = class
  public
    constructor Create(const ExplorerJsonFilePaths: TStringArray);
    destructor Destroy; override;
    function GetUrlForBipUri(ExplorerName: String; Uri: TBIP0122URI): String;
    function GetExplorerNames: TStringArray;
  private
    FChains: TChains;
    procedure ParseJSONFiles(Filenames: TStringArray);
    procedure ParseJSONData(J: TJSONData);
  end;

implementation

  constructor TBlockExplorer.Create(const name: String; const tx: String; const block: String; const height: String; const address: String);
  begin
    self.Name := name;
    self.TxUrl := tx;
    self.BlockUrl := block;
    self.HeightUrl := height;
    self.AddressUrl := address;
  end;

  constructor TChain.Create(const name: String; const hash: String; const explorers: TBlockExplorers);
  begin
    self.FName := name;
    self.FHash := hash;
    self.FExplorers := explorers;
  end;

  destructor TChain.Destroy;
  begin
    if Assigned(self.FExplorers) then
      self.FExplorers.Free;
    inherited;
  end;

  constructor TChains.Create;
  begin
    inherited Create([doOwnsValues]);
  end;

  constructor TBIP0122Processor.Create(const ExplorerJsonFilePaths: TStringArray);
  begin
    FChains := TChains.Create();
    ParseJSONFiles(ExplorerJsonFilePaths);
  end;

  destructor TBIP0122Processor.Destroy;
  var
    Chain: TChain;
  begin
    FreeAndNil(FChains);
    inherited;
  end;

  function TBIP0122Processor.GetUrlForBipUri(ExplorerName: string; Uri: TBIP0122URI): String;
  var
    Url: String;
    Argument: String;
    HeightNumber: UInt64;
    Explorer: TBlockExplorer;
    Explorers: TBlockExplorers;
    ChainHash: String;
  begin
    ChainHash := Uri.Chain;
    ChainHash := IfThen<string>(ChainHash = '', BitcoinMainNet, ChainHash);
    if FChains.ContainsKey(ChainHash) then
    begin
      Explorers := FChains[ChainHash].FExplorers;
      if Explorers.ContainsKey(ExplorerName) then
      begin
        Explorer := Explorers[ExplorerName];
        if Uri.UriType = 'tx' then
        begin
          Url := Explorer.TxUrl;
        end
        else if Uri.UriType = 'address' then
        begin
          Url := Explorer.AddressUrl;
        end
        else if Uri.UriType = 'block' then
        begin
          if (Length(Uri.Argument) < 64) and (UInt64.TryParse(Uri.Argument, HeightNumber)) then
          begin
            Url := Explorer.HeightUrl;
          end
          else
          begin
            Url := Explorer.BlockUrl;
          end;
        end;
        Argument := httpprotocol.HTTPEncode(Uri.Argument);
        Result := Format(Url, [Argument]);
      end;
    end;
  end;

  function TBIP0122Processor.GetExplorerNames: TStringArray;
  var
    List: TStringList;
    ChainKey: String;
    Chain: TChain;
    i: integer;
  begin
    List := TStringList.Create;
    List.Duplicates := dupIgnore;
    List.Sorted := true;
    try
      for ChainKey in FChains.Keys do
      begin
        Chain := FChains[ChainKey];
        List.AddStrings(Chain.FExplorers.Keys.ToArray);
      end;
      SetLength(Result, List.Count);
      for i := 0 to List.Count -1  do
        Result[i] := List[i];
    finally
      List.Free;
    end;
  end;

  procedure TBIP0122Processor.ParseJSONFiles(FileNames: TStringArray);
  var
    Fs: TFileStream;
    P: TJSONParser;
    J: TJSONData;
    O: TJSONOptions;
    Filename: string;
  begin
    for Filename in FileNames do
    begin
      if FileExists(Filename) then
      begin
        Fs := TFileStream.Create(Filename, fmopenRead);
        try
          O := [joUTF8, joComments, joIgnoreTrailingComma];
          P := TJSONParser.Create(Fs, O);
          try
            J := P.Parse;
            if Assigned(J) then
            begin
               ParseJSONData(J);
            end;
          finally
            if Assigned(J) then
              J.Free;
            P.Free;
          end;
        finally
          Fs.Free;
        end;
      end;
    end;
  end;

  procedure TBIP0122Processor.ParseJSONData(J: TJSONData);
  var
    O: TJSONObject;
    JSONChains: TJSONArray;
    JSONChain: TJSONEnum;
    ChainObject: TJSONObject;
    Chain: TChain;
    ChainName: String;
    ChainHash: String;
    JSONExplorers: TJSONArray;
    JSONExplorer: TJSONEnum;
    ExplorerObject: TJSONObject;
    Explorers: TBlockExplorers;
    Explorer: TBlockExplorer;
    ExplorerName: String;
    DefaultChains: TJSONArray;
    DefaultExplorers: TJSONArray;
  begin
    O := TJSONObject(J);
    DefaultChains := TJSONArray.Create();
    JSONChains := O.Get('chains', DefaultChains);
    for JSONChain in JSONChains do
    begin
      ChainObject := TJSONObject(JSONChain.Value);
      ChainHash := ChainObject.Get('chain', '');
      if ChainHash <> '' then
      begin
        if FChains.ContainsKey(ChainHash) then
        begin
          Chain := FChains[ChainHash];
          Explorers := Chain.FExplorers;
        end
        else
        begin
          Explorers := TBlockExplorers.Create();
          ChainName := ChainObject.Get('name', '');
          Chain := TChain.Create(ChainName, ChainHash, Explorers);
          FChains.Add(ChainHash, Chain);
        end;
        DefaultExplorers := TJSONArray.Create();
        JSONExplorers := ChainObject.Get('block-explorers', DefaultExplorers);
        for JSONExplorer in JSONExplorers do
        begin
          ExplorerObject := TJSONObject(JSONExplorer.Value);
          ExplorerName := ExplorerObject.Get('name', '');
          if ExplorerName <> '' then
          begin
            Explorer := TBlockExplorer.Create(
               ExplorerName,
               ExplorerObject.Get('tx', ''),
               ExplorerObject.Get('block', ''),
               ExplorerObject.Get('height', ''),
               ExplorerObject.Get('address', '')
            );

            if Explorers.ContainsKey(ExplorerName) then
            begin
              Explorers[ExplorerName] := Explorer;
            end
            else
            begin
              Explorers.Add(ExplorerName, Explorer);
            end;
          end;
        end;
        FreeAndNil(DefaultExplorers);
      end;
    end;
    FreeAndNil(DefaultChains);
  end;

end.
