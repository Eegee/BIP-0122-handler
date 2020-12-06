program bip0122handler;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Generics.Collections, Bip0122Uriparser, Bip0122Processor, Explorersprocessor,
  UnitTranslator, lclintf, LazFileUtils;

type
  TOptionsDictionary = TDictionary<string, string>;

  { TBIP0122Application }

  TBIP0122Application = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  private
    Options: TOptionsDictionary;
    procedure CreateOptions;
    function GetShortOptionsList: String;
    function GetLongOptionsList: TStringArray;
    procedure Process(SettingExplorerNames: TStrings; BipProcessor: TBIP0122Processor; const BipUri: TBIP0122URI);
    procedure WriteHelpPleaseSetExplorers(ExplorerNames: TStrings);
  end;

const
  ExplorersJsonFileName = 'block-explorers.json';
  ShortOptionHelp = 'h';
  ShortOptionSetExplorers = 's';
  ShortOptionGetExplorers = 'g';
  ShortOptionClearExplorers = 'x';
  ShortOptionLang = 'l';

resourcestring
  SExplorersNotSet = 'Block explorer(s) not yet set, cannot continue';
  SExplorersSet = 'Block explorer(s) set to the following:';
  SExplorersCleared = 'Block explorers cleared. You may set them again with %s';
  SHelpDisplayThisHelp = 'display this help and exit';
  SHelpGetExplorers = 'gets the list of currently used block explorers';
  SHelpParamOption = '[option]';
  SHelpParamURI = '[URI]';
  SHelpSetExplorers = 'sets comma-separated list of used explorers. ' + LineEnding + 'Use names from %s';
  SHelpClearExplorers = 'clear all used block explorers';
  SHelpUsage = 'Usage: ';
  SHelpWhereOptionIs = 'where [option] is one of the following:';
  SHelpWhereURIIs = 'where [URI] is a BIP0122 URI, or';
  SPleaseSetExplorers = 'Please set your desired block explorer(s) by using -' + ShortOptionSetExplorers + ' "comma-separated list containing selection of explorers below"';
  SUnhandledException = 'Unhandled %s exception: %s';
  SWarningUnknownExplorers = 'Warning: unknown explorers specified. Please use names from %s';

{ TBIP0122Application }

procedure TBIP0122Application.DoRun;
var
  ErrorMsg: String;
  NonOptions: TStringArray;
  NonOption: String;
  ShortOptions: String;
  LongOptions: TStringArray;
  BipUri: TBIP0122URI = nil;
  BipProcessor: TBIP0122Processor;
  ExplorersProcessor: TExplorersProcessor;
  ExplorerStorage: TRegistryExplorerStorage;
  SoftwareName: String;
  OptionExplorers: String = '';
  ShowHelp: Boolean = false;
  RegistryKey: String;
begin
  {$if declared(useHeapTrace)}
  SetHeapTraceOutput('heaptrace.log');
  globalSkipIfNoLeaks := true;
  {$endIf}

  ShortOptions := GetShortOptionsList();
  LongOptions := GetLongOptionsList();

  // quick check parameters
  ErrorMsg := CheckOptions(ShortOptions, LongOptions);
  if ErrorMsg<>'' then
  begin
    Writeln(stderr, ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption(ShortOptionHelp, Options[ShortOptionHelp]) then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  SoftwareName := ExtractFileName(ExtractFileNameWithoutExt(ExeName));
  RegistryKey := '\SOFTWARE\' + SoftwareName;

  try
    BipProcessor := TBip0122Processor.Create([
      ExplorersJsonFileName,
      GetAppConfigDir(true) + ExplorersJsonFileName,
      GetAppConfigDir(false) + ExplorersJsonFileName
    ]);
    ExplorerStorage := TRegistryExplorerStorage.Create(RegistryKey);
    try
      ExplorersProcessor := TExplorersProcessor.Create(BipProcessor.GetExplorerNames, ExplorerStorage);

      if HasOption(ShortOptionSetExplorers, Options[ShortOptionSetExplorers]) then
      begin
        OptionExplorers := GetOptionValue(ShortOptionSetExplorers, Options[ShortOptionSetExplorers]);
        ExplorersProcessor.SetExplorers(OptionExplorers);

        if ExplorersProcessor.ListOfUnknownExplorers.Count > 0 then
        begin
          writeln(Format(SWarningUnknownExplorers, [ExplorersJsonFileName]),
            LineEnding, ExplorersProcessor.ListOfUnknownExplorers.CommaText);
        end;
        if ExplorersProcessor.ListOfKnownExplorers.Count > 0 then
        begin
          writeln(SExplorersSet, LineEnding, ExplorersProcessor.ListOfKnownExplorers.CommaText);
        end
        else
        begin
          WriteHelpPleaseSetExplorers(ExplorersProcessor.ListOfAllExplorers);
        end;
      end
      else if HasOption(ShortOptionClearExplorers, Options[ShortOptionClearExplorers]) then
      begin
        ExplorersProcessor.SetExplorers('');
        writeln(Format(SExplorersCleared, ['-' + ShortOptionSetExplorers]));
      end
      else
      begin
        ExplorersProcessor.GetExplorers;

        if ExplorersProcessor.ListOfKnownExplorers.DelimitedText = '' then
        begin
          writeln(SExplorersNotSet);
          WriteHelpPleaseSetExplorers(ExplorersProcessor.ListOfAllExplorers);
        end
        else if HasOption(ShortOptionGetExplorers, Options[ShortOptionGetExplorers]) then
        begin
          writeln(ExplorersProcessor.ListOfKnownExplorers.CommaText);
        end
        else
        begin
          if ExplorersProcessor.ListOfUnknownExplorers.Count > 0 then
          begin
            writeln(Format(SWarningUnknownExplorers, [ExplorersJsonFileName]),
              LineEnding, ExplorersProcessor.ListOfUnknownExplorers.CommaText);
          end;

          NonOptions := GetNonOptions(ShortOptions, LongOptions);
          ShowHelp := True;
          if Length(NonOptions) > 0 then
          begin
            for NonOption in NonOptions do
            begin
              if TBIP0122URI.TryParse(NonOption, BipUri) then
              begin
                ShowHelp := False;
                Process(ExplorersProcessor.ListOfKnownExplorers, BipProcessor, BipUri);
                BipUri.Free;
                break;
              end
            end
          end;

          if ShowHelp then
            WriteHelp;
        end
      end
    finally
      FreeAndNil(ExplorersProcessor);
      FreeAndNil(BipProcessor);
    end;
  except
    on E: Exception do
    begin
      Writeln(stderr, Format(SUnhandledException, [E.ClassName, E.Message]));
    end;
  end;

  Terminate;
  Exit;
end;

constructor TBIP0122Application.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  CreateOptions;
end;

destructor TBIP0122Application.Destroy;
begin
  FreeAndNil(Options);
  inherited Destroy;
end;

procedure TBIP0122Application.WriteHelp;
begin
  writeln(SHelpUsage + ExtractFileName(ExeName) + ' ' + SHelpParamOption + ' ' + SHelpParamURI);
  writeln;
  writeln(SHelpWhereURIIs);
  writeln(SHelpWhereOptionIs);
  writeln(' -' + ShortOptionHelp, ' --' + Options[ShortOptionHelp], #9, #9, SHelpDisplayThisHelp);
  writeln(' -' + ShortOptionSetExplorers, ' --' + Options[ShortOptionSetExplorers], #9, StringReplace(Trim(Format(SHelpSetExplorers, [ExplorersJsonFileName])), LineEnding, LineEnding + #9#9#9, [rfReplaceAll]));
  writeln(' -' + ShortOptionGetExplorers, ' --' + Options[ShortOptionGetExplorers], #9, SHelpGetExplorers);
  writeln(' -' + ShortOptionClearExplorers, ' --' + Options[ShortOptionClearExplorers], #9, SHelpClearExplorers);
end;

procedure TBIP0122Application.WriteHelpPleaseSetExplorers(ExplorerNames: TStrings);
var
  ExplorerName: String;
begin
  writeln(SPleaseSetExplorers);
  for ExplorerName in ExplorerNames do
    writeln(ExplorerName);
end;

function TBIP0122Application.GetShortOptionsList: String;
begin
  Result := string.Join('', Options.Keys.ToArray);
end;


procedure TBIP0122Application.CreateOptions;
begin
  Options := TOptionsDictionary.Create;
  Options.Add(ShortOptionHelp, 'help');
  Options.Add(ShortOptionSetExplorers, 'setexplorers');
  Options.Add(ShortOptionGetExplorers, 'getexplorers');
  Options.Add(ShortOptionClearExplorers, 'clearexplorers');
  Options.Add(ShortOptionLang, 'lang');
end;

function TBIP0122Application.GetLongOptionsList: TStringArray;
var
  Value: String;
  i: Integer;
begin
  i := 0;
  Result := TStringArray.Create;
  SetLength(Result, Options.Values.Count);
  for Value in Options.Values do
  begin
    Result[i] := Value;
    i := i + 1;
  end;
end;

procedure TBIP0122Application.Process(SettingExplorerNames: TStrings; BipProcessor: TBIP0122Processor; const BipUri: TBIP0122URI);
var
  ResultUrl: String;
  ExplorerName: String;
begin
  for ExplorerName in SettingExplorerNames do
  begin
    ResultUrl := BipProcessor.GetUrlForBipUri(ExplorerName, BipUri);
    if ResultUrl <> '' then
    begin
      OpenUrl(ResultUrl);
    end;
  end;
end;

var
  Application: TBIP0122Application;

{$R *.res}

begin
  Application := TBIP0122Application.Create(nil);
  Application.Title:='BIP-0122 handler';
  Application.Run;
  Application.Free;
end.

