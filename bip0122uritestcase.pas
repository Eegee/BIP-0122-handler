unit bip0122uritestcase;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  Generics.Collections,
  bip0122uriparser;

type

  TBIP0122URITestCase = class(TTestCase)
  published
    procedure TestTryParse;
  end;

implementation

procedure TBIP0122URITestCase.TestTryParse;
var
  InputUri: string;
  ExpectedUri: TBIP0122URI;
  ChainExpectedUri: TBIP0122URI;
  AddressExpectedUri: TBIP0122URI;
  BlockExpectedUri: TBIP0122URI;
  ActualUri: TBIP0122URI;
  TestCases: TDictionary<string, TBIP0122URI>;
  TestCaseUri: TBIP0122URI;
  Key: string;
begin
  TestCases := TDictionary<string, TBIP0122URI>.Create;
  ExpectedUri := TBIP0122URI.Create;
  ExpectedUri.Chain := '';
  ExpectedUri.UriType := 'tx';
  ExpectedUri.Argument := 'b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da';

  ChainExpectedUri := TBIP0122URI.Create;
  ChainExpectedUri.Chain := '000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943';
  ChainExpectedUri.UriType := 'tx';
  ChainExpectedUri.Argument := '3b95a766d7a99b87188d6875c8484cb2b310b78459b7816d4dfc3f0f7e04281a';

  AddressExpectedUri := TBIP0122URI.Create;
  AddressExpectedUri.Chain := '';
  AddressExpectedUri.UriType := 'address';
  AddressExpectedUri.Argument := '16EW6Rv9P9AxFDBrZV816dD4sj1EAYUX3f';

  BlockExpectedUri := TBIP0122URI.Create;
  BlockExpectedUri.Chain := '';
  BlockExpectedUri.UriType := 'block';
  BlockExpectedUri.Argument := '00000000000000000119af5bcae2926df54ae262e9071a94a99c913cc217cc72';

  TestCases.Add('blockchain:/tx/b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da', ExpectedUri); // A transaction on Bitcoin main net
  TestCases.Add('BLOCKCHAIN:/tx/b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da', ExpectedUri); // Protocol is case insensitive
  TestCases.Add('blockchain:/TX/b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da', ExpectedUri); // UriType is case insensitive
  TestCases.Add('blockchain://000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943/tx/3b95a766d7a99b87188d6875c8484cb2b310b78459b7816d4dfc3f0f7e04281a', ChainExpectedUri); // A transaction on Bitcoin test net

  TestCases.Add('blockchain:/address/16EW6Rv9P9AxFDBrZV816dD4sj1EAYUX3f', AddressExpectedUri); // An address on Bitcoin main net
  TestCases.Add('blockchain:/aDdReSs/16EW6Rv9P9AxFDBrZV816dD4sj1EAYUX3f', AddressExpectedUri); // An address on Bitcoin main net, case insensitive

  TestCases.Add('blockchain:/block/00000000000000000119af5bcae2926df54ae262e9071a94a99c913cc217cc72', BlockExpectedUri); // A block on Bitcoin main net

  TestCases.Add('bitcoin:/tx/b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da', nil); // Fails, does not start with blockchain:/
  TestCases.Add('blockchain:/unknown/b462ae6eb8bdae2e060239a2a3ea5d9c3e0f9ef34d9717beb2dcf0ed42cee7da', nil); // Fails, doesn't contain a known UriType

  for Key in TestCases.Keys do
  begin
    InputUri := Key;
    TestCaseUri := TestCases[Key];
    if TBIP0122URI.TryParse(InputUri, ActualUri) then
    begin
      AssertEquals('Chain for ' + Key + ': ', TestCaseUri.Chain, ActualUri.Chain);
      AssertEquals('UriType for ' + Key + ': ', TestCaseUri.UriType, ActualUri.UriType);
      AssertEquals('Argument for ' + Key + ': ', TestCaseUri.Argument, ActualUri.Argument);
    end
    else if TestCaseUri = nil then
    begin
      AssertTrue(ActualUri = nil);
    end
    else
      Fail('Test case for ' + InputUri + ' failed');
  end;
end;

initialization

  RegisterTest(TBIP0122URITestCase);
end.

