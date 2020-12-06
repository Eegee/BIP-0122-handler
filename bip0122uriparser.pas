unit Bip0122Uriparser;

{$ifdef fpc}{$mode delphi}{$endif}{$H+}

interface

uses
  Classes, SysUtils, uriparser;

type
  TBIP0122URI = class
  public
    Chain: string;
    UriType: string;
    Argument: string;
    class function TryParse(const uriString: string; Out uri: TBIP0122URI): Boolean;
  end;

implementation

class function TBIP0122URI.TryParse(const uriString: string; Out uri: TBIP0122URI): Boolean;
var
  ParsedUri: TURI;
  ValidPaths: TStrings;
  ValidPathsIndex: integer;
begin
  Result := false;
  uri := nil;
  ParsedUri := UriParser.ParseURI(uriString);
  if CompareText(ParsedUri.Protocol, 'blockchain') = 0 then
  begin
    ValidPaths := TStringList.Create;
    try
      ValidPaths.Add('address');
      ValidPaths.Add('block');
      ValidPaths.Add('tx');

      ValidPathsIndex := ValidPaths.IndexOf(ParsedUri.Path.Trim('/').ToLowerInvariant);
      if ValidPathsIndex > -1 then
      begin
        uri := TBIP0122URI.Create;
        uri.Chain := ParsedUri.Host;
        uri.UriType := ValidPaths[ValidPathsIndex];
        uri.Argument := ParsedUri.Document;
        Result := true;
      end;
    finally
      ValidPaths.Free;
    end;
  end;
end;

end.

