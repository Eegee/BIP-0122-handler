program bip0122handlertests;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  bip0122uritestcase, explorersprocessortestcase;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='bip0122handlertests';
  Application.Run;
  Application.Free;
end.
