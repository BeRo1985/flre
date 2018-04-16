program FLRETest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}
{$ifdef win64}
 {$apptype console}
{$endif}

uses
  Classes,
  FLRE in '..\FLRE.pas',
  PUCU in '..\PUCU.pas',
  UnitTestGlobals in 'UnitTestGlobals.pas',
  UnitSearchTests in 'UnitSearchTests.pas',
  UnitReplaceTests in 'UnitReplaceTests.pas';

begin
 ExecuteSearchTests;
 ExecuteReplaceTests;
{$ifndef fpc}
 if DebugHook<>0 then begin
  readln;
 end;
{$endif}
end.
