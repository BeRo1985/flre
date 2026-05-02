program examples;

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils, Classes, basics;

begin
  WriteLn('*- Examples for Match/MatchAll -*');
  MatchDateRegex;

  WriteLn('');
  WriteLn('*- Examples for Match/MatchAll -*');
  MatchLazyDog;
{
  WriteLn('');
  WriteLn('*- Examples for Split -*');
  SplitStrings;
}
  WriteLn('');
  WriteLn('*- Examples for ExtractAll -*');
  ExtractAllStrings;

  WriteLn('');
  WriteLn('*- Examples for named capturing groups -*');
  NamedCapturingGroups;

  WriteLn('');
  WriteLn('*- Examples for Find -*');
  FindPosition;

  WriteLn('');
  WriteLn('*- Examples for Test -*');
  Test;

  WriteLn('');
  WriteLn('*- Examples for Replace -*');
  Replace;

  WriteLn('');
  WriteLn('*- Examples for converted regular expression -*');
  ConvertedExpressions;

  ReadLn;
end.
