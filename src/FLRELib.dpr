library FLRELib;
{$ifdef fpc}
 {$mode delphi}
{$endif}

{$ifndef fpc}
{ FastMM4 in 'FastMM4.pas',
  FastMove in 'FastMove.pas',
  FastcodeCPUID in 'FastcodeCPUID.pas',
  FastMM4Messages in 'FastMM4Messages.pas',}
{$endif}

uses
  SysUtils,
  Classes,
  FLRE in 'FLRE.pas',
  FLREUnicode in 'FLREUnicode.pas';

exports FLREGetVersion name 'FLREGetVersion',
        FLREGetVersionString name 'FLREGetVersionString',
        FLRECreate name 'FLRECreate',
        FLREDestroy name 'FLREDestroy',
        FLREFree name 'FLREFree',
        FLREGetCountCaptures name 'FLREGetCountCaptures',
        FLREGetNamedGroupIndex name 'FLREGetNamedGroupIndex',
        FLREGetPrefilterExpression name 'FLREGetPrefilterExpression',
        FLREGetPrefilterShortExpression name 'FLREGetPrefilterShortExpression',
        FLREGetPrefilterSQLBooleanFullTextExpression name 'FLREGetPrefilterSQLBooleanFullTextExpression',
        FLREGetPrefilterSQLExpression name 'FLREGetPrefilterSQLExpression',
        FLREGetRange name 'FLREGetRange',
        FLREMatch name 'FLREMatch',
        FLREMatchNext name 'FLREMatchNext',
        FLREMatchAll name 'FLREMatchAll',
        FLREReplaceAll name 'FLREReplaceAll';

begin
 InitializeFLRE;
end.
