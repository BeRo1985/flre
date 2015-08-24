program benchmark;
{$apptype console}

uses
  SysUtils,
  Classes,
  Windows,
  FLRE in '..\..\src\FLRE.pas',
  FLREUnicode in '..\..\src\FLREUnicode.pas';

// benchmark from http://lh3lh3.users.sourceforge.net/reb.shtml
{const BenchmarkPatterns2:array[0..4] of ansistring=('installation',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?',
                                                    '(?:[^ @]+)@(?:[^ @]+)',
                                                    '(?:[0-9][0-9]?)/(?:[0-9][0-9]?)/(?:[0-9][0-9](?:[0-9][0-9])?)',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?|(?:[^ @]+)@(?:[^ @]+)');
 {}
(*const BenchmarkPatterns:array[0..4] of ansistring=('Twain',
                                                   'Huck[a-zA-Z]+',
                                                   '[a-zA-Z]+ing',
                                                   'Tom|Sawyer|Huckleberry|Finn',
                                                   'Tom.{0,30}river|river.{0,30}Tom');
*)
const BenchmarkPatterns:array[0..4] of ansistring=('installation',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?',
                                                   '(?#Hallo)([^ @]+)@([^ @]+)',
                                                   '([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?|([^ @]+)@([^ @]+)');

var i,j,k,h:integer;
    s:ansistring;
    sl:TStringList;
    FLREInstance:TFLRE;
    t1,t2:longword;
    Captures:TFLREMultiCaptures;
{   RegExp:TRegexEngine;
    RegExprInstance:TRegExpr;{}
begin
 sl:=TStringList.Create;
 try
  sl.LoadFromFile('benchmark.txt');
  s:=sl.Text;
{}writeln('FLRE:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    FLREInstance:=TFLRE.Create(BenchmarkPatterns[i],[]);
    try
     write('/'+BenchmarkPatterns[i]+'/ : ':65);
     t1:=GetTickCount;
     FLREInstance.MatchAll(s,Captures);
     t2:=GetTickCount;
     writeln(t2-t1:5,' ms ',length(Captures));
    finally
     SetLength(Captures,0);
     FLREInstance.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');  {}
{ writeln('regex.pp:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    RegExp:=TRegexEngine.Create(BenchmarkPatterns[i]);
    try
     write('/'+BenchmarkPatterns[i]+'/ : ':65);
     t1:=GetTickCount;
     j:=0;
     k:=0;
     h:=0;
     while RegExp.MatchString(s,j,k) do begin
      inc(h);
     end;
     t2:=GetTickCount;
     writeln(t2-t1:5,' ms ',h);
    finally
     RegExp.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
{ writeln('RegExpr.pas:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    RegExprInstance:=TRegExpr.Create;
    RegExprInstance.Expression:=BenchmarkPatterns[i];
    RegExprInstance.Compile;
    try
     t1:=GetTickCount;
     RegExprInstance.Exec(s);
     while RegExprInstance.ExecNext do begin
     end;
     t2:=GetTickCount;
     writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
    finally
     RegExprInstance.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;{}
 finally
  sl.Free;
 end;
 s:='';
 readln;
end.


