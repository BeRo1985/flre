unit UnitTestGlobals;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,Classes,FLRE;

procedure CheckTestResult(const ResultValue:boolean;const What:TFLRERawByteString); 

procedure ExecuteReplaceTest(const RegExpString,RewriteString,OriginalString,SingleString,GlobalString:TFLRERawByteString;const ReplaceCount:longint;const RegExpFlags:TFLREFlags=[rfDELIMITERS]);

implementation

const HexChars:array[boolean,0..15] of ansichar=(('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'),
                                                 ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'));

function EscapeString(const s:TFLRERawByteString):TFLRERawByteString;
var i:longint;
    c:byte;
begin
 result:='';
 i:=1;
 while i<=length(s) do begin
  case s[i] of
   '"','\':begin
    result:=result+'\'+s[i];
    inc(i);
   end;
   #$0008:begin
    result:=result+'\b';
    inc(i);
   end;
   #$0009:begin
    result:=result+'\t';
    inc(i);
   end;
   #$000a:begin
    result:=result+'\n';
    inc(i);
   end;
   #$000b:begin
    result:=result+'\v';
    inc(i);
   end;
   #$000c:begin
    result:=result+'\f';
    inc(i);
   end;
   #$000d:begin
    result:=result+'\r';
    inc(i);
   end;
   #$0000..#$0007,#$000e..#$001f,#$007d..#$009f,#$00ad:begin
    c:=byte(ansichar(s[i]));
    result:=result+'\x'+HexChars[false,(c shr 4) and $f]+HexChars[false,c and $f];
    inc(i);
   end;
   else begin
    result:=result+s[i];
    inc(i);
   end;
  end;
 end;
end;

procedure CheckTestResult(const ResultValue:boolean;const What:TFLRERawByteString);
begin
 if ResultValue then begin
  writeln('Successful: '+EscapeString(What));
 end else begin
  writeln('    Failed: '+EscapeString(What));
 end;
end;

procedure ExecuteReplaceTest(const RegExpString,RewriteString,OriginalString,SingleString,GlobalString:TFLRERawByteString;const ReplaceCount:longint;const RegExpFlags:TFLREFlags=[rfDELIMITERS]);
var FLREInstance:TFLRE;
    ResultString:TFLRERawByteString;
begin
 FLREInstance:=TFLRE.Create(RegExpString,RegExpFlags);
 try

   ResultString:=FLREInstance.Replace(OriginalString,RewriteString,1,1);
   CheckTestResult(ResultString=SingleString,'Replace('''+OriginalString+''','''+RegExpString+''','''+RewriteString+''',1)='''+SingleString+''' ['''+ResultString+''']');

   ResultString:=FLREInstance.Replace(OriginalString,RewriteString,1,ReplaceCount);
   CheckTestResult(ResultString=GlobalString,'Replace('''+OriginalString+''','''+RegExpString+''','''+RewriteString+''','+IntToStr(ReplaceCount)+')='''+GlobalString+''' ['''+ResultString+''']');
   
 finally
  FLREInstance.Free;
 end;
end;

end.
