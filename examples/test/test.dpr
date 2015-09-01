program test;
{$apptype console}

uses
  SysUtils,
  Classes,
  FLRE in '..\..\src\FLRE.pas',
  FLREUnicode in '..\..\src\FLREUnicode.pas';

var FLREInstance:TFLRE;
    s:ansistring;
    Captures:TFLREMultiCaptures;
    i,j:longint;
begin
 try
//FLREInstance:=TFLRE.Create('b{1,4}',[rfCASEINSENSITIVE,rfMULTILINE]);
//FLREInstance:=TFLRE.Create('\d\d\d?[\-\.\ \,\:]\d\d\d?[\-\.\ \,\:]\d\d\d\d?',[rfCASEINSENSITIVE,rfMULTILINE]);
//  FLREInstance:=TFLRE.Create('\d\d\d{0,1}[\-\.\ \,\:]\d\d\d{0,1}[\-\.\ \,\:]\d\d\d\d{0,1}',[rfMULTILINE]);
//FLREInstance:=TFLRE.Create('[a-z\dA-Z][a-z0-9-_.]?',[rfMULTILINE]);
  FLREInstance:=TFLRE.Create('(?<test>[a][bde])*(va\a?)?(hello)?c',[rfCASEINSENSITIVE,rfMULTILINE]);
  try
   s:='abc abbc advac boom';
   if FLREInstance.MatchAll(s,Captures) then begin
    for i:=0 to length(Captures)-1 do begin
     for j:=0 to length(Captures[i])-1 do begin
      write('"',copy(s,Captures[i,j].Start,Captures[i,j].Length),'" ');
      write('(',Captures[i,j].Start,',',Captures[i,j].Length,') ');
     end;
     writeln;
    end;
   end;
   writeln;
   s:=FLREInstance.ReplaceAll(s,'b"${test}"')+' k';
   writeln(s);
  finally
   FLREInstance.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 readln;
end.
