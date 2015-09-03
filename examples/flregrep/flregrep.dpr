program flregrep;
{$ifdef fpc}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$define caninline}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpuamd64}
  {$define cpux86_64}
  {$define cpux64}
 {$else}
  {$ifdef cpux86_64}
   {$define cpuamd64}
   {$define cpux64}
  {$endif}
 {$endif}
 {$ifdef cpu386}
  {$define cpu386}
  {$asmmode intel}
  {$define canx86simd}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
{$else}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$safedivide off}
 {$optimization on}
 {$undef caninline}
 {$undef canx86simd}
 {$ifdef ver180}
  {$define caninline}
  {$ifdef cpu386}
   {$define canx86simd}
  {$endif}
  {$finitefloat off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$extendedsyntax on}
{$writeableconst on}
{$varstringchecks on}
{$typedaddress off}
{$overflowchecks off}
{$rangechecks off}
{$ifndef fpc}
{$realcompatibility off}
{$endif}
{$openstrings on}
{$longstrings on}
{$booleval off}
{$apptype console}

uses
{$ifdef windows}
  Windows,
  MMSystem,
{$endif}
  SysUtils,
  Classes,
  FLRE in '..\..\src\FLRE.pas',
  FLREUnicode in '..\..\src\FLREUnicode.pas',
  BeRoFileMappedStream in '..\common\BeRoFileMappedStream.pas',
  BeRoHighResolutionTimer in '..\common\BeRoHighResolutionTimer.pas';

const mNORMAL=0;
      mCOUNT=1;
      mBENCHMARK=2;

function PtrCopy(const Src:PAnsiChar;From,Len:longint):ansistring;
begin
 SetLength(result,Len);
 if Len>0 then begin
  Move(Src[From],result[1],Len);
 end;
end;

var FLREInstance:TFLRE;
    FileMappedStream:TBeRoFileMappedStream;
    MemoryViewSize,ToDo,SlidingOffset:int64;
    Memory:pointer;
    MultiCaptures:TFLREMultiCaptures;
    Count,Index,SubIndex,FirstNewLine,Mode,LastLineOffset,LastEndLineOffset,LineEndOffset,LineOffset:longint;
    Parameter,Argument,RegularExpression,FileName:ansistring;
    HasRegularExpression,HasFileName,SuppressErrorMessages:longbool;
    RegularExpressionFlags:TFLREFlags;
    SplitCharacter:ansichar;
    HighResolutionTimer:THighResolutionTimer;
    StartTime,EndTime:int64;
begin
 RegularExpression:='';
 FileName:='';
 HasRegularExpression:=false;
 HasFileName:=false;
 SuppressErrorMessages:=false;
 RegularExpressionFlags:=[];
 SplitCharacter:=#10;
 Mode:=mNORMAL;
 Index:=1;
 while Index<=ParamCount do begin
  Parameter:=ParamStr(Index);
  inc(Index);
  if (length(Parameter)>1) and (Parameter[1]='-') then begin
   SubIndex:=pos('=',Parameter);
   if SubIndex>0 then begin
    Argument:=copy(Parameter,SubIndex+1,length(Parameter)-SubIndex);
    Parameter:=copy(Parameter,1,SubIndex-1);
   end else begin
    Argument:='';
   end;
   if (Parameter='-e') or (Parameter='--regexp') then begin
    if Index<=ParamCount then begin
     RegularExpression:=ParamStr(Index);
     inc(Index);
     HasRegularExpression:=true;
    end else begin
     RegularExpression:=Argument;
     HasRegularExpression:=true;
    end;
   end else if (Parameter='-f') or (Parameter='--file') then begin
    if Index<=ParamCount then begin
     FileName:=ParamStr(Index);
     inc(Index);
     HasFileName:=true;
    end else begin
     FileName:=Argument;
     HasFileName:=true;
    end;
   end else if (Parameter='-i') or (Parameter='--ignore-case') then begin
    Include(RegularExpressionFlags,rfIGNORECASE);
   end else if (Parameter='-s') or (Parameter='--no-messages') then begin
    SuppressErrorMessages:=true;
   end else if (Parameter='-z') or (Parameter='--null-data') then begin
    SplitCharacter:=#0;
   end else if (Parameter='-c') or (Parameter='--count') then begin
    Mode:=mCOUNT;
   end else if (Parameter='-b') or (Parameter='--benchmark') then begin
    Mode:=mBENCHMARK;
   end;
  end else if length(Parameter)>0 then begin
   if not HasRegularExpression then begin
    RegularExpression:=Parameter;
    HasRegularExpression:=true;
   end else if not HasFileName then begin
    FileName:=Parameter;
    HasFileName:=true;
   end;
  end;
 end;
 if not HasFileName then begin
  FileName:='-';
  HasFileName:=true;
 end;
 if not HasRegularExpression then begin
  if not SuppressErrorMessages then begin
   writeln('flregrep: missing regular expression');
  end;
  halt(2);
 end;
 if not HasFileName then begin
  if not SuppressErrorMessages then begin
   writeln('flregrep: missing file name');
  end;
  halt(2);
 end;
 try
  FLREInstance:=TFLRE.Create(RegularExpression,RegularExpressionFlags);
  FLREInstance.MaximalDFAStates:=65536;
  try
   FileMappedStream:=TBeRoFileMappedStream.Create(FileName,fmOpenRead);
   try
    HighResolutionTimer:=THighResolutionTimer.Create;
    try
     Count:=0;
     StartTime:=HighResolutionTimer.GetTime;
     while FileMappedStream.Position<FileMappedStream.Size do begin
      SlidingOffset:=FileMappedStream.Position;
      ToDo:=FileMappedStream.Size-SlidingOffset;
      Memory:=FileMappedStream.Memory;
      MemoryViewSize:=FileMappedStream.MemoryViewSize;
      if ToDo>MemoryViewSize then begin
       ToDo:=MemoryViewSize;
      end;
      if ToDo=0 then begin
       break;
      end;
      LastLineOffset:=-1;
      LastEndLineOffset:=-1;
      FirstNewLine:=-1;
      for Index:=0 to ToDo-1 do begin
       if PAnsiChar(Memory)[Index]=SplitCharacter then begin
        // First new line detected
        FirstNewLine:=Index;
        break;
       end;
      end;
      if FirstNewLine>=0 then begin
       for Index:=ToDo-1 downto FirstNewLine do begin
        if PAnsiChar(Memory)[Index]=SplitCharacter then begin
         // New line detected, trimming todo-count to it
         if ToDo>Index then begin
          ToDo:=Index;
         end;
         break;
        end;
       end;
      end;
      if FLREInstance.PtrMatchAll(Memory,ToDo,MultiCaptures) then begin

       case Mode of
        mNORMAL:begin
         for Index:=0 to length(MultiCaptures)-1 do begin
          if LastEndLineOffset<MultiCaptures[Index,0].Start then begin
           LineOffset:=0;
           for SubIndex:=MultiCaptures[Index,0].Start downto 0 do begin
            if PAnsiChar(Memory)[SubIndex]=SplitCharacter then begin
             LineOffset:=SubIndex+1;
             break;
            end;
           end;
           if LastLineOffset<>LineOffset then begin
            LastLineOffset:=LineOffset;
            LineEndOffset:=ToDo-1;
            for SubIndex:=LineOffset to ToDo-1 do begin
             if PAnsiChar(Memory)[SubIndex]=SplitCharacter then begin
              LineEndOffset:=SubIndex-1;
              LastEndLineOffset:=LineEndOffset;
              break;
             end;
            end;
            writeln(PtrCopy(PAnsiChar(Memory),LineOffset,(LineEndOffset-LineOffset)+1));
           end;
          end;
         end;
        end;
       end;

       // Adjust start offsets
       for Index:=0 to length(MultiCaptures)-1 do begin
        for SubIndex:=0 to length(MultiCaptures[Index])-1 do begin
         inc(MultiCaptures[Index,SubIndex].Start,SlidingOffset);
        end;
       end;

       inc(Count,length(MultiCaptures));

      end;
      FileMappedStream.Seek(ToDo,soCurrent);
     end;
     EndTime:=HighResolutionTimer.GetTime;
     case Mode of
      mCOUNT:begin
       writeln(Count);
      end;
      mBENCHMARK:begin
       writeln(Count,' founds in ',HighResolutionTimer.ToMicroseconds(EndTime-StartTime)/1000.0:1:4,' milliseconds');
      end;
     end;
    finally
     HighResolutionTimer.Free;
    end;
   finally
    FileMappedStream.Free;
   end;
  finally
   FLREInstance.Free;
  end;
 except
  on e:Exception do begin
   if not SuppressErrorMessages then begin
    writeln('flregrep[',e.ClassName,']: ',e.Message);
   end;
   halt(2);
  end;
 end;
{$ifndef fpc}
 if DebugHook<>0 then begin
  readln;
 end;
{$endif}
 halt(0);
end.
