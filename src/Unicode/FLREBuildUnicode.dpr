(**********************************************************
** FLRE Regular Expression Library                        *
***********************************************************
**
** This file is part of the FLRE Regular Expression Library.
** Copyright (C) 2011-2016 by Benjamin Rosseaux
**
** See the file COPYING.FLRE, included in this distribution,
** for details about the copyright.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*)
program FLREBuildUnicode;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

uses
  SysUtils,
  Classes,
  FLREUnicode in 'FLREUnicode.pas';

      // For hash map
const CELL_EMPTY=-1;
      CELL_DELETED=-2;

      // For hash map
      ENT_EMPTY=-1;
      ENT_DELETED=-2;

      ucrWORDS=0;
      ucrDIGITS=1;
      ucrWHITESPACES=2;
      ucrLAST=ucrWHITESPACES;

type PFLRERawByteChar=^TFLRERawByteChar;
     TFLRERawByteChar=ansichar;

     PUnicodeCharRange=^TUnicodeCharRange;
     TUnicodeCharRange=array[0..1] of longword;

     PUnicodeCharRanges=^TUnicodeCharRanges;
     TUnicodeCharRanges=array of TUnicodeCharRange;

     PUnicodeCharRangeClasses=^TUnicodeCharRangeClasses;
     TUnicodeCharRangeClasses=array[0..ucrLAST] of TUnicodeCharRanges;

     PUnicodeCharClassRange=^TUnicodeCharClassRange;
     TUnicodeCharClassRange=record
      FromChar:longword;
      ToChar:longword;
     end;

     TUnicodeCharClassBlock=array of TUnicodeCharClassRange;

     TUnicodeCharClassBlocks=array of TUnicodeCharClassBlock;

     TFLREStringIntegerPairHashMapData=int64;

     PFLREStringIntegerPairHashMapEntity=^TFLREStringIntegerPairHashMapEntity;
     TFLREStringIntegerPairHashMapEntity=record
      Key:TFLRERawByteString;
      Value:TFLREStringIntegerPairHashMapData;
     end;

     TFLREStringIntegerPairHashMapEntities=array of TFLREStringIntegerPairHashMapEntity;

     TFLREStringIntegerPairHashMapEntityIndices=array of longint;

     TFLREStringIntegerPairHashMap=class
      private
       function FindCell(const Key:TFLRERawByteString):longword;
       procedure Resize;
      protected
       function GetValue(const Key:TFLRERawByteString):TFLREStringIntegerPairHashMapData;
       procedure SetValue(const Key:TFLRERawByteString;const Value:TFLREStringIntegerPairHashMapData);
      public
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TFLREStringIntegerPairHashMapEntities;
       EntityToCellIndex:TFLREStringIntegerPairHashMapEntityIndices;
       CellToEntityIndex:TFLREStringIntegerPairHashMapEntityIndices;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure LowerCaseAssignFrom(const HashMap:TFLREStringIntegerPairHashMap);
       function Add(const Key:TFLRERawByteString;Value:TFLREStringIntegerPairHashMapData):PFLREStringIntegerPairHashMapEntity;
       function Get(const Key:TFLRERawByteString;CreateIfNotExist:boolean=false):PFLREStringIntegerPairHashMapEntity;
       function Delete(const Key:TFLRERawByteString):boolean;
       property Values[const Key:TFLRERawByteString]:TFLREStringIntegerPairHashMapData read GetValue write SetValue; default;
     end;

function HashString(const Str:TFLRERawByteString):longword;
{$ifdef cpuarm}
var b:PFLRERawByteChar;
    len,h,i:longword;
begin
 result:=2166136261;
 len:=length(Str);
 h:=len;
 if len>0 then begin
  b:=PFLRERawByteChar(Str);
  while len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(len,4);
  end;
  if len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(len,2);
  end;
  if len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$else}
const m=longword($57559429);
      n=longword($5052acdb);
var b:PFLRERawByteChar;
    h,k,len:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 len:=length(Str);
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=PFLRERawByteChar(Str);
  while len>7 do begin
   begin
    p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*{$ifdef fpc}qword{$else}int64{$endif}(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*{$ifdef fpc}qword{$else}int64{$endif}(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

function UTF8Correct(const s:ansistring):ansistring;
begin
 result:=s;
end;

constructor TFLREStringIntegerPairHashMap.Create;
begin
 inherited Create;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 Entities:=nil;
 EntityToCellIndex:=nil;
 CellToEntityIndex:=nil;
 Resize;
end;

destructor TFLREStringIntegerPairHashMap.Destroy;
var Counter:longint;
begin
 Clear;
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 inherited Destroy;
end;

procedure TFLREStringIntegerPairHashMap.Clear;
var Counter:longint;
begin
 for Counter:=0 to length(Entities)-1 do begin
  Entities[Counter].Key:='';
 end;
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

procedure TFLREStringIntegerPairHashMap.LowerCaseAssignFrom(const HashMap:TFLREStringIntegerPairHashMap);
var Counter:longint;
begin
 Clear;
 for Counter:=0 to length(HashMap.EntityToCellIndex)-1 do begin
  if HashMap.EntityToCellIndex[Counter]>=0 then begin
   Add(LowerCase(HashMap.Entities[Counter].Key),HashMap.Entities[Counter].Value);
  end;
 end;
end;

function TFLREStringIntegerPairHashMap.FindCell(const Key:TFLRERawByteString):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 HashCode:=HashString(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and (Entities[Entity].Key=Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

procedure TFLREStringIntegerPairHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TFLREStringIntegerPairHashMapEntities;
    OldCellToEntityIndex:TFLREStringIntegerPairHashMapEntityIndices;
    OldEntityToCellIndex:TFLREStringIntegerPairHashMapEntityIndices;
begin
 NewLogSize:=0;
 NewSize:=RealSize;
 while NewSize<>0 do begin
  NewSize:=NewSize shr 1;
  inc(NewLogSize);
 end;
 if NewLogSize<1 then begin
  NewLogSize:=1;
 end;
 Size:=0;
 RealSize:=0;
 LogSize:=NewLogSize;
 OldEntities:=Entities;
 OldCellToEntityIndex:=CellToEntityIndex;
 OldEntityToCellIndex:=EntityToCellIndex;
 Entities:=nil;
 CellToEntityIndex:=nil;
 EntityToCellIndex:=nil;
 SetLength(Entities,2 shl LogSize);
 SetLength(CellToEntityIndex,2 shl LogSize);
 SetLength(EntityToCellIndex,2 shl LogSize);
 for Counter:=0 to length(CellToEntityIndex)-1 do begin
  CellToEntityIndex[Counter]:=ENT_EMPTY;
 end;
 for Counter:=0 to length(EntityToCellIndex)-1 do begin
  EntityToCellIndex[Counter]:=CELL_EMPTY;
 end;
 for Counter:=0 to length(OldEntityToCellIndex)-1 do begin
  Cell:=OldEntityToCellIndex[Counter];
  if Cell>=0 then begin
   Entity:=OldCellToEntityIndex[Cell];
   if Entity>=0 then begin
    Add(OldEntities[Counter].Key,OldEntities[Counter].Value);
   end;
  end;
 end;
 for Counter:=0 to length(OldEntities)-1 do begin
  OldEntities[Counter].Key:='';
 end;
 SetLength(OldEntities,0);
 SetLength(OldCellToEntityIndex,0);
 SetLength(OldEntityToCellIndex,0);
end;

function TFLREStringIntegerPairHashMap.Add(const Key:TFLRERawByteString;Value:TFLREStringIntegerPairHashMapData):PFLREStringIntegerPairHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 while RealSize>=(1 shl LogSize) do begin
  Resize;
 end;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
  exit;
 end;
 Entity:=Size;
 inc(Size);
 if Entity<(2 shl LogSize) then begin
  CellToEntityIndex[Cell]:=Entity;
  EntityToCellIndex[Entity]:=Cell;
  inc(RealSize);
  result:=@Entities[Entity];
  result^.Key:=Key;
  result^.Value:=Value;
 end;
end;

function TFLREStringIntegerPairHashMap.Get(const Key:TFLRERawByteString;CreateIfNotExist:boolean=false):PFLREStringIntegerPairHashMapEntity;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=@Entities[Entity];
 end else if CreateIfNotExist then begin
  result:=Add(Key,-1);
 end;
end;

function TFLREStringIntegerPairHashMap.Delete(const Key:TFLRERawByteString):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:='';
  Entities[Entity].Value:=-1;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  result:=true;
 end;
end;

function TFLREStringIntegerPairHashMap.GetValue(const Key:TFLRERawByteString):TFLREStringIntegerPairHashMapData;
var Entity:longint;
    Cell:longword;
begin
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Value;
 end else begin
  result:=-1;
 end;
end;

procedure TFLREStringIntegerPairHashMap.SetValue(const Key:TFLRERawByteString;const Value:TFLREStringIntegerPairHashMapData);
begin
 Add(Key,Value);
end;

function UnicodeGetCategoryFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr FLREUnicodeCategoryArrayBlockBits;
  result:=FLREUnicodeCategoryArrayBlockData[FLREUnicodeCategoryArrayIndexBlockData[FLREUnicodeCategoryArrayIndexIndexData[Index shr FLREUnicodeCategoryArrayIndexBlockBits],Index and FLREUnicodeCategoryArrayIndexBlockMask],c and FLREUnicodeCategoryArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetScriptFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr FLREUnicodeScriptArrayBlockBits;
  result:=FLREUnicodeScriptArrayBlockData[FLREUnicodeScriptArrayIndexBlockData[FLREUnicodeScriptArrayIndexIndexData[Index shr FLREUnicodeScriptArrayIndexBlockBits],Index and FLREUnicodeScriptArrayIndexBlockMask],c and FLREUnicodeScriptArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr FLREUnicodeUpperCaseDeltaArrayBlockBits;
  result:=FLREUnicodeUpperCaseDeltaArrayBlockData[FLREUnicodeUpperCaseDeltaArrayIndexBlockData[FLREUnicodeUpperCaseDeltaArrayIndexIndexData[Index shr FLREUnicodeUpperCaseDeltaArrayIndexBlockBits],Index and FLREUnicodeUpperCaseDeltaArrayIndexBlockMask],c and FLREUnicodeUpperCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr FLREUnicodeLowerCaseDeltaArrayBlockBits;
  result:=FLREUnicodeLowerCaseDeltaArrayBlockData[FLREUnicodeLowerCaseDeltaArrayIndexBlockData[FLREUnicodeLowerCaseDeltaArrayIndexIndexData[Index shr FLREUnicodeLowerCaseDeltaArrayIndexBlockBits],Index and FLREUnicodeLowerCaseDeltaArrayIndexBlockMask],c and FLREUnicodeLowerCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr FLREUnicodeTitleCaseDeltaArrayBlockBits;
  result:=FLREUnicodeTitleCaseDeltaArrayBlockData[FLREUnicodeTitleCaseDeltaArrayIndexBlockData[FLREUnicodeTitleCaseDeltaArrayIndexIndexData[Index shr FLREUnicodeTitleCaseDeltaArrayIndexBlockBits],Index and FLREUnicodeTitleCaseDeltaArrayIndexBlockMask],c and FLREUnicodeTitleCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeIsWord(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [FLREUnicodeCategoryLu,FLREUnicodeCategoryLl,FLREUnicodeCategoryLt,FLREUnicodeCategoryLm,FLREUnicodeCategoryLo,FLREUnicodeCategoryNd,FLREUnicodeCategoryNl,FLREUnicodeCategoryNo,FLREUnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsIDBegin(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [FLREUnicodeCategoryLu,FLREUnicodeCategoryLl,FLREUnicodeCategoryLt,FLREUnicodeCategoryLm,FLREUnicodeCategoryLo,FLREUnicodeCategoryNl,FLREUnicodeCategoryNo,FLREUnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsIDPart(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [FLREUnicodeCategoryLu,FLREUnicodeCategoryLl,FLREUnicodeCategoryLt,FLREUnicodeCategoryLm,FLREUnicodeCategoryLo,FLREUnicodeCategoryNd,FLREUnicodeCategoryNl,FLREUnicodeCategoryNo,FLREUnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsWhiteSpace(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
//result:=UnicodeGetCategoryFromTable(c) in [FLREUnicodeCategoryZs,FLREUnicodeCategoryZp,FLREUnicodeCategoryZl];
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function UnicodeToUpper(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetUpperCaseDeltaFromTable(c)));
end;

function UnicodeToLower(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetLowerCaseDeltaFromTable(c)));
end;

function UnicodeToTitle(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetTitleCaseDeltaFromTable(c)));
end;

type TFLREUnicodeCharClass=class;

     TFLREUnicodeCharClassRange=class
      public
       CharClass:TFLREUnicodeCharClass;
       Previous,Next,Left,Right:TFLREUnicodeCharClassRange;
       Lo,Hi:longword;
       constructor Create(ACharClass:TFLREUnicodeCharClass;ALo,AHi:longword);
       constructor CreateBefore(ACharClass:TFLREUnicodeCharClass;ABefore:TFLREUnicodeCharClassRange;ALo,AHi:longword);
       constructor CreateAfter(ACharClass:TFLREUnicodeCharClass;AAfter:TFLREUnicodeCharClassRange;ALo,AHi:longword);
       destructor Destroy; override;
     end;

     TFLREUnicodeCharClassCharSet=set of ansichar;

     TFLREUnicodeCharClassRanges=array of TFLREUnicodeCharClassRange;

     TFLREUnicodeCharClass=class
      public
       First,Last,Root:TFLREUnicodeCharClassRange;
       CharSet:TFLREUnicodeCharClassCharSet;
       Inverted:longbool;
       Canonicalized:longbool;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure Dump;
       procedure DebugDump;
       procedure Optimize;
       procedure AddRange(Lo,Hi:longword;IgnoreCase:boolean=false);
       procedure AddChar(c:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeCategory(CategoryFlags:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeScript(Script:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeBlock(Block:longword;IgnoreCase:boolean=false);
       procedure AddUnicodeAdditionalBlock(Block:longword;IgnoreCase:boolean=false);
       procedure Combine(From:TFLREUnicodeCharClass);
       function Subtraction(From,SubtractWith:TFLREUnicodeCharClass):boolean;
       function Intersection(From,IntersectWith:TFLREUnicodeCharClass):boolean;
       procedure TakeoverCombine(From:TFLREUnicodeCharClass);
       procedure TakeoverDirtyCombine(From:TFLREUnicodeCharClass);
       procedure Assign(From:TFLREUnicodeCharClass);
       procedure Append(From:TFLREUnicodeCharClass);
       procedure Invert;
       procedure Canonicalize;
       procedure CompileBinarySearchTree;
       procedure CompileCharset;
       procedure Finalize;
       function Count:longword;
       function Contains(c:longword):boolean;
       function Intersects(Other:TFLREUnicodeCharClass):boolean;
       function IsSingle:boolean;
       function HashCode:longword;
       function EqualsTo(OtherObject:TFLREUnicodeCharClass):boolean;
     end;

var LowerUpperCaseUnicodeCharClass:TFLREUnicodeCharClass;

    UnitSourceList:TStringList;
    LineIndex:longint;
    Line:ansistring;

    UnicodeCharRangeClasses:TUnicodeCharRangeClasses;

    UnicodeCategoryBlocks:TUnicodeCharClassBlocks;
    UnicodeIgnoreCaseCategoryBlocks:TUnicodeCharClassBlocks;
    CountUnicodeCategoryBlocks:longint;

    UnicodeScriptBlocks:TUnicodeCharClassBlocks;
    UnicodeIgnoreCaseScriptBlocks:TUnicodeCharClassBlocks;
    CountUnicodeScriptBlocks:longint;

    UnicodeBlockBlocks:TUnicodeCharClassBlocks;
    UnicodeIgnoreCaseBlockBlocks:TUnicodeCharClassBlocks;
    CountUnicodeBlockBlocks:longint;

    UnicodeAdditionalBlocks:TUnicodeCharClassBlocks;
    UnicodeIgnoreCaseAdditionalBlocks:TUnicodeCharClassBlocks;
    CountUnicodeAdditionalBlocks:longint;

    UnicodeClassHashMap:TFLREStringIntegerPairHashMap;
    UnicodeScriptHashMap:TFLREStringIntegerPairHashMap;
    UnicodeBlockHashMap:TFLREStringIntegerPairHashMap;
    UnicodeAdditionalBlockHashMap:TFLREStringIntegerPairHashMap;
    UnicodeClassLowerCaseHashMap:TFLREStringIntegerPairHashMap;
    UnicodeScriptLowerCaseHashMap:TFLREStringIntegerPairHashMap;
    UnicodeBlockLowerCaseHashMap:TFLREStringIntegerPairHashMap;
    UnicodeAdditionalBlockLowerCaseHashMap:TFLREStringIntegerPairHashMap;

constructor TFLREUnicodeCharClassRange.Create(ACharClass:TFLREUnicodeCharClass;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 if assigned(CharClass.Last) then begin
  Previous:=CharClass.Last;
  CharClass.Last:=self;
  Previous.Next:=self;
  Next:=nil;
 end else begin
  CharClass.First:=self;
  CharClass.Last:=self;
  Previous:=nil;
  Next:=nil;
 end;
 Left:=nil;
 Right:=nil;
end;

constructor TFLREUnicodeCharClassRange.CreateBefore(ACharClass:TFLREUnicodeCharClass;ABefore:TFLREUnicodeCharClassRange;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 Previous:=ABefore.Previous;
 Next:=ABefore;
 ABefore.Previous:=self;
 if assigned(Previous) then begin
  Previous.Next:=self;
 end else begin
  CharClass.First:=self;
 end;
end;

constructor TFLREUnicodeCharClassRange.CreateAfter(ACharClass:TFLREUnicodeCharClass;AAfter:TFLREUnicodeCharClassRange;ALo,AHi:longword);
begin
 inherited Create;
 CharClass:=ACharClass;
 Lo:=ALo;
 Hi:=AHi;
 Previous:=AAfter;
 Next:=AAfter.Next;
 AAfter.Next:=self;
 if assigned(Next) then begin
  Next.Previous:=self;
 end else begin
  CharClass.Last:=self;
 end;
end;

destructor TFLREUnicodeCharClassRange.Destroy;
begin
 if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if CharClass.First=self then begin
  CharClass.First:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if CharClass.Last=self then begin
  CharClass.Last:=Previous;
 end;
 Previous:=nil;
 Next:=nil;
 inherited Destroy;
end;

constructor TFLREUnicodeCharClass.Create;
begin
 inherited Create;
 First:=nil;
 Last:=nil;
 Root:=nil;
 Inverted:=false;
 Canonicalized:=false;
 CharSet:=[];
end;

destructor TFLREUnicodeCharClass.Destroy;
begin
 while assigned(First) do begin
  First.Free;
 end;
 inherited Destroy;
end;

procedure TFLREUnicodeCharClass.Clear;
begin
 while assigned(First) do begin
  First.Free;
 end;
 Inverted:=false;
 Canonicalized:=false;
 Root:=nil;
end;

procedure TFLREUnicodeCharClass.Dump;
var Range:TFLREUnicodeCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  writeln(Range.Lo:8,' ',Range.Hi:8);
  Range:=Range.Next;
 end;
end;

procedure TFLREUnicodeCharClass.DebugDump;
var Range:TFLREUnicodeCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  Range:=Range.Next;
 end;
end;

procedure TFLREUnicodeCharClass.Optimize;
var Range:TFLREUnicodeCharClassRange;
begin
 Range:=First;
 while assigned(Range) do begin
  if assigned(Range.Previous) and (((Range.Previous.Hi>=Range.Lo) or ((Range.Previous.Hi+1)=Range.Lo))) then begin
   if Range.Lo>Range.Previous.Lo then begin
    Range.Lo:=Range.Previous.Lo;
   end;
   if Range.Hi<Range.Previous.Hi then begin
    Range.Hi:=Range.Previous.Hi;
   end;
   Range.Previous.Free;
   if assigned(Range.Previous) then begin
    Range:=Range.Previous;
   end;
  end else if assigned(Range.Next) and (((Range.Hi>=Range.Next.Lo) or ((Range.Hi+1)=Range.Next.Lo))) then begin
   if Range.Lo>Range.Next.Lo then begin
    Range.Lo:=Range.Next.Lo;
   end;
   if Range.Hi<Range.Next.Hi then begin
    Range.Hi:=Range.Next.Hi;
   end;
   Range.Next.Free;
   if assigned(Range.Previous) then begin
    Range:=Range.Previous;
   end;
  end else begin
   Range:=Range.Next;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.AddRange(Lo,Hi:longword;IgnoreCase:boolean=false);
var Range,TempRange:TFLREUnicodeCharClassRange;
    c,cl,cu:longword;
    Temp,OtherTemp:TFLREUnicodeCharClass;
begin
 if IgnoreCase then begin
  AddRange(Lo,Hi,false);
  Temp:=TFLREUnicodeCharClass.Create;
  try
   OtherTemp:=TFLREUnicodeCharClass.Create;
   try
    OtherTemp.AddRange(Lo,Hi,false);
    Temp.Intersection(OtherTemp,LowerUpperCaseUnicodeCharClass);
   finally
    OtherTemp.Free;
   end;
   TempRange:=Temp.First;
   while assigned(TempRange) do begin
    for c:=TempRange.Lo to TempRange.Hi do begin
     cl:=UnicodeToLower(c);
     cu:=UnicodeToUpper(c);
     if cl<>c then begin
      AddRange(cl,cl,false);
     end;
     if cu<>c then begin
      AddRange(cu,cu,false);
     end;
    end;
    TempRange:=TempRange.Next;
   end;
  finally
   Temp.Free;
  end;
 end else begin
  Range:=First;
  while assigned(Range) do begin
   if (Lo>=Range.Lo) and (Hi<=Range.Hi) then begin
    exit;
   end else if (Lo<=Range.Lo) or ((Lo=Range.Lo) and (Hi<=Range.Hi)) then begin
    break;
   end;
   Range:=Range.Next;
  end;
  if assigned(Range) then begin
   TFLREUnicodeCharClassRange.CreateBefore(self,Range,Lo,Hi);
  end else begin
   TFLREUnicodeCharClassRange.Create(self,Lo,Hi);
  end;
  Optimize;
 end;
end;

procedure TFLREUnicodeCharClass.AddChar(c:longword;IgnoreCase:boolean=false);
begin
 AddRange(c,c,IgnoreCase);
end;

procedure TFLREUnicodeCharClass.AddUnicodeCategory(CategoryFlags:longword;IgnoreCase:boolean=false);
var Value,LowValue,HighValue,Index:longword;
begin
 LowValue:=$ffffffff;
 HighValue:=0;
 for Value:=0 to $10ffff do begin
  Index:=Value shr FLREUnicodeCategoryArrayBlockBits;
  if (CategoryFlags and (1 shl FLREUnicodeCategoryArrayBlockData[FLREUnicodeCategoryArrayIndexBlockData[FLREUnicodeCategoryArrayIndexIndexData[Index shr FLREUnicodeCategoryArrayIndexBlockBits],Index and FLREUnicodeCategoryArrayIndexBlockMask],Value and FLREUnicodeCategoryArrayBlockMask]))<>0 then begin
   if LowValue<=HighValue then begin
    if (HighValue+1)=Value then begin
     HighValue:=Value;
    end else begin
     AddRange(LowValue,HighValue,IgnoreCase);
     LowValue:=Value;
     HighValue:=Value;
    end;
   end else begin
    LowValue:=Value;
    HighValue:=Value;
   end;
  end;
 end;
 if LowValue<=HighValue then begin
  AddRange(LowValue,HighValue,IgnoreCase);
 end;
end;

procedure TFLREUnicodeCharClass.AddUnicodeScript(Script:longword;IgnoreCase:boolean=false);
var Value,LowValue,HighValue,Index:longword;
begin
 LowValue:=$ffffffff;
 HighValue:=0;
 for Value:=0 to $10ffff do begin
  Index:=Value shr FLREUnicodeScriptArrayBlockBits;
  if Script=FLREUnicodeScriptArrayBlockData[FLREUnicodeScriptArrayIndexBlockData[FLREUnicodeScriptArrayIndexIndexData[Index shr FLREUnicodeScriptArrayIndexBlockBits],Index and FLREUnicodeScriptArrayIndexBlockMask],Value and FLREUnicodeScriptArrayBlockMask] then begin
   if LowValue<=HighValue then begin
    if (HighValue+1)=Value then begin
     HighValue:=Value;
    end else begin
     AddRange(LowValue,HighValue,IgnoreCase);
     LowValue:=Value;
     HighValue:=Value;
    end;
   end else begin
    LowValue:=Value;
    HighValue:=Value;
   end;
  end;
 end;
 if LowValue<=HighValue then begin
  AddRange(LowValue,HighValue,IgnoreCase);
 end;
end;

procedure TFLREUnicodeCharClass.AddUnicodeBlock(Block:longword;IgnoreCase:boolean=false);
begin
 AddRange(FLREUnicodeBlocks[Block].FromChar,FLREUnicodeBlocks[Block].ToChar,IgnoreCase);
end;

procedure TFLREUnicodeCharClass.AddUnicodeAdditionalBlock(Block:longword;IgnoreCase:boolean=false);
var Range:longint;
begin
 for Range:=0 to length(UnicodeAdditionalBlocks[Block])-1 do begin
  AddRange(UnicodeAdditionalBlocks[Block,Range].FromChar,UnicodeAdditionalBlocks[Block,Range].ToChar,IgnoreCase);
 end;
end;

procedure TFLREUnicodeCharClass.Combine(From:TFLREUnicodeCharClass);
var Range:TFLREUnicodeCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  Range:=From.First;
  while assigned(Range) do begin
   AddRange(Range.Lo,Range.Hi,false);
   Range:=Range.Next;
  end;
 end;
end;
                                 
function TFLREUnicodeCharClass.Subtraction(From,SubtractWith:TFLREUnicodeCharClass):boolean;
var Range1,Range2:TFLREUnicodeCharClassRange;
    First,Min,Max:longword;
begin
 result:=false;
 if ((assigned(From) and assigned(SubtractWith)) and (assigned(From.First) and assigned(SubtractWith.First))) and ((From.First.Lo<=SubtractWith.Last.Hi) and (SubtractWith.First.Lo<=From.Last.Hi)) then begin
  Canonicalized:=From.Canonicalized;
  Range1:=From.First;
  while assigned(Range1) do begin
   First:=Range1.Lo;
   Range2:=SubtractWith.First;
   while assigned(Range2) do begin
    if (First<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     if First>Range2.Lo then begin
      Min:=First;
     end else begin
      Min:=Range2.Lo;
     end;
     if Range1.Hi<Range2.Hi then begin
      Max:=Range1.Hi;
     end else begin
      Max:=Range2.Hi;
     end;
     if First<Min then begin
      AddRange(First,Min-1,false);
     end;
     First:=Max+1;
    end;
    Range2:=Range2.Next;
   end;
   if First<=Range1.Hi then begin
    AddRange(First,Range1.Hi,false);
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

function TFLREUnicodeCharClass.Intersection(From,IntersectWith:TFLREUnicodeCharClass):boolean;
var Range1,Range2:TFLREUnicodeCharClassRange;
    Min,Max:longword;
begin
 result:=false;
 if ((assigned(From) and assigned(IntersectWith)) and (assigned(From.First) and assigned(IntersectWith.First))) and ((From.First.Lo<=IntersectWith.Last.Hi) and (IntersectWith.First.Lo<=From.Last.Hi)) then begin
  Canonicalized:=From.Canonicalized;
  Range1:=From.First;
  while assigned(Range1) do begin
   Range2:=IntersectWith.First;
   while assigned(Range2) do begin
    if (Range1.Lo<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     if Range1.Lo>Range2.Lo then begin
      Min:=Range1.Lo;
     end else begin
      Min:=Range2.Lo;
     end;
     if Range1.Hi<Range2.Hi then begin
      Max:=Range1.Hi;
     end else begin
      Max:=Range2.Hi;
     end;
     if Min<=Max then begin
      AddRange(Min,Max,false);
     end;
    end;
    Range2:=Range2.Next;
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.TakeoverCombine(From:TFLREUnicodeCharClass);
var SrcRange,NextSrcRange,Range:TFLREUnicodeCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  SrcRange:=From.First;
  while assigned(SrcRange) do begin
   Range:=First;
   while assigned(Range) do begin
    if (SrcRange.Lo>=Range.Lo) and (SrcRange.Hi<=Range.Hi) then begin
     exit;
    end else if (SrcRange.Lo<=Range.Lo) or ((SrcRange.Lo=Range.Lo) and (SrcRange.Hi<=Range.Hi)) then begin
     break;
    end;
    Range:=Range.Next;
   end;
   SrcRange.CharClass:=self;
   NextSrcRange:=SrcRange.Next;
   if assigned(SrcRange.Previous) then begin
    SrcRange.Previous.Next:=SrcRange.Next;
   end else if From.First=SrcRange then begin
    From.First:=SrcRange.Next;
   end;
   if assigned(SrcRange.Next) then begin
    SrcRange.Next.Previous:=SrcRange.Previous;
   end else if From.Last=SrcRange then begin
    From.Last:=SrcRange.Previous;
   end;
   SrcRange.Previous:=nil;
   SrcRange.Next:=nil;
   if assigned(Range) then begin
    SrcRange.Previous:=Range.Previous;
    SrcRange.Next:=Range;
    Range.Previous:=SrcRange;
    if assigned(SrcRange.Previous) then begin
     SrcRange.Previous.Next:=SrcRange;
    end else begin
     SrcRange.CharClass.First:=SrcRange;
    end;
   end else begin
    if assigned(SrcRange.CharClass.Last) then begin
     SrcRange.Previous:=SrcRange.CharClass.Last;
     SrcRange.CharClass.Last:=SrcRange;
     SrcRange.Previous.Next:=SrcRange;
     SrcRange.Next:=nil;
    end else begin
     SrcRange.CharClass.First:=SrcRange;
     SrcRange.CharClass.Last:=SrcRange;
     SrcRange.Previous:=nil;
     SrcRange.Next:=nil;
    end;
   end;
   Optimize;
   SrcRange:=NextSrcRange;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.TakeoverDirtyCombine(From:TFLREUnicodeCharClass);
var SrcRange:TFLREUnicodeCharClassRange;
begin
 if assigned(From) then begin
  if assigned(First) then begin
   Canonicalized:=Canonicalized and From.Canonicalized;
  end else begin
   Canonicalized:=From.Canonicalized;
  end;
  SrcRange:=From.First;
  while assigned(SrcRange) do begin
   SrcRange.CharClass:=self;
   SrcRange:=SrcRange.Next;
  end;
  if assigned(Last) then begin
   Last.Next:=From.First;
   From.First.Previous:=Last;
   Last:=From.Last;
  end else begin
   First:=From.First;
   Last:=From.Last;
  end;
  From.First:=nil;
  From.Last:=nil;
 end;
end;

procedure TFLREUnicodeCharClass.Assign(From:TFLREUnicodeCharClass);
var Range:TFLREUnicodeCharClassRange;
begin
 if assigned(From) then begin
  while assigned(First) do begin
   First.Free;
  end;
  Inverted:=From.Inverted;
  Canonicalized:=From.Canonicalized;
  Range:=From.First;
  while assigned(Range) do begin
   Range.CharClass:=self;
   Range:=Range.Next;
  end;
  First:=From.First;
  Last:=From.Last;
  From.First:=nil;
  From.Last:=nil;
 end;
end;

procedure TFLREUnicodeCharClass.Append(From:TFLREUnicodeCharClass);
var Range:TFLREUnicodeCharClassRange;
begin
 if assigned(From) then begin
  Range:=From.First;
  while assigned(Range) do begin
   TFLREUnicodeCharClassRange.Create(self,Range.Lo,Range.Hi);
   Range:=Range.Next;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.Invert;
var NewList:TFLREUnicodeCharClass;
    Range:TFLREUnicodeCharClassRange;
    Lo,Hi:longword;
begin
 Optimize;
 Inverted:=not Inverted;
 if assigned(First) and (First=Last) and (First.Lo=0) and (First.Hi=$ffffffff) then begin
  First.Free;
 end else if not assigned(First) then begin
  TFLREUnicodeCharClassRange.Create(self,0,$ffffffff);
 end else begin
  NewList:=TFLREUnicodeCharClass.Create;
  try
   Range:=First;
   if Range.Lo>0 then begin
    TFLREUnicodeCharClassRange.Create(NewList,0,Range.Lo-1);
   end;
   Lo:=Range.Hi;
   Range:=Range.Next;
   while assigned(Range) do begin
    if (Lo+1)<Range.Lo then begin
     Hi:=Range.Lo;
     TFLREUnicodeCharClassRange.Create(NewList,Lo+1,Hi-1);
    end;
    Lo:=Range.Hi;
    Range:=Range.Next;
   end;
   if Lo<>$ffffffff then begin
    TFLREUnicodeCharClassRange.Create(NewList,Lo+1,$ffffffff);
   end;
   while assigned(First) do begin
    First.Free;
   end;
   Range:=NewList.First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
   First:=NewList.First;
   Last:=NewList.Last;
   NewList.First:=nil;
   NewList.Last:=nil;
   Range:=First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
  finally
   NewList.Free;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.Canonicalize;
var NewList:TFLREUnicodeCharClass;
    Range:TFLREUnicodeCharClassRange;
    OldInverted:boolean;
begin
 if not Canonicalized then begin
  NewList:=TFLREUnicodeCharClass.Create;
  try
   OldInverted:=Inverted;
   if Inverted then begin
    Invert;
   end;
   Range:=First;
   while assigned(Range) do begin
    NewList.AddRange(Range.Lo,Range.Hi,true);
    Range:=Range.Next;
   end;
   while assigned(First) do begin
    First.Free;
   end;
   First:=NewList.First;
   Last:=NewList.Last;
   NewList.First:=nil;
   NewList.Last:=nil;
   Range:=First;
   while assigned(Range) do begin
    Range.CharClass:=self;
    Range:=Range.Next;
   end;
   if OldInverted then begin
    Invert;
   end;
   Inverted:=OldInverted;
  finally
   NewList.Free;
  end;
  Canonicalized:=true;
 end;
end;

procedure TFLREUnicodeCharClass.CompileBinarySearchTree;
type PTFLREUnicodeCharClassRange=^TFLREUnicodeCharClassRange;
var Ranges:TFLREUnicodeCharClassRanges;
 procedure Process(Parent:PTFLREUnicodeCharClassRange;LowIndex,HighIndex:longint);
 var Middle:longint;
 begin
  while LowIndex<=HighIndex do begin
   Middle:=(LowIndex+HighIndex) div 2;
   Parent^:=Ranges[Middle];
   case HighIndex-LowIndex of
    0:begin
     break;
    end;
    1:begin
     if Middle=LowIndex then begin
      Parent:=@Parent^.Right;
      LowIndex:=Middle+1;
     end else begin
      Parent:=@Parent^.Left;
      HighIndex:=Middle-1;
     end;
    end;
    else begin
     Process(@Parent^.Left,LowIndex,Middle-1);
     Parent:=@Parent^.Right;
     LowIndex:=Middle+1;
    end;
   end;
  end;
 end;
var Range:TFLREUnicodeCharClassRange;
    Count:longint;
begin
 Root:=nil;
 Ranges:=nil;
 try
  Count:=0;
  Range:=First;
  while assigned(Range) do begin
   inc(Count);
   Range:=Range.Next;
  end;
  SetLength(Ranges,Count);
  Count:=0;
  Range:=First;
  while assigned(Range) do begin
   Ranges[Count]:=Range;
   inc(Count);
   Range:=Range.Next;
  end;
  if Count>0 then begin
   Process(@Root,0,Count-1);
  end;
 finally
  SetLength(Ranges,0);
 end;
end;

procedure TFLREUnicodeCharClass.CompileCharset;
var Range:TFLREUnicodeCharClassRange;
begin
 CharSet:=[];
 Range:=First;
 while assigned(Range) and (Range.Lo<256) do begin
  if Range.Lo=Range.Hi then begin
   System.Include(CharSet,ansichar(byte(Range.Lo)));
  end else begin
   if Range.Hi<256 then begin
    CharSet:=CharSet+[ansichar(byte(Range.Lo))..ansichar(byte(Range.Hi))];
   end else begin
    CharSet:=CharSet+[ansichar(byte(Range.Lo))..#$ff];
   end;
  end;
  Range:=Range.Next;
 end;
end;

procedure TFLREUnicodeCharClass.Finalize;
begin
 CompileCharset;
 CompileBinarySearchTree;
end;

function TFLREUnicodeCharClass.Count:longword;
var Range:TFLREUnicodeCharClassRange;
begin
 result:=0;
 Range:=First;
 while assigned(Range) do begin
  inc(result,(Range.Hi-Range.Lo)+1);
  Range:=Range.Next;
 end;
end;

function TFLREUnicodeCharClass.Contains(c:longword):boolean;
var Range:TFLREUnicodeCharClassRange;
begin
 result:=false;
 Range:=Root;
 if assigned(Range) and ((c>=First.Lo) and (c<=Last.Hi)) then begin
  if First=Last then begin
   result:=true;
   exit;
  end else begin
   if c<256 then begin
    result:=ansichar(byte(c)) in CharSet;
   end else begin
    repeat
     if (c>=Range.Lo) and (c<=Range.Hi) then begin
      result:=true;
      break;
     end;
     if c<Range.Lo then begin
      Range:=Range.Left;
      continue;
     end;
     if Range.Hi<c then begin
      Range:=Range.Right;
      continue;
     end;
     result:=false;
     break;
    until not assigned(Range);
   end;
  end;
 end;
end;

function TFLREUnicodeCharClass.Intersects(Other:TFLREUnicodeCharClass):boolean;
var Range1,Range2:TFLREUnicodeCharClassRange;
begin
 result:=false;
 if (assigned(Other) and (assigned(First) and assigned(Other.First))) and ((First.Lo<=Other.Last.Hi) and (Other.First.Lo<=Last.Hi)) then begin
  Range1:=First;
  while assigned(Range1) do begin
   Range2:=Other.First;
   while assigned(Range2) do begin
    if (Range1.Lo<=Range2.Hi) and (Range2.Lo<=Range1.Hi) then begin
     result:=true;
     exit;
    end;
    Range2:=Range2.Next;
   end;
   Range1:=Range1.Next;
  end;
 end;
end;

function TFLREUnicodeCharClass.IsSingle:boolean;
begin
 result:=(First=Last) and ((assigned(First) and (First.Lo=First.Hi)) or not assigned(First));
end;

function TFLREUnicodeCharClass.HashCode:longword;
var Range:TFLREUnicodeCharClassRange;
    h,i:longword;
begin
 result:=$811c9dc5;
 Range:=First;
 h:=0;
 while assigned(Range) do begin
  inc(h);
  Range:=Range.Next;
 end;
 Range:=First;
 while assigned(Range) do begin
  i:=((Range.Hi and $ffff) shl 16) or (Range.Lo and $ffff);
  h:=(h xor i) xor $2e63823a;
  inc(h,(h shl 15) or (h shr (32-15)));
  dec(h,(h shl 9) or (h shr (32-9)));
  inc(h,(h shl 4) or (h shr (32-4)));
  dec(h,(h shl 1) or (h shr (32-1)));
  h:=h xor (h shl 2) or (h shr (32-2));
  result:=result xor i;
  inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  i:=(Range.Hi and $ffff0000) or ((Range.Lo and $ffff0000) shr 16);
  h:=(h xor i) xor $2e63823a;
  inc(h,(h shl 15) or (h shr (32-15)));
  dec(h,(h shl 9) or (h shr (32-9)));
  inc(h,(h shl 4) or (h shr (32-4)));
  dec(h,(h shl 1) or (h shr (32-1)));
  h:=h xor (h shl 2) or (h shr (32-2));
  result:=result xor i;
  inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  Range:=Range.Next;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;

function CompareCharClasses(c1,c2:TFLREUnicodeCharClass):longint;
var r1,r2:TFLREUnicodeCharClassRange;
begin
 r1:=c1.First;
 r2:=c2.First;
 while assigned(r1) and assigned(r2) do begin
  if r1.Lo<>r2.Lo then begin
   result:=longint(r1.Lo)-longint(r2.Lo);
   exit;
  end;
  if r1.Hi<>r2.Hi then begin
   result:=longint(r1.Hi)-longint(r2.Hi);
   exit;
  end;
  r1:=r1.Next;
  r2:=r2.Next;
 end;
 if assigned(r1) then begin
  result:=1;
 end else if assigned(r2) then begin
  result:=-1;
 end else begin
  result:=0;
 end;
end;

function TFLREUnicodeCharClass.EqualsTo(OtherObject:TFLREUnicodeCharClass):boolean;
begin
 result:=CompareCharClasses(self,TFLREUnicodeCharClass(OtherObject))=0;
end;

procedure InitializeUnicode;
var i,l,h,cl,cu:longword;
    Count:longint;
    s:TFLRERawByteString;
 procedure AddRange(const Table,FirstChar,LastChar:longword);
 begin
  if (Count+1)>length(UnicodeCharRangeClasses[Table]) then begin
   SetLength(UnicodeCharRangeClasses[Table],(Count+4097) and not 4095);
  end;
  UnicodeCharRangeClasses[Table,Count,0]:=FirstChar;
  UnicodeCharRangeClasses[Table,Count,1]:=LastChar;
  inc(Count);
 end;
 procedure AddChar(const Table,TheChar:longword);
 begin
  AddRange(Table,TheChar,TheChar);
 end;
 procedure AddUnicodeBlockAliasTo(const AliasName,MapToName:TFLRERawByteString);
 var s:TFLRERawByteString;
     i:TFLREStringIntegerPairHashMapData;
 begin
  s:=TFLRERawByteString(StringReplace(String(MapToName),' ','_',[rfREPLACEALL]));
  i:=UnicodeBlockHashMap.GetValue(s);
  if i>=0 then begin
   s:=TFLRERawByteString(StringReplace(String(AliasName),' ','_',[rfREPLACEALL]));
   UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
   s:=TFLRERawByteString(StringReplace(String(s),'_','',[rfREPLACEALL]));
   UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
  end;
 end;
 function AddUnicodeCharClassBlockEx(var Blocks,IgnoreCaseBlocks:TUnicodeCharClassBlocks;var CountBlocks:longint;const ACharClass:TFLREUnicodeCharClass):longint;
 var BlockIndex,CountRanges,Index:longint;
     Range:PUnicodeCharClassRange;
     CharClass:TFLREUnicodeCharClass;
     CharClassRange:TFLREUnicodeCharClassRange;
 begin

  BlockIndex:=CountBlocks;
  inc(CountBlocks);

  result:=BlockIndex;

  if CountBlocks>length(Blocks) then begin
   SetLength(Blocks,CountBlocks*2);
  end;

  if CountBlocks>length(IgnoreCaseBlocks) then begin
   SetLength(IgnoreCaseBlocks,CountBlocks*2);
  end;

  CharClass:=TFLREUnicodeCharClass.Create;
  try

   CharClass.Append(ACharClass);

   CharClass.Optimize;

   CountRanges:=0;
   CharClassRange:=CharClass.First;
   while assigned(CharClassRange) do begin
    inc(CountRanges);
    CharClassRange:=CharClassRange.Next;
   end;

   SetLength(Blocks[BlockIndex],CountRanges);

   CountRanges:=0;
   CharClassRange:=CharClass.First;
   while assigned(CharClassRange) do begin
    Range:=@Blocks[BlockIndex,CountRanges];
    inc(CountRanges);
    Range^.FromChar:=CharClassRange.Lo;
    Range^.ToChar:=CharClassRange.Hi;
    CharClassRange:=CharClassRange.Next;
   end;

   SetLength(Blocks[BlockIndex],CountRanges);

   CharClass.Canonicalize;

   CharClass.Optimize;

   CountRanges:=0;
   CharClassRange:=CharClass.First;
   while assigned(CharClassRange) do begin
    inc(CountRanges);
    CharClassRange:=CharClassRange.Next;
   end;

   SetLength(IgnoreCaseBlocks[BlockIndex],CountRanges);

   CountRanges:=0;
   CharClassRange:=CharClass.First;
   while assigned(CharClassRange) do begin
    Range:=@IgnoreCaseBlocks[BlockIndex,CountRanges];
    inc(CountRanges);
    Range^.FromChar:=CharClassRange.Lo;
    Range^.ToChar:=CharClassRange.Hi;
    CharClassRange:=CharClassRange.Next;
   end;

   SetLength(IgnoreCaseBlocks[BlockIndex],CountRanges);

  finally
   CharClass.Free;
  end;

 end;
 function AddUnicodeCharClassBlock(var Blocks,IgnoreCaseBlocks:TUnicodeCharClassBlocks;var CountBlocks:longint;const Ranges:array of longword):longint;
 var Index:longint;
     CharClass:TFLREUnicodeCharClass;
 begin

  CharClass:=TFLREUnicodeCharClass.Create;
  try

   Index:=0;
   while Index<length(Ranges) do begin
    if (Index+1)<length(Ranges) then begin
     CharClass.AddRange(Ranges[Index],Ranges[Index+1],false);
     inc(Index,2);
    end else begin
     CharClass.AddRange(Ranges[Index],Ranges[Index],false);
     break;
    end;
   end;

   CharClass.Optimize;

   result:=AddUnicodeCharClassBlockEx(Blocks,IgnoreCaseBlocks,CountBlocks,CharClass);

  finally
   CharClass.Free;
  end;

 end;
 procedure AddUnicodeAdditionalBlock(const Name:TFLRERawByteString;const Ranges:array of longword);
 var BlockIndex:longint;
     s:TFLRERawByteString;
 begin
  BlockIndex:=AddUnicodeCharClassBlock(UnicodeAdditionalBlocks,UnicodeIgnoreCaseAdditionalBlocks,CountUnicodeAdditionalBlocks,Ranges);
  s:=TFLRERawByteString(StringReplace(String(Name),' ','_',[rfREPLACEALL]));
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct(s),BlockIndex);
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct('In'+s),BlockIndex);
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct('Is'+s),BlockIndex);
  s:=TFLRERawByteString(StringReplace(String(s),'_','',[rfREPLACEALL]));
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct(s),BlockIndex);
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct('In'+s),BlockIndex);
  UnicodeAdditionalBlockHashMap.SetValue(UTF8Correct('Is'+s),BlockIndex);
 end;
var CharClass:TFLREUnicodeCharClass;
begin
 FillChar(UnicodeCharRangeClasses,SizeOf(TUnicodeCharRangeClasses),#0);
 begin
  Count:=0;
  l:=$ffffffff;
  h:=0;
  for i:=0 to $10ffff do begin
   if (UnicodeGetCategoryFromTable(i) in [FLREUnicodeCategoryLu,FLREUnicodeCategoryLl,FLREUnicodeCategoryLt,FLREUnicodeCategoryLm,FLREUnicodeCategoryLo,FLREUnicodeCategoryNd,FLREUnicodeCategoryNl,FLREUnicodeCategoryNo,FLREUnicodeCategoryPc]) or (i=ord('_')) then begin
    if l<=h then begin
     if (h+1)=i then begin
      h:=i;
     end else begin
      AddRange(ucrWORDS,l,h);
      l:=i;
      h:=i;
     end;
    end else begin
     l:=i;
     h:=i;
    end;
   end;
  end;
  if l<=h then begin
   AddRange(ucrWORDS,l,h);
  end;
  SetLength(UnicodeCharRangeClasses[ucrWORDS],Count);
 end;
 begin
  LowerUpperCaseUnicodeCharClass:=TFLREUnicodeCharClass.Create;
  Count:=0;
  l:=$ffffffff;
  h:=0;
  for i:=0 to $10ffff do begin
   cl:=UnicodeToLower(i);
   cu:=UnicodeToUpper(i);
   if (cl<>cu) or (cl<>i) or (cu<>i) then begin
    if l<=h then begin
     if (h+1)=i then begin
      h:=i;
     end else begin
      LowerUpperCaseUnicodeCharClass.AddRange(l,h);
      l:=i;
      h:=i;
     end;
    end else begin
     l:=i;
     h:=i;
    end;
   end;
  end;
  if l<=h then begin
   LowerUpperCaseUnicodeCharClass.AddRange(l,h);
  end;
 end;
 begin
  Count:=0;
  l:=$ffffffff;
  h:=0;
  for i:=0 to $10ffff do begin
   if UnicodeGetCategoryFromTable(i) in [FLREUnicodeCategoryNd] then begin
    if l<=h then begin
     if (h+1)=i then begin
      h:=i;
     end else begin
      AddRange(ucrDIGITS,l,h);
      l:=i;
      h:=i;
     end;
    end else begin
     l:=i;
     h:=i;
    end;
   end;
  end;
  if l<=h then begin
   AddRange(ucrDIGITS,l,h);
  end;
  SetLength(UnicodeCharRangeClasses[ucrDIGITS],Count);
 end;
 begin
  Count:=0;
{ AddRange(ucrWHITESPACES,$0009,$000d);
  AddChar(ucrWHITESPACES,$0020);
  AddChar(ucrWHITESPACES,$00a0);
  AddChar(ucrWHITESPACES,$1680);
  AddChar(ucrWHITESPACES,$180e);
  AddRange(ucrWHITESPACES,$2000,$200b);
  AddRange(ucrWHITESPACES,$2028,$2029);
  AddChar(ucrWHITESPACES,$202f);
  AddChar(ucrWHITESPACES,$205f);
  AddChar(ucrWHITESPACES,$3000);
  AddChar(ucrWHITESPACES,$fffe);
  AddChar(ucrWHITESPACES,$feff);{}
{}l:=$ffffffff;
  h:=0;
  for i:=0 to $10ffff do begin
   if UnicodeIsWhiteSpace(i) then begin
    if l<=h then begin
     if (h+1)=i then begin
      h:=i;
     end else begin
      AddRange(ucrWHITESPACES,l,h);
      l:=i;
      h:=i;
     end;
    end else begin
     l:=i;
     h:=i;
    end;
   end;
  end;
  if l<=h then begin
   AddRange(ucrWHITESPACES,l,h);
  end;{}
  SetLength(UnicodeCharRangeClasses[ucrWHITESPACES],Count);
 end;
 begin
  UnicodeClassHashMap:=TFLREStringIntegerPairHashMap.Create;
  for i:=0 to FLREUnicodeCategoryCount-1 do begin
   s:=FLREUnicodeCategoryIDs[i];
   UnicodeClassHashMap.SetValue(UTF8Correct(s),1 shl i);
   UnicodeClassHashMap.SetValue(UTF8Correct('Is'+s),1 shl i);
   UnicodeClassHashMap.SetValue(UTF8Correct('In'+s),1 shl i);
  end;
  begin
   UnicodeClassHashMap.SetValue(UTF8Correct('Lu'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Uppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('UppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Ll'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Lowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('LowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Lt'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Titlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('TitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Lm'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Modifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('ModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Lo'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Other_Letter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('OtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('L'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
   UnicodeClassHashMap.SetValue(UTF8Correct('Mn'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('Non_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('NonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('Me'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('Enclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('EnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('Mc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('Spacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('SpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('M'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('Mark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('Nd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Decimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('DecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Nl'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Letter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('LetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('No'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('Other_Number'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('OtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('N'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('Number'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('Zs'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Space_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('SpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Zl'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Line_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('LineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Zp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Paragraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('ParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('Z'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('Seperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('Cc'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Control'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Cf'),1 shl FLRE_CT_FORMAT);
   UnicodeClassHashMap.SetValue(UTF8Correct('Format'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Co'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('Private_Use'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('PrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('Cs'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('Surrogate'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('Cn'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('Unassigned'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('C'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('Other'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('Pd'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Dash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('DashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Ps'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Open_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('OpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Pe'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Close_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('ClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Pi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Initial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Pf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Final_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('FinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Pc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Connector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('ConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Po'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('Other_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('OtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('P'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('Punctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('Sm'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Math_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('MathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Sc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Currency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('CurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Sk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Modifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('ModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('So'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('Other_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('OtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('S'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   UnicodeClassHashMap.SetValue(UTF8Correct('Symbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
  end;
  begin
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLu'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsUppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsUppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLl'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLt'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsTitlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsTitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLm'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsModifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLo'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOther_Letter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsL'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMn'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNon_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMe'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsEnclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsEnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSpacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsM'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsDecimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsDecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNl'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLetter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNo'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOther_Number'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsN'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsNumber'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsZs'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSpace_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsZl'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLine_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsLineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsZp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsParagraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsZ'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSeperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCc'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsControl'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCf'),1 shl FLRE_CT_FORMAT);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsFormat'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCo'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPrivate_Use'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCs'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSurrogate'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCn'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsUnassigned'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsC'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOther'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPd'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsDash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsDashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPs'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOpen_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPe'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsClose_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsInitial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsInitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsFinal_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsFinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsConnector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPo'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOther_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsP'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsPunctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSm'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMath_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsMathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCurrency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsCurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsModifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSo'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOther_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsOtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('IsS'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   UnicodeClassHashMap.SetValue(UTF8Correct('IsSymbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
  end;
  begin
   UnicodeClassHashMap.SetValue(UTF8Correct('InLu'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InUppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InUppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLl'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLt'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InTitlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InTitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLm'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InModifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLo'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOther_Letter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InL'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
   UnicodeClassHashMap.SetValue(UTF8Correct('InMn'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InNon_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InNonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InMe'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InEnclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InEnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InMc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSpacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
   UnicodeClassHashMap.SetValue(UTF8Correct('InM'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('InMark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
   UnicodeClassHashMap.SetValue(UTF8Correct('InNd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InDecimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InDecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InNl'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLetter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InNo'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOther_Number'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
   UnicodeClassHashMap.SetValue(UTF8Correct('InN'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('InNumber'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
   UnicodeClassHashMap.SetValue(UTF8Correct('InZs'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSpace_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InZl'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLine_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InLineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InZp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InParagraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
   UnicodeClassHashMap.SetValue(UTF8Correct('InZ'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('InSeperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
   UnicodeClassHashMap.SetValue(UTF8Correct('InCc'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InControl'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCf'),1 shl FLRE_CT_FORMAT);
   UnicodeClassHashMap.SetValue(UTF8Correct('InFormat'),1 shl FLRE_CT_CONTROL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCo'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPrivate_Use'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCs'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSurrogate'),1 shl FLRE_CT_SURROGATE);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCn'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('InUnassigned'),1 shl FLRE_CT_UNASSIGNED);
   UnicodeClassHashMap.SetValue(UTF8Correct('InC'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('InOther'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
   UnicodeClassHashMap.SetValue(UTF8Correct('InPd'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InDash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InDashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPs'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOpen_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPe'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InClose_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InInitial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InInitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InFinal_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InFinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InConnector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InPo'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOther_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
   UnicodeClassHashMap.SetValue(UTF8Correct('InP'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('InPunctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
   UnicodeClassHashMap.SetValue(UTF8Correct('InSm'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InMath_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InMathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCurrency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InCurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InModifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InSo'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOther_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InOtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
   UnicodeClassHashMap.SetValue(UTF8Correct('InS'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   UnicodeClassHashMap.SetValue(UTF8Correct('InSymbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
  end;
 end;
 begin
  UnicodeScriptHashMap:=TFLREStringIntegerPairHashMap.Create;
  for i:=FLREUnicodeScriptCommon to FLREUnicodeScriptCount-1 do begin
   s:=FLREUnicodeScriptIDs[i];
   UnicodeScriptHashMap.SetValue(UTF8Correct(s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('Is'+s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct(s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('Is'+s),i);
   s:=TFLRERawByteString(StringReplace(String(s),'_','',[rfREPLACEALL]));
   UnicodeScriptHashMap.SetValue(UTF8Correct(s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeScriptHashMap.SetValue(UTF8Correct('Is'+s),i);
  end;
 end;
 begin
  UnicodeBlockHashMap:=TFLREStringIntegerPairHashMap.Create;
  for i:=FLREUnicodeScriptCommon to FLREUnicodeBlockCount-1 do begin
   s:=TFLRERawByteString(StringReplace(String(FLREUnicodeBlocks[i].Name),' ','_',[rfREPLACEALL]));
   UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
   s:=TFLRERawByteString(StringReplace(String(s),'_','',[rfREPLACEALL]));
   UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
   UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
  end;
  AddUnicodeBlockAliasTo('Combining Marks for Symbols','Combining Diacritical Marks for Symbols');
 end;
 begin
  UnicodeCategoryBlocks:=nil;
  UnicodeIgnoreCaseCategoryBlocks:=nil;
  CountUnicodeCategoryBlocks:=0;
  for i:=0 to FLREUnicodeCategoryCount-1 do begin
   CharClass:=TFLREUnicodeCharClass.Create;
   try
    CharClass.AddUnicodeCategory(longword(1) shl i,false);
    Assert(AddUnicodeCharClassBlockEx(UnicodeCategoryBlocks,UnicodeIgnoreCaseCategoryBlocks,CountUnicodeCategoryBlocks,CharClass)=i);
   finally
    CharClass.Free;
   end;
  end;
  SetLength(UnicodeCategoryBlocks,CountUnicodeCategoryBlocks);
  SetLength(UnicodeIgnoreCaseCategoryBlocks,CountUnicodeCategoryBlocks);
 end;
 begin
  UnicodeScriptBlocks:=nil;
  UnicodeIgnoreCaseScriptBlocks:=nil;
  CountUnicodeScriptBlocks:=0;
  for i:=0 to FLREUnicodeScriptCount-1 do begin
   CharClass:=TFLREUnicodeCharClass.Create;
   try
    CharClass.AddUnicodeScript(i,false);
    Assert(AddUnicodeCharClassBlockEx(UnicodeScriptBlocks,UnicodeIgnoreCaseScriptBlocks,CountUnicodeScriptBlocks,CharClass)=i);
   finally
    CharClass.Free;
   end;
  end;
  SetLength(UnicodeScriptBlocks,CountUnicodeScriptBlocks);
  SetLength(UnicodeIgnoreCaseScriptBlocks,CountUnicodeScriptBlocks);
 end;
 begin
  UnicodeBlockBlocks:=nil;
  UnicodeIgnoreCaseBlockBlocks:=nil;
  CountUnicodeBlockBlocks:=0;
  for i:=0 to FLREUnicodeBlockCount-1 do begin
   CharClass:=TFLREUnicodeCharClass.Create;
   try
    CharClass.AddUnicodeBlock(i,false);
    Assert(AddUnicodeCharClassBlockEx(UnicodeBlockBlocks,UnicodeIgnoreCaseBlockBlocks,CountUnicodeBlockBlocks,CharClass)=i) ;
   finally
    CharClass.Free;
   end;
  end;
  SetLength(UnicodeBlockBlocks,CountUnicodeBlockBlocks);
  SetLength(UnicodeIgnoreCaseBlockBlocks,CountUnicodeBlockBlocks);
 end;
 begin
  UnicodeAdditionalBlocks:=nil;
  UnicodeIgnoreCaseAdditionalBlocks:=nil;
  CountUnicodeAdditionalBlocks:=0;
  UnicodeAdditionalBlockHashMap:=TFLREStringIntegerPairHashMap.Create;
  AddUnicodeAdditionalBlock('_xmlC',[$002d,$002f,$0030,$003b,$0041,$005b,$005f,$0060,$0061,$007b,$00b7,$00b8,$00c0,$00d7,$00d8,$00f7,
                                     $00f8,$0132,$0134,$013f,$0141,$0149,$014a,$017f,$0180,$01c4,$01cd,$01f1,$01f4,$01f6,$01fa,$0218,
                                     $0250,$02a9,$02bb,$02c2,$02d0,$02d2,$0300,$0346,$0360,$0362,$0386,$038b,$038c,$038d,$038e,$03a2,
                                     $03a3,$03cf,$03d0,$03d7,$03da,$03db,$03dc,$03dd,$03de,$03df,$03e0,$03e1,$03e2,$03f4,$0401,$040d,
                                     $040e,$0450,$0451,$045d,$045e,$0482,$0483,$0487,$0490,$04c5,$04c7,$04c9,$04cb,$04cd,$04d0,$04ec,
                                     $04ee,$04f6,$04f8,$04fa,$0531,$0557,$0559,$055a,$0561,$0587,$0591,$05a2,$05a3,$05ba,$05bb,$05be,
                                     $05bf,$05c0,$05c1,$05c3,$05c4,$05c5,$05d0,$05eb,$05f0,$05f3,$0621,$063b,$0640,$0653,$0660,$066a,
                                     $0670,$06b8,$06ba,$06bf,$06c0,$06cf,$06d0,$06d4,$06d5,$06e9,$06ea,$06ee,$06f0,$06fa,$0901,$0904,
                                     $0905,$093a,$093c,$094e,$0951,$0955,$0958,$0964,$0966,$0970,$0981,$0984,$0985,$098d,$098f,$0991,
                                     $0993,$09a9,$09aa,$09b1,$09b2,$09b3,$09b6,$09ba,$09bc,$09bd,$09be,$09c5,$09c7,$09c9,$09cb,$09ce,
                                     $09d7,$09d8,$09dc,$09de,$09df,$09e4,$09e6,$09f2,$0a02,$0a03,$0a05,$0a0b,$0a0f,$0a11,$0a13,$0a29,
                                     $0a2a,$0a31,$0a32,$0a34,$0a35,$0a37,$0a38,$0a3a,$0a3c,$0a3d,$0a3e,$0a43,$0a47,$0a49,$0a4b,$0a4e,
                                     $0a59,$0a5d,$0a5e,$0a5f,$0a66,$0a75,$0a81,$0a84,$0a85,$0a8c,$0a8d,$0a8e,$0a8f,$0a92,$0a93,$0aa9,
                                     $0aaa,$0ab1,$0ab2,$0ab4,$0ab5,$0aba,$0abc,$0ac6,$0ac7,$0aca,$0acb,$0ace,$0ae0,$0ae1,$0ae6,$0af0,
                                     $0b01,$0b04,$0b05,$0b0d,$0b0f,$0b11,$0b13,$0b29,$0b2a,$0b31,$0b32,$0b34,$0b36,$0b3a,$0b3c,$0b44,
                                     $0b47,$0b49,$0b4b,$0b4e,$0b56,$0b58,$0b5c,$0b5e,$0b5f,$0b62,$0b66,$0b70,$0b82,$0b84,$0b85,$0b8b,
                                     $0b8e,$0b91,$0b92,$0b96,$0b99,$0b9b,$0b9c,$0b9d,$0b9e,$0ba0,$0ba3,$0ba5,$0ba8,$0bab,$0bae,$0bb6,
                                     $0bb7,$0bba,$0bbe,$0bc3,$0bc6,$0bc9,$0bca,$0bce,$0bd7,$0bd8,$0be7,$0bf0,$0c01,$0c04,$0c05,$0c0d,
                                     $0c0e,$0c11,$0c12,$0c29,$0c2a,$0c34,$0c35,$0c3a,$0c3e,$0c45,$0c46,$0c49,$0c4a,$0c4e,$0c55,$0c57,
                                     $0c60,$0c62,$0c66,$0c70,$0c82,$0c84,$0c85,$0c8d,$0c8e,$0c91,$0c92,$0ca9,$0caa,$0cb4,$0cb5,$0cba,
                                     $0cbe,$0cc5,$0cc6,$0cc9,$0cca,$0cce,$0cd5,$0cd7,$0cde,$0cdf,$0ce0,$0ce2,$0ce6,$0cf0,$0d02,$0d04,
                                     $0d05,$0d0d,$0d0e,$0d11,$0d12,$0d29,$0d2a,$0d3a,$0d3e,$0d44,$0d46,$0d49,$0d4a,$0d4e,$0d57,$0d58,
                                     $0d60,$0d62,$0d66,$0d70,$0e01,$0e2f,$0e30,$0e3b,$0e40,$0e4f,$0e50,$0e5a,$0e81,$0e83,$0e84,$0e85,
                                     $0e87,$0e89,$0e8a,$0e8b,$0e8d,$0e8e,$0e94,$0e98,$0e99,$0ea0,$0ea1,$0ea4,$0ea5,$0ea6,$0ea7,$0ea8,
                                     $0eaa,$0eac,$0ead,$0eaf,$0eb0,$0eba,$0ebb,$0ebe,$0ec0,$0ec5,$0ec6,$0ec7,$0ec8,$0ece,$0ed0,$0eda,
                                     $0f18,$0f1a,$0f20,$0f2a,$0f35,$0f36,$0f37,$0f38,$0f39,$0f3a,$0f3e,$0f48,$0f49,$0f6a,$0f71,$0f85,
                                     $0f86,$0f8c,$0f90,$0f96,$0f97,$0f98,$0f99,$0fae,$0fb1,$0fb8,$0fb9,$0fba,$10a0,$10c6,$10d0,$10f7,
                                     $1100,$1101,$1102,$1104,$1105,$1108,$1109,$110a,$110b,$110d,$110e,$1113,$113c,$113d,$113e,$113f,
                                     $1140,$1141,$114c,$114d,$114e,$114f,$1150,$1151,$1154,$1156,$1159,$115a,$115f,$1162,$1163,$1164,
                                     $1165,$1166,$1167,$1168,$1169,$116a,$116d,$116f,$1172,$1174,$1175,$1176,$119e,$119f,$11a8,$11a9,
                                     $11ab,$11ac,$11ae,$11b0,$11b7,$11b9,$11ba,$11bb,$11bc,$11c3,$11eb,$11ec,$11f0,$11f1,$11f9,$11fa,
                                     $1e00,$1e9c,$1ea0,$1efa,$1f00,$1f16,$1f18,$1f1e,$1f20,$1f46,$1f48,$1f4e,$1f50,$1f58,$1f59,$1f5a,
                                     $1f5b,$1f5c,$1f5d,$1f5e,$1f5f,$1f7e,$1f80,$1fb5,$1fb6,$1fbd,$1fbe,$1fbf,$1fc2,$1fc5,$1fc6,$1fcd,
                                     $1fd0,$1fd4,$1fd6,$1fdc,$1fe0,$1fed,$1ff2,$1ff5,$1ff6,$1ffd,$20d0,$20dd,$20e1,$20e2,$2126,$2127,
                                     $212a,$212c,$212e,$212f,$2180,$2183,$3005,$3006,$3007,$3008,$3021,$3030,$3031,$3036,$3041,$3095,
                                     $3099,$309b,$309d,$309f,$30a1,$30fb,$30fc,$30ff,$3105,$312d,$4e00,$9fa6,$ac00,$d7a4]);
  AddUnicodeAdditionalBlock('_xmlD',[$0030,$003a,$0660,$066a,$06f0,$06fa,$0966,$0970,$09e6,$09f0,$0a66,$0a70,$0ae6,$0af0,$0b66,$0b70,
                                     $0be7,$0bf0,$0c66,$0c70,$0ce6,$0cf0,$0d66,$0d70,$0e50,$0e5a,$0ed0,$0eda,$0f20,$0f2a,$1040,$104a,
                                     $1369,$1372,$17e0,$17ea,$1810,$181a,$ff10,$ff1a]);
  AddUnicodeAdditionalBlock('_xmlI',[$003a,$003b,$0041,$005b,$005f,$0060,$0061,$007b,$00c0,$00d7,$00d8,$00f7,$00f8,$0132,$0134,$013f,
                                     $0141,$0149,$014a,$017f,$0180,$01c4,$01cd,$01f1,$01f4,$01f6,$01fa,$0218,$0250,$02a9,$02bb,$02c2,
                                     $0386,$0387,$0388,$038b,$038c,$038d,$038e,$03a2,$03a3,$03cf,$03d0,$03d7,$03da,$03db,$03dc,$03dd,
                                     $03de,$03df,$03e0,$03e1,$03e2,$03f4,$0401,$040d,$040e,$0450,$0451,$045d,$045e,$0482,$0490,$04c5,
                                     $04c7,$04c9,$04cb,$04cd,$04d0,$04ec,$04ee,$04f6,$04f8,$04fa,$0531,$0557,$0559,$055a,$0561,$0587,
                                     $05d0,$05eb,$05f0,$05f3,$0621,$063b,$0641,$064b,$0671,$06b8,$06ba,$06bf,$06c0,$06cf,$06d0,$06d4,
                                     $06d5,$06d6,$06e5,$06e7,$0905,$093a,$093d,$093e,$0958,$0962,$0985,$098d,$098f,$0991,$0993,$09a9,
                                     $09aa,$09b1,$09b2,$09b3,$09b6,$09ba,$09dc,$09de,$09df,$09e2,$09f0,$09f2,$0a05,$0a0b,$0a0f,$0a11,
                                     $0a13,$0a29,$0a2a,$0a31,$0a32,$0a34,$0a35,$0a37,$0a38,$0a3a,$0a59,$0a5d,$0a5e,$0a5f,$0a72,$0a75,
                                     $0a85,$0a8c,$0a8d,$0a8e,$0a8f,$0a92,$0a93,$0aa9,$0aaa,$0ab1,$0ab2,$0ab4,$0ab5,$0aba,$0abd,$0abe,
                                     $0ae0,$0ae1,$0b05,$0b0d,$0b0f,$0b11,$0b13,$0b29,$0b2a,$0b31,$0b32,$0b34,$0b36,$0b3a,$0b3d,$0b3e,
                                     $0b5c,$0b5e,$0b5f,$0b62,$0b85,$0b8b,$0b8e,$0b91,$0b92,$0b96,$0b99,$0b9b,$0b9c,$0b9d,$0b9e,$0ba0,
                                     $0ba3,$0ba5,$0ba8,$0bab,$0bae,$0bb6,$0bb7,$0bba,$0c05,$0c0d,$0c0e,$0c11,$0c12,$0c29,$0c2a,$0c34,
                                     $0c35,$0c3a,$0c60,$0c62,$0c85,$0c8d,$0c8e,$0c91,$0c92,$0ca9,$0caa,$0cb4,$0cb5,$0cba,$0cde,$0cdf,
                                     $0ce0,$0ce2,$0d05,$0d0d,$0d0e,$0d11,$0d12,$0d29,$0d2a,$0d3a,$0d60,$0d62,$0e01,$0e2f,$0e30,$0e31,
                                     $0e32,$0e34,$0e40,$0e46,$0e81,$0e83,$0e84,$0e85,$0e87,$0e89,$0e8a,$0e8b,$0e8d,$0e8e,$0e94,$0e98,
                                     $0e99,$0ea0,$0ea1,$0ea4,$0ea5,$0ea6,$0ea7,$0ea8,$0eaa,$0eac,$0ead,$0eaf,$0eb0,$0eb1,$0eb2,$0eb4,
                                     $0ebd,$0ebe,$0ec0,$0ec5,$0f40,$0f48,$0f49,$0f6a,$10a0,$10c6,$10d0,$10f7,$1100,$1101,$1102,$1104,
                                     $1105,$1108,$1109,$110a,$110b,$110d,$110e,$1113,$113c,$113d,$113e,$113f,$1140,$1141,$114c,$114d,
                                     $114e,$114f,$1150,$1151,$1154,$1156,$1159,$115a,$115f,$1162,$1163,$1164,$1165,$1166,$1167,$1168,
                                     $1169,$116a,$116d,$116f,$1172,$1174,$1175,$1176,$119e,$119f,$11a8,$11a9,$11ab,$11ac,$11ae,$11b0,
                                     $11b7,$11b9,$11ba,$11bb,$11bc,$11c3,$11eb,$11ec,$11f0,$11f1,$11f9,$11fa,$1e00,$1e9c,$1ea0,$1efa,
                                     $1f00,$1f16,$1f18,$1f1e,$1f20,$1f46,$1f48,$1f4e,$1f50,$1f58,$1f59,$1f5a,$1f5b,$1f5c,$1f5d,$1f5e,
                                     $1f5f,$1f7e,$1f80,$1fb5,$1fb6,$1fbd,$1fbe,$1fbf,$1fc2,$1fc5,$1fc6,$1fcd,$1fd0,$1fd4,$1fd6,$1fdc,
                                     $1fe0,$1fed,$1ff2,$1ff5,$1ff6,$1ffd,$2126,$2127,$212a,$212c,$212e,$212f,$2180,$2183,$3007,$3008,
                                     $3021,$302a,$3041,$3095,$30a1,$30fb,$3105,$312d,$4e00,$9fa6,$ac00,$d7a4]);
  AddUnicodeAdditionalBlock('_xmlW',[$0024,$0025,$002b,$002c,$0030,$003a,$003c,$003f,$0041,$005b,$005e,$005f,$0060,$007b,$007c,$007d,
                                     $007e,$007f,$00a2,$00ab,$00ac,$00ad,$00ae,$00b7,$00b8,$00bb,$00bc,$00bf,$00c0,$0221,$0222,$0234,
                                     $0250,$02ae,$02b0,$02ef,$0300,$0350,$0360,$0370,$0374,$0376,$037a,$037b,$0384,$0387,$0388,$038b,
                                     $038c,$038d,$038e,$03a2,$03a3,$03cf,$03d0,$03f7,$0400,$0487,$0488,$04cf,$04d0,$04f6,$04f8,$04fa,
                                     $0500,$0510,$0531,$0557,$0559,$055a,$0561,$0588,$0591,$05a2,$05a3,$05ba,$05bb,$05be,$05bf,$05c0,
                                     $05c1,$05c3,$05c4,$05c5,$05d0,$05eb,$05f0,$05f3,$0621,$063b,$0640,$0656,$0660,$066a,$066e,$06d4,
                                     $06d5,$06dd,$06de,$06ee,$06f0,$06ff,$0710,$072d,$0730,$074b,$0780,$07b2,$0901,$0904,$0905,$093a,
                                     $093c,$094e,$0950,$0955,$0958,$0964,$0966,$0970,$0981,$0984,$0985,$098d,$098f,$0991,$0993,$09a9,
                                     $09aa,$09b1,$09b2,$09b3,$09b6,$09ba,$09bc,$09bd,$09be,$09c5,$09c7,$09c9,$09cb,$09ce,$09d7,$09d8,
                                     $09dc,$09de,$09df,$09e4,$09e6,$09fb,$0a02,$0a03,$0a05,$0a0b,$0a0f,$0a11,$0a13,$0a29,$0a2a,$0a31,
                                     $0a32,$0a34,$0a35,$0a37,$0a38,$0a3a,$0a3c,$0a3d,$0a3e,$0a43,$0a47,$0a49,$0a4b,$0a4e,$0a59,$0a5d,
                                     $0a5e,$0a5f,$0a66,$0a75,$0a81,$0a84,$0a85,$0a8c,$0a8d,$0a8e,$0a8f,$0a92,$0a93,$0aa9,$0aaa,$0ab1,
                                     $0ab2,$0ab4,$0ab5,$0aba,$0abc,$0ac6,$0ac7,$0aca,$0acb,$0ace,$0ad0,$0ad1,$0ae0,$0ae1,$0ae6,$0af0,
                                     $0b01,$0b04,$0b05,$0b0d,$0b0f,$0b11,$0b13,$0b29,$0b2a,$0b31,$0b32,$0b34,$0b36,$0b3a,$0b3c,$0b44,
                                     $0b47,$0b49,$0b4b,$0b4e,$0b56,$0b58,$0b5c,$0b5e,$0b5f,$0b62,$0b66,$0b71,$0b82,$0b84,$0b85,$0b8b,
                                     $0b8e,$0b91,$0b92,$0b96,$0b99,$0b9b,$0b9c,$0b9d,$0b9e,$0ba0,$0ba3,$0ba5,$0ba8,$0bab,$0bae,$0bb6,
                                     $0bb7,$0bba,$0bbe,$0bc3,$0bc6,$0bc9,$0bca,$0bce,$0bd7,$0bd8,$0be7,$0bf3,$0c01,$0c04,$0c05,$0c0d,
                                     $0c0e,$0c11,$0c12,$0c29,$0c2a,$0c34,$0c35,$0c3a,$0c3e,$0c45,$0c46,$0c49,$0c4a,$0c4e,$0c55,$0c57,
                                     $0c60,$0c62,$0c66,$0c70,$0c82,$0c84,$0c85,$0c8d,$0c8e,$0c91,$0c92,$0ca9,$0caa,$0cb4,$0cb5,$0cba,
                                     $0cbe,$0cc5,$0cc6,$0cc9,$0cca,$0cce,$0cd5,$0cd7,$0cde,$0cdf,$0ce0,$0ce2,$0ce6,$0cf0,$0d02,$0d04,
                                     $0d05,$0d0d,$0d0e,$0d11,$0d12,$0d29,$0d2a,$0d3a,$0d3e,$0d44,$0d46,$0d49,$0d4a,$0d4e,$0d57,$0d58,
                                     $0d60,$0d62,$0d66,$0d70,$0d82,$0d84,$0d85,$0d97,$0d9a,$0db2,$0db3,$0dbc,$0dbd,$0dbe,$0dc0,$0dc7,
                                     $0dca,$0dcb,$0dcf,$0dd5,$0dd6,$0dd7,$0dd8,$0de0,$0df2,$0df4,$0e01,$0e3b,$0e3f,$0e4f,$0e50,$0e5a,
                                     $0e81,$0e83,$0e84,$0e85,$0e87,$0e89,$0e8a,$0e8b,$0e8d,$0e8e,$0e94,$0e98,$0e99,$0ea0,$0ea1,$0ea4,
                                     $0ea5,$0ea6,$0ea7,$0ea8,$0eaa,$0eac,$0ead,$0eba,$0ebb,$0ebe,$0ec0,$0ec5,$0ec6,$0ec7,$0ec8,$0ece,
                                     $0ed0,$0eda,$0edc,$0ede,$0f00,$0f04,$0f13,$0f3a,$0f3e,$0f48,$0f49,$0f6b,$0f71,$0f85,$0f86,$0f8c,
                                     $0f90,$0f98,$0f99,$0fbd,$0fbe,$0fcd,$0fcf,$0fd0,$1000,$1022,$1023,$1028,$1029,$102b,$102c,$1033,
                                     $1036,$103a,$1040,$104a,$1050,$105a,$10a0,$10c6,$10d0,$10f9,$1100,$115a,$115f,$11a3,$11a8,$11fa,
                                     $1200,$1207,$1208,$1247,$1248,$1249,$124a,$124e,$1250,$1257,$1258,$1259,$125a,$125e,$1260,$1287,
                                     $1288,$1289,$128a,$128e,$1290,$12af,$12b0,$12b1,$12b2,$12b6,$12b8,$12bf,$12c0,$12c1,$12c2,$12c6,
                                     $12c8,$12cf,$12d0,$12d7,$12d8,$12ef,$12f0,$130f,$1310,$1311,$1312,$1316,$1318,$131f,$1320,$1347,
                                     $1348,$135b,$1369,$137d,$13a0,$13f5,$1401,$166d,$166f,$1677,$1681,$169b,$16a0,$16eb,$16ee,$16f1,
                                     $1700,$170d,$170e,$1715,$1720,$1735,$1740,$1754,$1760,$176d,$176e,$1771,$1772,$1774,$1780,$17d4,
                                     $17d7,$17d8,$17db,$17dd,$17e0,$17ea,$180b,$180e,$1810,$181a,$1820,$1878,$1880,$18aa,$1e00,$1e9c,
                                     $1ea0,$1efa,$1f00,$1f16,$1f18,$1f1e,$1f20,$1f46,$1f48,$1f4e,$1f50,$1f58,$1f59,$1f5a,$1f5b,$1f5c,
                                     $1f5d,$1f5e,$1f5f,$1f7e,$1f80,$1fb5,$1fb6,$1fc5,$1fc6,$1fd4,$1fd6,$1fdc,$1fdd,$1ff0,$1ff2,$1ff5,
                                     $1ff6,$1fff,$2044,$2045,$2052,$2053,$2070,$2072,$2074,$207d,$207f,$208d,$20a0,$20b2,$20d0,$20eb,
                                     $2100,$213b,$213d,$214c,$2153,$2184,$2190,$2329,$232b,$23b4,$23b7,$23cf,$2400,$2427,$2440,$244b,
                                     $2460,$24ff,$2500,$2614,$2616,$2618,$2619,$267e,$2680,$268a,$2701,$2705,$2706,$270a,$270c,$2728,
                                     $2729,$274c,$274d,$274e,$274f,$2753,$2756,$2757,$2758,$275f,$2761,$2768,$2776,$2795,$2798,$27b0,
                                     $27b1,$27bf,$27d0,$27e6,$27f0,$2983,$2999,$29d8,$29dc,$29fc,$29fe,$2b00,$2e80,$2e9a,$2e9b,$2ef4,
                                     $2f00,$2fd6,$2ff0,$2ffc,$3004,$3008,$3012,$3014,$3020,$3030,$3031,$303d,$303e,$3040,$3041,$3097,
                                     $3099,$30a0,$30a1,$30fb,$30fc,$3100,$3105,$312d,$3131,$318f,$3190,$31b8,$31f0,$321d,$3220,$3244,
                                     $3251,$327c,$327f,$32cc,$32d0,$32ff,$3300,$3377,$337b,$33de,$33e0,$33ff,$3400,$4db6,$4e00,$9fa6,
                                     $a000,$a48d,$a490,$a4c7,$ac00,$d7a4,$f900,$fa2e,$fa30,$fa6b,$fb00,$fb07,$fb13,$fb18,$fb1d,$fb37,
                                     $fb38,$fb3d,$fb3e,$fb3f,$fb40,$fb42,$fb43,$fb45,$fb46,$fbb2,$fbd3,$fd3e,$fd50,$fd90,$fd92,$fdc8,
                                     $fdf0,$fdfd,$fe00,$fe10,$fe20,$fe24,$fe62,$fe63,$fe64,$fe67,$fe69,$fe6a,$fe70,$fe75,$fe76,$fefd,
                                     $ff04,$ff05,$ff0b,$ff0c,$ff10,$ff1a,$ff1c,$ff1f,$ff21,$ff3b,$ff3e,$ff3f,$ff40,$ff5b,$ff5c,$ff5d,
                                     $ff5e,$ff5f,$ff66,$ffbf,$ffc2,$ffc8,$ffca,$ffd0,$ffd2,$ffd8,$ffda,$ffdd,$ffe0,$ffe7,$ffe8,$ffef,
                                     $fffc,$fffe]);
  SetLength(UnicodeAdditionalBlocks,CountUnicodeAdditionalBlocks);
  SetLength(UnicodeIgnoreCaseAdditionalBlocks,CountUnicodeAdditionalBlocks);
 end;
 begin
  UnicodeClassLowerCaseHashMap:=TFLREStringIntegerPairHashMap.Create;
  UnicodeClassLowerCaseHashMap.LowerCaseAssignFrom(UnicodeClassHashMap);
 end;
 begin
  UnicodeScriptLowerCaseHashMap:=TFLREStringIntegerPairHashMap.Create;
  UnicodeScriptLowerCaseHashMap.LowerCaseAssignFrom(UnicodeScriptHashMap);
 end;
 begin
  UnicodeBlockLowerCaseHashMap:=TFLREStringIntegerPairHashMap.Create;
  UnicodeBlockLowerCaseHashMap.LowerCaseAssignFrom(UnicodeBlockHashMap);
 end;
 begin
  UnicodeAdditionalBlockLowerCaseHashMap:=TFLREStringIntegerPairHashMap.Create;
  UnicodeAdditionalBlockLowerCaseHashMap.LowerCaseAssignFrom(UnicodeAdditionalBlockHashMap);
 end;
end;

procedure MakeMinimalPerfectHashTable(const StringIntegerPairHashMap:TFLREStringIntegerPairHashMap;const HashTableName:ansistring);
 function HashFromString(Seed:longword;const s:TFLRERawByteString):longword;
 var i:longint;
 begin
  if Seed=0 then begin
   result:=$811c9dc5;
  end else begin
   result:=Seed;
  end;
  for i:=1 to length(s) do begin
   result:=(result+(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24)) xor byte(TFLRERawByteChar(s[i]));
  end;
  result:=result and $7fffffff;
 end;
type PHashBucketItem=^THashBucketItem;
     THashBucketItem=record
      Key:TFLRERawByteString;
      Value:int64;
     end;
     PHashBucket=^THashBucket;
     THashBucket=record
      Items:array of THashBucketItem;
      Count:longint;
     end;
     THashBuckets=array of THashBucket;
var Index,Size,HashValue,OldCount,SubIndex,CountBuckets,TempInteger,BucketIndex,ItemIndex,SlotIndex,CountSlots,FreeListSize,
    Seed:longint;
    Buckets:THashBuckets;
    Bucket:PHashBucket;
    BucketItem:PHashBucketItem;
    TempPointer:pointer;
    MinValue,MaxValue:int64;
    Used:array of boolean;
    Values:array of PHashBucketItem;
    Slots:array of longint;
    Seeds:array of longint;
    FreeList:array of longint;
begin
 Buckets:=nil;
 CountBuckets:=0;
 Used:=nil;
 Values:=nil;
 Slots:=nil;
 Seeds:=nil;
 FreeList:=nil;
 try

  Size:=0;
  for Index:=0 to length(StringIntegerPairHashMap.EntityToCellIndex)-1 do begin
   if StringIntegerPairHashMap.EntityToCellIndex[Index]>=0 then begin
    inc(Size);
   end;
  end;

  if Size>0 then begin

   for Index:=0 to length(StringIntegerPairHashMap.EntityToCellIndex)-1 do begin
    if StringIntegerPairHashMap.EntityToCellIndex[Index]>=0 then begin
     HashValue:=longint(HashFromString(0,StringIntegerPairHashMap.Entities[Index].Key)) mod Size;
     if HashValue>=length(Buckets) then begin
      OldCount:=length(Buckets);
      SetLength(Buckets,(HashValue+1)*2);
      for SubIndex:=OldCount to length(Buckets)-1 do begin
       Bucket:=@Buckets[SubIndex];
       Bucket^.Items:=nil;
       Bucket^.Count:=0;
      end;
     end;
     if CountBuckets<=HashValue then begin
      CountBuckets:=HashValue+1;
     end;
     Bucket:=@Buckets[HashValue];
     inc(Bucket^.Count);
     if Bucket^.Count>length(Bucket^.Items) then begin
      SetLength(Bucket^.Items,Bucket^.Count*2);
     end;
     BucketItem:=@Bucket^.Items[Bucket^.Count-1];
     BucketItem^.Key:=StringIntegerPairHashMap.Entities[Index].Key;
     BucketItem^.Value:=StringIntegerPairHashMap.Entities[Index].Value;
    end;
   end;

   Index:=0;
   while Index<(CountBuckets-1) do begin
    if Buckets[Index].Count<Buckets[Index+1].Count then begin
     TempPointer:=pointer(Buckets[Index].Items);
     pointer(Buckets[Index].Items):=pointer(Buckets[Index+1].Items);
     pointer(Buckets[Index+1].Items):=TempPointer;
     TempInteger:=Buckets[Index].Count;
     Buckets[Index].Count:=Buckets[Index+1].Count;
     Buckets[Index+1].Count:=TempInteger;
     if Index>0 then begin
      dec(Index);
     end else begin
      inc(Index);
     end;
    end else begin
     inc(Index);
    end;
   end;

   SetLength(Used,Size);
   SetLength(Values,Size);
   SetLength(Seeds,Size);

   for Index:=0 to Size-1 do begin
    Used[Index]:=false;
    Values[Index]:=nil;
    Seeds[Index]:=0;
   end;

   SetLength(Slots,0);
   CountSlots:=0;

   BucketIndex:=0;
   while BucketIndex<CountBuckets do begin
    Seed:=1;
    Bucket:=@Buckets[BucketIndex];
    if Bucket^.Count<2 then begin
     break;
    end else begin
     for Index:=0 to Size-1 do begin
      Used[Index]:=false;
     end;
     CountSlots:=0;
     ItemIndex:=0;
     while ItemIndex<Bucket^.Count do begin
      BucketItem:=@Bucket^.Items[ItemIndex];
      SlotIndex:=longint(HashFromString(longword(Seed),BucketItem^.Key)) mod Size;
      if assigned(Values[SlotIndex]) or Used[SlotIndex] then begin
       inc(Seed);
       ItemIndex:=0;
       CountSlots:=0;
       for Index:=0 to Size-1 do begin
        Used[Index]:=false;
       end;
      end else begin
       Used[SlotIndex]:=true;
       inc(CountSlots);
       if CountSlots>length(Slots) then begin
        SetLength(Slots,CountSlots*2);
       end;
       Slots[CountSlots-1]:=SlotIndex;
       inc(ItemIndex);
      end;
     end;
     BucketItem:=@Bucket^.Items[0];
     Seeds[longint(HashFromString(0,BucketItem^.Key)) mod Size]:=Seed;
     for ItemIndex:=0 to Bucket^.Count-1 do begin
      Values[Slots[ItemIndex]]:=@Bucket^.Items[ItemIndex];
     end;
    end;
    inc(BucketIndex);
   end;
   SetLength(Slots,CountSlots);

   FreeList:=nil;
   FreeListSize:=0;
   for Index:=0 to Size-1 do begin
    if not assigned(Values[Index]) then begin
     inc(FreeListSize);
    end;
   end;
   SetLength(FreeList,FreeListSize);
   FreeListSize:=0;
   for Index:=0 to Size-1 do begin
    if not assigned(Values[Index]) then begin
     FreeList[FreeListSize]:=Index;
     inc(FreeListSize);
    end;
   end;

   while BucketIndex<CountBuckets do begin
    Bucket:=@Buckets[BucketIndex];
    if Bucket^.Count=0 then begin
     break;
    end else begin
     Assert(FreeListSize>0);
     dec(FreeListSize);
     SlotIndex:=FreeList[FreeListSize];
     BucketItem:=@Bucket^.Items[0];
     Seeds[longint(HashFromString(0,BucketItem^.Key)) mod Size]:=-(SlotIndex+1);
     Assert(not assigned(Values[SlotIndex]));
     Values[SlotIndex]:=@Bucket^.Items[0];
     inc(BucketIndex);
    end;
   end;

   for Index:=0 to length(StringIntegerPairHashMap.EntityToCellIndex)-1 do begin
    if StringIntegerPairHashMap.EntityToCellIndex[Index]>=0 then begin
     Seed:=Seeds[longint(HashFromString(0,StringIntegerPairHashMap.Entities[Index].Key)) mod length(Seeds)];
     if Seed<0 then begin
      TempPointer:=Values[-(Seed+1)];
     end else begin
      TempPointer:=Values[longint(HashFromString(longword(Seed),StringIntegerPairHashMap.Entities[Index].Key)) mod length(Values)];
     end;
     Assert(assigned(TempPointer) and (PHashBucketItem(TempPointer)^.Key=StringIntegerPairHashMap.Entities[Index].Key));
    end;
   end;

   MinValue:=$7fffffffffffffff;
   MaxValue:=int64(-$7fffffffffffffff)-1; // int64(-$8000000000000000) => parser error under Delphi 7
   for Index:=0 to length(Seeds)-1 do begin
    if MinValue>Seeds[Index] then begin
     MinValue:=Seeds[Index];
    end;
    if MaxValue<Seeds[Index] then begin
     MaxValue:=Seeds[Index];
    end;
   end;
   UnitSourceList.Add('const FLRE'+HashTableName+'SeedSize='+IntToStr(length(Seeds))+';');
   UnitSourceList.Add('      FLRE'+HashTableName+'ValueSize='+IntToStr(length(Values))+';');
   UnitSourceList.Add('      FLRE'+HashTableName+'Size='+IntToStr(Size)+';');
   UnitSourceList.Add('');
   if (MinValue>=int64(-$80)) and (MaxValue<=int64($7f)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'SeedBits=8;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Seeds:array[0..'+IntToStr(length(Seeds)-1)+'] of shortint=(');
   end else if (MinValue>=int64(-$8000)) and (MaxValue<=int64($7fff)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'SeedBits=16;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Seeds:array[0..'+IntToStr(length(Seeds)-1)+'] of smallint=(');
   end else if (MinValue>=-int64($80000000)) and (MaxValue<=int64($7fffffff)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'SeedBits=32;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Seeds:array[0..'+IntToStr(length(Seeds)-1)+'] of longint=(');
   end else begin
    Assert(false,'Ups, seed should be maximal 32-bit signed!');
    UnitSourceList.Add('      FLRE'+HashTableName+'SeedBits=64;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Seeds:array[0..'+IntToStr(length(Seeds)-1)+'] of int64=(');
   end;
   for Index:=0 to length(Seeds)-1 do begin
    if (Index+1)<length(Seeds) then begin
     UnitSourceList.Add(IntToStr(Seeds[Index])+',');
    end else begin
     UnitSourceList.Add(IntToStr(Seeds[Index]));
    end;
   end;
   UnitSourceList.Add(');');
   UnitSourceList.Add('');
   UnitSourceList.Add('      FLRE'+HashTableName+'Keys:array[0..'+IntToStr(length(Values)-1)+'] of TFLRERawByteString=(');
   for Index:=0 to length(Values)-1 do begin
    if assigned(Values[Index]) then begin
     if (Index+1)<length(Values) then begin
      UnitSourceList.Add(''''+PHashBucketItem(Values[Index])^.Key+''',');
     end else begin
      UnitSourceList.Add(''''+PHashBucketItem(Values[Index])^.Key+'''');
     end;
    end else begin
     if (Index+1)<length(Values) then begin
      UnitSourceList.Add(''''',');
     end else begin
      UnitSourceList.Add('''''');
     end;
    end;
   end;
   UnitSourceList.Add(');');
   UnitSourceList.Add('');

   MinValue:=$7fffffffffffffff;
   MaxValue:=int64(-$7fffffffffffffff)-1; // int64(-$8000000000000000) => parser error under Delphi 7
   for Index:=0 to length(Values)-1 do begin
    if assigned(Values[Index]) then begin
     if MinValue>PHashBucketItem(Values[Index])^.Value then begin
      MinValue:=PHashBucketItem(Values[Index])^.Value;
     end;
     if MaxValue<PHashBucketItem(Values[Index])^.Value then begin
      MaxValue:=PHashBucketItem(Values[Index])^.Value;
     end;
    end else begin
     if MinValue>-1 then begin
      MinValue:=-1;
     end;
     if MaxValue<-1 then begin
      MaxValue:=-1;
     end;
    end;
   end;
   if (MinValue>=0) and (MaxValue<int64($100)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'ValueBits=8;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Values:array[0..'+IntToStr(length(Values)-1)+'] of byte=(');
   end else if (MinValue>=0) and (MaxValue<int64($10000)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'ValueBits=16;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Values:array[0..'+IntToStr(length(Values)-1)+'] of word=(');
   end else if (MinValue>=0) and (MaxValue<int64($100000000)) then begin
    UnitSourceList.Add('      FLRE'+HashTableName+'ValueBits=32;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Values:array[0..'+IntToStr(length(Values)-1)+'] of longword=(');
   end else begin
    UnitSourceList.Add('      FLRE'+HashTableName+'ValueBits=64;');
    UnitSourceList.Add('');
    UnitSourceList.Add('      FLRE'+HashTableName+'Values:array[0..'+IntToStr(length(Values)-1)+'] of int64=(');
   end;
   for Index:=0 to length(Values)-1 do begin
    if assigned(Values[Index]) then begin
     Assert((PHashBucketItem(Values[Index])^.Value>=0) and (PHashBucketItem(Values[Index])^.Value<=$ffffffff));
     if (Index+1)<length(Values) then begin
      UnitSourceList.Add(IntToStr(PHashBucketItem(Values[Index])^.Value)+',');
     end else begin
      UnitSourceList.Add(IntToStr(PHashBucketItem(Values[Index])^.Value));
     end;
    end else begin
     Assert(false);
     if (Index+1)<length(Values) then begin
      UnitSourceList.Add('-1,');
     end else begin
      UnitSourceList.Add('-1');
     end;
    end;
   end;
   UnitSourceList.Add(');');
   UnitSourceList.Add('');

  end else begin

   Assert(false);
   
  end;

 finally
  SetLength(Buckets,0);
  SetLength(Used,0);
  SetLength(Values,0);
  SetLength(Slots,0);
  SetLength(Seeds,0);
  SetLength(FreeList,0);
 end;
end;

procedure WriteCharClassBlocks(const Name:ansistring;const Blocks:TUnicodeCharClassBlocks);
var i,j:longint; 
begin
 UnitSourceList.Add('const FLRECountUnicode'+Name+'Blocks='+IntToStr(length(Blocks)-1)+';');
 UnitSourceList.Add('');

 for i:=0 to length(Blocks)-1 do begin
  if length(Blocks[i])>0 then begin
   UnitSourceList.Add('const FLREUnicode'+Name+'Blocks'+IntToStr(i)+':array[0..'+IntToStr(length(Blocks[i])-1)+'] of TFLREUnicodeCharRange=(');
   for j:=0 to length(Blocks[i])-1 do begin
    if (j+1)<length(Blocks[i]) then begin
     UnitSourceList.Add('('+IntToStr(Blocks[i,j].FromChar)+','+IntToStr(Blocks[i,j].ToChar)+'),');
    end else begin
     UnitSourceList.Add('('+IntToStr(Blocks[i,j].FromChar)+','+IntToStr(Blocks[i,j].ToChar)+')');
    end;
   end;
   UnitSourceList.Add(');');
   UnitSourceList.Add('');
  end;
 end;

 UnitSourceList.Add('const FLREUnicode'+Name+'BlocksData:array[0..'+IntToStr(length(Blocks)-1)+'] of pointer=(');
 for i:=0 to length(Blocks)-1 do begin
  if length(Blocks[i])>0 then begin
   if (i+1)<length(Blocks) then begin
    UnitSourceList.Add('@FLREUnicode'+Name+'Blocks'+IntToStr(i)+',');
   end else begin
    UnitSourceList.Add('@FLREUnicode'+Name+'Blocks'+IntToStr(i));
   end;
  end else begin
   if (i+1)<length(Blocks) then begin
    UnitSourceList.Add('nil,');
   end else begin
    UnitSourceList.Add('nil');
   end;
  end;
 end;
 UnitSourceList.Add(');');
 UnitSourceList.Add('');

 UnitSourceList.Add('const FLREUnicode'+Name+'BlocksCounts:array[0..'+IntToStr(length(Blocks)-1)+'] of longint=(');
 for i:=0 to length(Blocks)-1 do begin
  if (i+1)<length(Blocks) then begin
   UnitSourceList.Add(IntToStr(length(Blocks[i]))+',');
  end else begin
   UnitSourceList.Add(IntToStr(length(Blocks[i])));
  end;
 end;
 UnitSourceList.Add(');');
 UnitSourceList.Add('');
end;

var i,j,k,Size:longint;
    s:TFLRERawByteString;
    UnicodeCharClassRange:TFLREUnicodeCharClassRange;
begin
 UnicodeCategoryBlocks:=nil;
 UnicodeIgnoreCaseCategoryBlocks:=nil;
 CountUnicodeCategoryBlocks:=0;

 UnicodeScriptBlocks:=nil;
 UnicodeIgnoreCaseScriptBlocks:=nil;
 CountUnicodeScriptBlocks:=0;

 UnicodeBlockBlocks:=nil;
 UnicodeIgnoreCaseBlockBlocks:=nil;
 CountUnicodeBlockBlocks:=0;

 UnitSourceList:=TStringList.Create;
 try
  UnitSourceList.LoadFromFile('FLREUnicode.pas');
  for LineIndex:=UnitSourceList.Count-1 downto 0 do begin
   Line:=UnitSourceList.Strings[LineIndex];
   if pos('type TFLRERawByteString=',Line)=1 then begin
    UnitSourceList.Strings[LineIndex]:='uses FLRE;';
    continue;
   end;
   if pos('implementation',Line)=1 then begin
    UnitSourceList.Delete(LineIndex);
    continue;
   end;
   if pos('end.',Line)=1 then begin
    UnitSourceList.Delete(LineIndex);
    continue;
   end;
  end;

  InitializeUnicode;

  UnitSourceList.Add('const FLREucrWORDS=0;');
  UnitSourceList.Add('      FLREucrDIGITS=1;');
  UnitSourceList.Add('      FLREucrWHITESPACES=2;');
  UnitSourceList.Add('      FLREucrLAST=FLREucrWHITESPACES;');
  UnitSourceList.Add('');
  UnitSourceList.Add('type PFLREUnicodeCharRange=^TFLREUnicodeCharRange;');
  UnitSourceList.Add('     TFLREUnicodeCharRange=array[0..1] of longword;');
  UnitSourceList.Add('');                                                   
  UnitSourceList.Add('     PFLREUnicodeCharRanges=^TFLREUnicodeCharRanges;');
  UnitSourceList.Add('     TFLREUnicodeCharRanges=array[0..0] of TFLREUnicodeCharRange;');
  UnitSourceList.Add('');

  for i:=0 to length(UnicodeCharRangeClasses)-1 do begin
   UnitSourceList.Add('const FLREUnicodeCharRangeClasses'+IntToStr(i)+':array[0..'+IntToStr(length(UnicodeCharRangeClasses[i])-1)+'] of TFLREUnicodeCharRange=(');
   for j:=0 to length(UnicodeCharRangeClasses[i])-1 do begin
    if (j+1)<length(UnicodeCharRangeClasses[i]) then begin
     UnitSourceList.Add('('+IntToStr(UnicodeCharRangeClasses[i,j,0])+','+IntToStr(UnicodeCharRangeClasses[i,j,1])+'),');
    end else begin
     UnitSourceList.Add('('+IntToStr(UnicodeCharRangeClasses[i,j,0])+','+IntToStr(UnicodeCharRangeClasses[i,j,1])+')');
    end;
   end;
   UnitSourceList.Add(');');
   UnitSourceList.Add('');
  end;

  UnitSourceList.Add('const FLREUnicodeCharRangeClassesData:array[0..'+IntToStr(length(UnicodeCharRangeClasses)-1)+'] of pointer=(');
  for i:=0 to length(UnicodeCharRangeClasses)-1 do begin
   if (i+1)<length(UnicodeCharRangeClasses) then begin
    UnitSourceList.Add('@FLREUnicodeCharRangeClasses'+IntToStr(i)+',');
   end else begin
    UnitSourceList.Add('@FLREUnicodeCharRangeClasses'+IntToStr(i));
   end;
  end;
  UnitSourceList.Add(');');
  UnitSourceList.Add('');

  UnitSourceList.Add('const FLREUnicodeCharRangeClassesCounts:array[0..'+IntToStr(length(UnicodeCharRangeClasses)-1)+'] of longint=(');
  for i:=0 to length(UnicodeCharRangeClasses)-1 do begin
   if (i+1)<length(UnicodeCharRangeClasses) then begin
    UnitSourceList.Add(IntToStr(length(UnicodeCharRangeClasses[i]))+',');
   end else begin
    UnitSourceList.Add(IntToStr(length(UnicodeCharRangeClasses[i])));
   end;
  end;
  UnitSourceList.Add(');');
  UnitSourceList.Add('');

  WriteCharClassBlocks('Category',UnicodeCategoryBlocks);
  WriteCharClassBlocks('IgnoreCaseCategory',UnicodeIgnoreCaseCategoryBlocks);

  WriteCharClassBlocks('Script',UnicodeScriptBlocks);
  WriteCharClassBlocks('IgnoreCaseScript',UnicodeIgnoreCaseScriptBlocks);

  WriteCharClassBlocks('Block',UnicodeBlockBlocks);
  WriteCharClassBlocks('IgnoreCaseBlock',UnicodeIgnoreCaseBlockBlocks);

  WriteCharClassBlocks('Additional',UnicodeAdditionalBlocks);
  WriteCharClassBlocks('IgnoreCaseAdditional',UnicodeIgnoreCaseAdditionalBlocks);

  Size:=0;
  UnicodeCharClassRange:=LowerUpperCaseUnicodeCharClass.First;
  while assigned(UnicodeCharClassRange) do begin
   inc(Size);
   UnicodeCharClassRange:=UnicodeCharClassRange.Next;
  end;
  UnitSourceList.Add('const FLRELowerUpperCaseUnicodeCharClassSize='+IntToStr(Size)+';');
  UnitSourceList.Add('');

  UnitSourceList.Add('      FLRELowerUpperCaseUnicodeCharClass:array[0..'+IntToStr(Size-1)+',0..1] of longword=(');
  UnicodeCharClassRange:=LowerUpperCaseUnicodeCharClass.First;
  while assigned(UnicodeCharClassRange) do begin
   s:='('+IntToStr(UnicodeCharClassRange.Lo)+','+IntToStr(UnicodeCharClassRange.Hi)+')';
   UnicodeCharClassRange:=UnicodeCharClassRange.Next;
   if assigned(UnicodeCharClassRange) then begin
    s:=s+',';
   end;
   UnitSourceList.Add(s);
  end;
  UnitSourceList.Add(');');
  UnitSourceList.Add('');

  MakeMinimalPerfectHashTable(UnicodeClassHashMap,'UnicodeClassHashMap');
  MakeMinimalPerfectHashTable(UnicodeScriptHashMap,'UnicodeScriptHashMap');
  MakeMinimalPerfectHashTable(UnicodeBlockHashMap,'UnicodeBlockHashMap');
  MakeMinimalPerfectHashTable(UnicodeAdditionalBlockHashMap,'UnicodeAdditionalBlockHashMap');
  MakeMinimalPerfectHashTable(UnicodeClassLowerCaseHashMap,'UnicodeClassLowerCaseHashMap');
  MakeMinimalPerfectHashTable(UnicodeScriptLowerCaseHashMap,'UnicodeScriptLowerCaseHashMap');
  MakeMinimalPerfectHashTable(UnicodeBlockLowerCaseHashMap,'UnicodeBlockLowerCaseHashMap');
  MakeMinimalPerfectHashTable(UnicodeAdditionalBlockLowerCaseHashMap,'UnicodeAdditionalBlockLowerCaseHashMap');

  UnitSourceList.Add('implementation');
  UnitSourceList.Add('end.');
  UnitSourceList.SaveToFile(IncludeTrailingPathDelimiter('..')+'FLREUnicode.pas');

  for i:=low(TUnicodeCharRangeClasses) to high(TUnicodeCharRangeClasses) do begin
   SetLength(UnicodeCharRangeClasses[i],0);
  end;

  SetLength(UnicodeCategoryBlocks,0);
  SetLength(UnicodeIgnoreCaseCategoryBlocks,0);
  SetLength(UnicodeScriptBlocks,0);
  SetLength(UnicodeIgnoreCaseScriptBlocks,0);
  SetLength(UnicodeBlockBlocks,0);
  SetLength(UnicodeIgnoreCaseBlockBlocks,0);
  SetLength(UnicodeAdditionalBlocks,0);
  SetLength(UnicodeIgnoreCaseAdditionalBlocks,0);
  FreeAndNil(UnicodeClassHashMap);
  FreeAndNil(UnicodeScriptHashMap);
  FreeAndNil(UnicodeBlockHashMap);
  FreeAndNil(UnicodeAdditionalBlockHashMap);
  FreeAndNil(UnicodeClassLowerCaseHashMap);
  FreeAndNil(UnicodeScriptLowerCaseHashMap);
  FreeAndNil(UnicodeBlockLowerCaseHashMap);
  FreeAndNil(UnicodeAdditionalBlockLowerCaseHashMap);

 finally
  UnitSourceList.Free;
 end;
end.
