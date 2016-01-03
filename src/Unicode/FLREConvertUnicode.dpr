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
program FLREConvertUnicode;
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

uses SysUtils,Classes;

const MaxUnicodeChar=$10ffff;
      CountUnicodeChars=$110000;

type TFLRERawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     TFLREUnicodeDWords=array[0..MaxUnicodeChar] of longint;
     
var FLREUnicodeCategories:TFLREUnicodeDWords;
    FLREUnicodeScripts:TFLREUnicodeDWords;
    FLREUnicodeLowerCaseDeltas:TFLREUnicodeDWords;
    FLREUnicodeUpperCaseDeltas:TFLREUnicodeDWords;
    FLREUnicodeTitleCaseDeltas:TFLREUnicodeDWords;
    FLRECategories:TStringList;
    FLREScripts:TStringList;
    OutputList:TStringList;

function GetUntilSplitter(const Splitter:TFLRERawByteString;var s:TFLRERawByteString):TFLRERawByteString;
var i:longint;
begin
 i:=pos(Splitter,s);
 if i>0 then begin
  result:=trim(copy(s,1,i-1));
  Delete(s,1,(i+length(Splitter))-1);
  s:=trim(s);
 end else begin
  result:=trim(s);
  s:='';
 end;
end;

procedure ParseBlocks;
type TFLREUnicodeBlock=record
      Name:TFLRERawByteString;
      FromChar,ToChar:longword;
     end;
var List:TStringList;
    i,j,k,FromChar,ToChar,Count:longint;
    s,p:TFLRERawByteString;
    Blocks:array of TFLREUnicodeBlock;
begin
 Blocks:=nil;
 try
  Count:=0;
  OutputList.Add('type TFLREUnicodeBlock=record');
  OutputList.Add('      Name:TFLRERawByteString;');
  OutputList.Add('      FromChar,ToChar:longword;');
  OutputList.Add('     end;');
  List:=TStringList.Create;
  try
   List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'Blocks.txt');
   for i:=0 to List.Count-1 do begin
    s:=trim(List[i]);
    if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
     continue;
    end;
    j:=pos('#',s);
    if j>0 then begin
     s:=trim(copy(s,1,j-1));
    end;
    j:=pos(';',s);
    if j=0 then begin
     continue;
    end;
    p:=trim(copy(s,j+1,length(s)-j));
    s:=trim(copy(s,1,j-1));
    j:=pos('..',s);
    if j=0 then begin
     FromChar:=StrToInt('$'+trim(s));
     ToChar:=FromChar;
    end else begin
     FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
     ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    end;
    if (Count+1)>=length(Blocks) then begin
     j:=1;
     k:=Count+1;
     while j<=k do begin
      inc(j,j);
     end;
     SetLength(Blocks,j);
    end;
    Blocks[Count].Name:=p;
    Blocks[Count].FromChar:=FromChar;
    Blocks[Count].ToChar:=ToChar;
    inc(Count);
   end;
   SetLength(Blocks,Count);
  finally
   List.Free;
  end;
  OutputList.Add('const FLREUnicodeBlockCount='+IntToStr(Count)+';');
  OutputList.Add('      FLREUnicodeBlocks:array[0..'+IntToStr(Count-1)+'] of TFLREUnicodeBlock=(');
  for i:=0 to Count-1 do begin
   if (i+1)<Count then begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'),');
   end else begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'));');
   end;
  end;
  if Count=0 then begin
   OutputList.Add(');');
  end;
  OutputList.Add('');
 finally
  SetLength(Blocks,0);
 end;
end;

procedure ParseDerivedGeneralCategory;
var List:TStringList;
    i,j,ci,FromChar,ToChar,CurrentChar:longint;
    s,p:TFLRERawByteString;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'DerivedGeneralCategory.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   ci:=FLRECategories.IndexOf(p);
   if ci<0 then begin
    ci:=FLRECategories.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    FLREUnicodeCategories[CurrentChar]:=ci;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     FLREUnicodeCategories[CurrentChar]:=ci;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseScripts;
var List:TStringList;
    i,j,si,FromChar,ToChar,CurrentChar:longint;
    s,p:TFLRERawByteString;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'Scripts.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   si:=FLREScripts.IndexOf(p);
   if si<0 then begin
    si:=FLREScripts.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    FLREUnicodeScripts[CurrentChar]:=si;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     FLREUnicodeScripts[CurrentChar]:=si;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseUnicodeData;
var List:TStringList;
    i,j,ci,OtherChar,CurrentChar:longint;
    s,cs:TFLRERawByteString;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'UnicodeData.txt');
  for i:=ord('a') to ord('z') do begin
   FLREUnicodeUpperCaseDeltas[i]:=longint(ord('A')-ord('a'));
  end;
  for i:=ord('A') to ord('Z') do begin
   FLREUnicodeLowerCaseDeltas[i]:=ord('a')-ord('A');
  end;
  for i:=$ff21 to $ff3a do begin
   FLREUnicodeLowerCaseDeltas[i]:=$ff41-$ff21;
  end;
  for i:=$ff41 to $ff5a do begin
   FLREUnicodeUpperCaseDeltas[i]:=longint($ff21-$ff41);
  end;
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   CurrentChar:=StrToInt('$'+GetUntilSplitter(';',s)); // Code
   GetUntilSplitter(';',s); // Name
   begin
    cs:=GetUntilSplitter(';',s); // Class
    ci:=FLRECategories.IndexOf(cs);
    if ci<0 then begin
     ci:=FLRECategories.Add(cs);
    end;
    if FLREUnicodeCategories[CurrentChar]<>ci then begin
     writeln(ErrOutput,CurrentChar,' has multiple categories?');
     FLREUnicodeCategories[CurrentChar]:=ci;
    end;
   end;
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // UpperChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     FLREUnicodeUpperCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // LowerChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     FLREUnicodeLowerCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // TitleChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     FLREUnicodeTitleCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure PackTable(const Table:array of longint;Level:integer;const Name:TFLRERawByteString);
type TBlock=array of longint;
     TBlocks=array of TBlock;
     TIndices=array of longint;
var BestBlockSize,BlockSize,CountBlocks,CountIndices,Index,BlockPosition,Bytes,BestBytes,Bits,BestBits,EntryBytes,IndicesEntryBytes,BestIndicesEntryBytes,i,j,k:longint;
    Block:TBlock;
    Blocks:TBlocks;
    Indices:TIndices;
    BestBlocks:TBlocks;
    BestIndices:TIndices;
    OK:boolean;
    s:TFLRERawByteString;
begin
 if Level<2 then begin
  Block:=nil;
  Blocks:=nil;
  Indices:=nil;
  BestBlocks:=nil;
  BestIndices:=nil;
  try
   BestBlockSize:=length(Table)*2;
   BestBits:=24;
   BlockSize:=1;
   Bits:=0;
   BestBytes:=-1;
   i:=0;
   OK:=true;
   for Index:=0 to length(Table)-1 do begin
    j:=Table[Index];
    if j<0 then begin
     OK:=false;
    end;
    j:=abs(j);
    if i<j then begin
     i:=j;
    end;
   end;
   if OK then begin
    if i<256 then begin
     EntryBytes:=1;
     s:='byte';
    end else if i<65536 then begin
     EntryBytes:=2;
     s:='word';
    end else begin
     EntryBytes:=4;
     s:='longword';
    end;
   end else begin
    if i<128 then begin
     EntryBytes:=1;
     s:='shortint';
    end else if i<32768 then begin
     EntryBytes:=2;
     s:='smallint';
    end else begin
     EntryBytes:=4;
     s:='longint';
    end;
   end;
   BestIndicesEntryBytes:=4;
   while BlockSize<length(Table) do begin
    SetLength(Block,BlockSize);
    SetLength(Blocks,(length(Table) div BlockSize)+1);
    FillChar(Block[0],BlockSize,#$ff);
    BlockPosition:=0;
    CountBlocks:=0;
    CountIndices:=0;
    for Index:=0 to length(Table)-1 do begin
     Block[BlockPosition]:=Table[Index];
     inc(BlockPosition);
     if BlockPosition=BlockSize then begin
      k:=-1;
      for i:=0 to CountBlocks-1 do begin
       OK:=true;
       for j:=0 to BlockSize-1 do begin
        if Blocks[i,j]<>Block[j] then begin
         OK:=false;
         break;
        end;
       end;
       if OK then begin
        k:=i;
        break;
       end;
      end;
      if k<0 then begin
       k:=CountBlocks;
       Blocks[CountBlocks]:=copy(Block);
       inc(CountBlocks);
      end;
      if (CountIndices+1)>=length(Indices) then begin
       i:=1;
       j:=CountIndices+1;
       while i<=j do begin
        inc(i,i);
       end;
       SetLength(Indices,i);
      end;
      Indices[CountIndices]:=k;
      inc(CountIndices);
      BlockPosition:=0;
     end;
    end;
    if CountBlocks<256 then begin
     IndicesEntryBytes:=1;
    end else if CountBlocks<65536 then begin
     IndicesEntryBytes:=2;
    end else begin
     IndicesEntryBytes:=4;
    end;
    Bytes:=((CountBlocks*BlockSize)*EntryBytes)+(CountIndices*IndicesEntryBytes);
    if (BestBytes<0) or (Bytes<=BestBytes) then begin
     BestBytes:=Bytes;
     BestBlockSize:=BlockSize;
     BestBits:=Bits;
     BestIndicesEntryBytes:=EntryBytes;
     BestBlocks:=copy(Blocks,0,CountBlocks);
     BestIndices:=copy(Indices,0,CountIndices);
    end;
    SetLength(Blocks,0);
    SetLength(Indices,0);
    inc(BlockSize,BlockSize);
    inc(Bits);
   end;
   OutputList.Add('// '+Name+': '+IntToStr(BestBytes)+' bytes, '+IntToStr(length(BestBlocks))+' blocks with '+IntToStr(BestBlockSize)+' items per '+IntToStr(EntryBytes)+' bytes and '+IntToStr(length(BestIndices))+' indices per '+IntToStr(BestIndicesEntryBytes)+' bytes');
   OutputList.Add('const '+Name+'BlockBits='+IntToStr(BestBits)+';');
   OutputList.Add('      '+Name+'BlockMask='+IntToStr((1 shl BestBits)-1)+';');
   OutputList.Add('      '+Name+'BlockSize='+IntToStr(BestBlockSize)+';');
   OutputList.Add('      '+Name+'BlockCount='+IntToStr(length(BestBlocks))+';');
   OutputList.Add('      '+Name+'BlockData:array[0..'+IntToStr(length(BestBlocks)-1)+',0..'+IntToStr(BestBlockSize-1)+'] of '+s+'=(');
   s:='';
   for i:=0 to length(BestBlocks)-1 do begin
    s:=s+'(';
    for j:=0 to BestBlockSize-1 do begin
     s:=s+IntToStr(BestBlocks[i,j]);
     if (j+1)<BestBlockSize then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    s:=s+')';
    if (i+1)<length(BestBlocks) then begin
     s:=s+',';
    end;
    OutputList.Add(s);
    s:='';
   end;
   if length(s)>0 then begin
    OutputList.Add(s);
    s:='';
   end;
   OutputList.Add(');');
   if Level=1 then begin
    case BestIndicesEntryBytes of
     1:begin
      s:='byte';
     end;
     2:begin
      s:='word';
     end;
     else begin
      s:='longword';
     end;
    end;
    OutputList.Add('      '+Name+'IndexCount='+IntToStr(length(BestBlocks))+';');
    OutputList.Add('      '+Name+'IndexData:array[0..'+IntToStr(length(BestIndices)-1)+'] of '+s+'=(');
    s:='';
    for i:=0 to length(BestIndices)-1 do begin
     s:=s+IntToStr(BestIndices[i]);
     if (i+1)<length(BestIndices) then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    if length(s)>0 then begin
     OutputList.Add(s);
     s:='';
    end;
    OutputList.Add(');');
    OutputList.Add('');
   end else begin
    OutputList.Add('');
    PackTable(BestIndices,Level+1,Name+'Index');
   end;
  finally
   SetLength(Block,0);
   SetLength(Blocks,0);
   SetLength(Indices,0);
   SetLength(BestBlocks,0);
   SetLength(BestIndices,0);
  end;
 end;
end;

var i:longint;
begin
 FillChar(FLREUnicodeCategories,sizeof(TFLREUnicodeDWords),#0);
 FillChar(FLREUnicodeScripts,sizeof(TFLREUnicodeDWords),#$0);
 FillChar(FLREUnicodeUpperCaseDeltas,sizeof(TFLREUnicodeDWords),#$0);
 FillChar(FLREUnicodeLowerCaseDeltas,sizeof(TFLREUnicodeDWords),#$0);
 FillChar(FLREUnicodeTitleCaseDeltas,sizeof(TFLREUnicodeDWords),#$0);
 OutputList:=TStringList.Create;
 try
  FLRECategories:=TStringList.Create;
  FLRECategories.Add('Cn');
  try
   FLREScripts:=TStringList.Create;
   FLREScripts.Add('Unknown');
   FLREScripts.Add('Common');
   try
    ParseDerivedGeneralCategory;
    ParseScripts;
    ParseUnicodeData;
    OutputList.Add('unit FLREUnicode;');
    OutputList.Add('{$ifdef fpc}');
    OutputList.Add(' {$mode delphi}');
    OutputList.Add('{$endif}');
    OutputList.Add('interface');
    OutputList.Add('');
//  OutputList.Add('uses FLRE;');
    OutputList.Add('type TFLRERawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};');
    OutputList.Add('');
    ParseBlocks;
    begin
     OutputList.Add('const FLREUnicodeCategoryIDs:array[0..'+IntToStr(FLRECategories.Count-1)+'] of TFLRERawByteString=(');
     for i:=0 to FLRECategories.Count-1 do begin
      if (i+1)<FLRECategories.Count then begin
       OutputList.Add(''''+FLRECategories[i]+''',');
      end else begin
       OutputList.Add(''''+FLRECategories[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to FLRECategories.Count-1 do begin
      OutputList.Add('      FLREUnicodeCategory'+FLRECategories[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('      FLREUnicodeCategoryCount='+IntToStr(FLRECategories.Count)+';');
     OutputList.Add('      FLRE_CT_UNASSIGNED=FLREUnicodeCategoryCn;');
     OutputList.Add('      FLRE_CT_UPPERCASE_LETTER=FLREUnicodeCategoryLu;');
     OutputList.Add('      FLRE_CT_LOWERCASE_LETTER=FLREUnicodeCategoryLl;');
     OutputList.Add('      FLRE_CT_TITLECASE_LETTER=FLREUnicodeCategoryLt;');
     OutputList.Add('      FLRE_CT_MODIFIER_LETTER=FLREUnicodeCategoryLm;');
     OutputList.Add('      FLRE_CT_OTHER_LETTER=FLREUnicodeCategoryLo;');
     OutputList.Add('      FLRE_CT_NON_SPACING_MARK=FLREUnicodeCategoryMn;');
     OutputList.Add('      FLRE_CT_ENCLOSING_MARK=FLREUnicodeCategoryMe;');
     OutputList.Add('      FLRE_CT_COMBINING_SPACING_MARK=FLREUnicodeCategoryMc;');
     OutputList.Add('      FLRE_CT_DECIMAL_DIGIT_NUMBER=FLREUnicodeCategoryNd;');
     OutputList.Add('      FLRE_CT_LETTER_NUMBER=FLREUnicodeCategoryNl;');
     OutputList.Add('      FLRE_CT_OTHER_NUMBER=FLREUnicodeCategoryNo;');
     OutputList.Add('      FLRE_CT_SPACE_SEPARATOR=FLREUnicodeCategoryZs;');
     OutputList.Add('      FLRE_CT_LINE_SEPARATOR=FLREUnicodeCategoryZl;');
     OutputList.Add('      FLRE_CT_PARAGRAPH_SEPARATOR=FLREUnicodeCategoryZp;');
     OutputList.Add('      FLRE_CT_CONTROL=FLREUnicodeCategoryCc;');
     OutputList.Add('      FLRE_CT_FORMAT=FLREUnicodeCategoryCf;');
     OutputList.Add('      FLRE_CT_PRIVATE_USE=FLREUnicodeCategoryCo;');
     OutputList.Add('      FLRE_CT_SURROGATE=FLREUnicodeCategoryCs;');
     OutputList.Add('      FLRE_CT_DASH_PUNCTUATION=FLREUnicodeCategoryPd;');
     OutputList.Add('      FLRE_CT_START_PUNCTUATION=FLREUnicodeCategoryPs;');
     OutputList.Add('      FLRE_CT_END_PUNCTUATION=FLREUnicodeCategoryPe;');
     OutputList.Add('      FLRE_CT_INITIAL_PUNCTUATION=FLREUnicodeCategoryPi;');
     OutputList.Add('      FLRE_CT_FINAL_PUNCTUATION=FLREUnicodeCategoryPf;');
     OutputList.Add('      FLRE_CT_CONNECTOR_PUNCTUATION=FLREUnicodeCategoryPc;');
     OutputList.Add('      FLRE_CT_OTHER_PUNCTUATION=FLREUnicodeCategoryPo;');
     OutputList.Add('      FLRE_CT_MATH_SYMBOL=FLREUnicodeCategorySm;');
     OutputList.Add('      FLRE_CT_CURRENCY_SYMBOL=FLREUnicodeCategorySc;');
     OutputList.Add('      FLRE_CT_MODIFIER_SYMBOL=FLREUnicodeCategorySk;');
     OutputList.Add('      FLRE_CT_OTHER_SYMBOL=FLREUnicodeCategorySo;');
     OutputList.Add('');
    end;
    begin
     OutputList.Add('const FLREUnicodeScriptIDs:array[0..'+IntToStr(FLREScripts.Count-1)+'] of TFLRERawByteString=(');
     for i:=0 to FLREScripts.Count-1 do begin
      if (i+1)<FLREScripts.Count then begin
       OutputList.Add(''''+FLREScripts[i]+''',');
      end else begin
       OutputList.Add(''''+FLREScripts[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to FLREScripts.Count-1 do begin
      OutputList.Add('     FLREUnicodeScript'+FLREScripts[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('     FLREUnicodeScriptCount='+IntToStr(FLREScripts.Count)+';');
     OutputList.Add('');
    end;
    PackTable(FLREUnicodeCategories,0,'FLREUnicodeCategoryArray');
    OutputList.Add('');
    PackTable(FLREUnicodeScripts,0,'FLREUnicodeScriptArray');
    OutputList.Add('');
    PackTable(FLREUnicodeUpperCaseDeltas,0,'FLREUnicodeUpperCaseDeltaArray');
    PackTable(FLREUnicodeLowerCaseDeltas,0,'FLREUnicodeLowerCaseDeltaArray');
    PackTable(FLREUnicodeTitleCaseDeltas,0,'FLREUnicodeTitleCaseDeltaArray');
    OutputList.Add('implementation');
    OutputList.Add('end.');
    OutputList.SaveToFile('FLREUnicode.pas');
//  OutputList.SaveToFile(IncludeTrailingPathDelimiter('..')+'FLREUnicode.pas');
   finally
    FLREScripts.Free;
   end;
  finally
   FLRECategories.Free;
  end;
 finally
  OutputList.Free;
 end;
end.
