(*******************************************************************************
                                 L I C E N S E
********************************************************************************

FLRE - Fast Light Regular Expressions - A fast light regular expression library
Copyright (C) 2015, Benjamin 'BeRo' Rosseaux

The source code of the FLRE engine library and helper tools are
distributed under the Library GNU Lesser General Public License Version 2.1 
(see the file copying.txt) with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a module
which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but you 
are not obligated to do so. If you do not wish to do so, delete this exception
statement from your version.

If you didn't receive a copy of the license, see <http://www.gnu.org/licenses/>
or contact:
      Free Software Foundation
      675 Mass Ave
      Cambridge, MA  02139
      USA

*******************************************************************************)
unit FLRE;
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
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
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

interface

uses SysUtils,Classes,Math,FLREUnicode;

const FLREVersionStr='1.00.2015.08.24.1633';

      MaxPrefixCharClasses=32;

type EFLRE=class(Exception);

     PFLREQWord=^TFLREQWord;
     PFLREPtrUInt=^TFLREPtrUInt;
     PFLREPtrInt=^TFLREPtrInt;

{$ifdef fpc}
     TFLREQWord=qword;

     TFLREPtrUInt=PtrUInt;
     TFLREPtrInt=PtrInt;
{$else}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     TFLREQWord=uint64;

     TFLREPtrUInt=NativeUInt;
     TFLREPtrInt=NativeInt;
{$else}
     TFLREQWord=int64;

{$ifdef cpu64}
     TFLREPtrUInt=TFLREQWord;
     TFLREPtrInt=int64;
{$else}
     TFLREPtrUInt=longword;
     TFLREPtrInt=longint;
{$endif}
{$ifend}
{$endif}

     PFLREFlag=^TFLREFlag;
     TFLREFlag=(rfCASEINSENSITIVE,
                rfSINGLELINE,
                rfMULTILINE,
                rfFREESPACING,
                rfLONGEST,
                rfLAZY,
                rfGREEDY,
                rfUTF8);

     TFLREFlags=set of TFLREFlag;

     PFLRECharClass=^TFLRECharClass;
     TFLRECharClass=set of ansichar;

     TPFLRECharClasses=array of PFLRECharClass;

     PPFLRENode=^PFLRENode;
     PFLRENode=^TFLRENode;
     TFLRENode=record
      NodeType:longint;
      CharClass:TFLRECharClass;
      Value:longint;
      MinCount:longint;
      MaxCount:longint;
      Left:PFLRENode;
      Right:PFLRENode;
      Extra:PFLRENode;
      Index:longint;
     end;

     PFLREInstruction=^TFLREInstruction;
     TFLREInstruction=record
      IndexAndOpcode:TFLREPtrInt;
      Value:TFLREPtrInt;
      Generation:TFLREPtrUInt;
      Next:PFLREInstruction;
      OtherNext:PFLREInstruction;
     end;

     TFLREInstructions=array of TFLREInstruction;

     TPFLREInstructions=array of PFLREInstruction;

     PPFLREInstructionsStatic=^TPFLREInstructionsStatic;
     TPFLREInstructionsStatic=array[0..65535] of PFLREInstruction;

     PFLRESubMatchesItem=^TFLRESubMatchesItem;
     TFLRESubMatchesItem=longint;

     TFLRESubMatchesItems=array of TFLRESubMatchesItem;

     PFLRESubMatches=^TFLRESubMatches;
     TFLRESubMatches=record
      Next:PFLRESubMatches;
      ReferenceCounter:longint;
      Count:longint;
      BitState:longword;
      SubMatches:TFLRESubMatchesItems;
     end;

     PFLREThread=^TFLREThread;
     TFLREThread=record
      Instruction:PFLREInstruction;
      SubMatches:PFLRESubMatches;
     end;

     TFLREThreads=array of TFLREThread;

     PFLREThreadList=^TFLREThreadList;
     TFLREThreadList=record
      Threads:TFLREThreads;
      Count:longint;
     end;

     TFLREThreadLists=array[0..1] of TFLREThreadList;

     PFLREByteMap=^TFLREByteMap;
     TFLREByteMap=array[byte] of byte;

     PFLREOnePassNFAStateCharClassAction=^TFLREOnePassNFAStateCharClassAction;
     TFLREOnePassNFAStateCharClassAction=record // 32-bit: 16 bytes, 64-bit: 32 bytes
      AllNext,Next:PFLREOnePassNFAStateCharClassAction;
      CharClass:TFLRECharClass;
      Condition:longword;
     end;

     PFLREOnePassNFAState=^TFLREOnePassNFAState;
     TFLREOnePassNFAState=record
      MatchCondition,NoAction:longword;
      CharClassAction:PFLREOnePassNFAStateCharClassAction;
      Action:array[0..0] of longword;
     end;

     TFLREOnePassNFACaptures=array of longint;

     PFLREBitStateNFAJob=^TFLREBitStateNFAJob;
     TFLREBitStateNFAJob=record
      Instruction:PFLREInstruction;
      Position:longint;
      Argument:longint;
{$ifndef cpu64}
      Dummy:longint;
{$endif}
     end;

     TFLREBitStateNFAJobs=array of TFLREBitStateNFAJob;

     TFLREBitStateNFAVisited=array[0..(32768 div SizeOf(longword))-1] of longword;

     TFLREBitStateNFACaptures=array of longint;

     PFLREDFAState=^TFLREDFAState;

     TPFLREDFAStates=array of PFLREDFAState;

     PPFLREDFANextStatesByteBlock=^TPFLREDFANextStatesByteBlock;
     TPFLREDFANextStatesByteBlock=array[byte] of PFLREDFAState;

     TFLREDFAState=record
      Flags:longword;
      Instructions:TPFLREInstructions;
      CountInstructions:longint;
      NextStates:TPFLREDFANextStatesByteBlock;
     end;

     PFLREDFAStatePool=^TFLREDFAStatePool;
     TFLREDFAStatePool=record
      Next:PFLREDFAStatePool;
      States:pointer;
      EndState:pointer;
      NextState:pointer;
     end;

     PFLREDFAStateHashMapEntity=^TFLREDFAStateHashMapEntity;
     TFLREDFAStateHashMapEntity=record
      Key:PFLREDFAState;
      Value:int64;
     end;

     TFLREDFAStateHashMapEntities=array of TFLREDFAStateHashMapEntity;

     TFLREDFAStateHashMapEntityIndices=array of longint;

     TFLREDFAStateHashMap=class
      private
       RealSize:longint;
       LogSize:longint;
       Size:longint;
       Entities:TFLREDFAStateHashMapEntities;
       EntityToCellIndex:TFLREDFAStateHashMapEntityIndices;
       CellToEntityIndex:TFLREDFAStateHashMapEntityIndices;
       function FindCell(const Key:PFLREDFAState):longword;
       procedure Resize;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Get(const Key:PFLREDFAState):PFLREDFAState;
       function Add(const Key:PFLREDFAState):PFLREDFAStateHashMapEntity;
       function Delete(const Key:PFLREDFAState):boolean;
       property Keys[const Key:PFLREDFAState]:PFLREDFAState read Get; default;
     end;

     TFLRECharPatternBitMasks=array[ansichar] of longword;

     TFLREBoyerMooreNext=array of longint;

     PFLRECapture=^TFLRECapture;
     TFLRECapture=record
      Start:longint;
      Length:longint;
     end;

     TFLRECaptures=array of TFLRECapture;

     TFLREMultiCaptures=array of TFLRECaptures;

     TFLREPrefixCharClasses=array[0..MaxPrefixCharClasses-1] of TFLRECharClass;
    
     TFLREStringIntegerPairHashMapData=longint;

     PFLREStringIntegerPairHashMapEntity=^TFLREStringIntegerPairHashMapEntity;
     TFLREStringIntegerPairHashMapEntity=record
      Key:ansistring;
      Value:TFLREStringIntegerPairHashMapData;
     end;

     TFLREStringIntegerPairHashMapEntities=array of TFLREStringIntegerPairHashMapEntity;

     TFLREStringIntegerPairHashMapEntityIndices=array of longint;

     TFLREStringIntegerPairHashMap=class
      private
       function FindCell(const Key:ansistring):longword;
       procedure Resize;
      protected
       function GetValue(const Key:ansistring):TFLREStringIntegerPairHashMapData;
       procedure SetValue(const Key:ansistring;const Value:TFLREStringIntegerPairHashMapData);
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
       function Add(const Key:ansistring;Value:TFLREStringIntegerPairHashMapData):PFLREStringIntegerPairHashMapEntity;
       function Get(const Key:ansistring;CreateIfNotExist:boolean=false):PFLREStringIntegerPairHashMapEntity;
       function Delete(const Key:ansistring):boolean;
       property Values[const Key:ansistring]:TFLREStringIntegerPairHashMapData read GetValue write SetValue; default;
     end;

     TFLRE=class
      private

       RegularExpression:ansistring;

       Flags:TFLREFlags;

       AnchoredRootNode:PFLRENode;

       UnanchoredRootNode:PFLRENode;

       Nodes:TList;

       Generation:longword;

       CountParens:longint;

       CountSubMatches:longint;

       ForwardInstructions:TFLREInstructions;
       CountForwardInstructions:longint;

       BackwardInstructions:TFLREInstructions;
       CountBackwardInstructions:longint;

       AnchoredStartInstruction:PFLREInstruction;
       UnanchoredStartInstruction:PFLREInstruction;
       ReversedStartInstruction:PFLREInstruction;

       CharClasses:TPFLRECharClasses;
       CountCharClasses:longint;

       ThreadLists:TFLREThreadLists;

       FreeSubMatches:PFLRESubMatches;
       AllSubMatches:TList;

       FixedString:ansistring;
       FixedStringIsWholeRegExp:longbool;
       FixedStringLength:longint;
       FixedStringPatternBitMasks:TFLRECharPatternBitMasks;
       FixedStringBoyerMooreSkip:TFLRECharPatternBitMasks;
       FixedStringBoyerMooreNext:TFLREBoyerMooreNext;

       PrefixCharClasses:TFLREPrefixCharClasses;
       CountPrefixCharClasses:longint;
       CountObviousPrefixCharClasses:longint;
       PrefixPatternBitMasks:TFLRECharPatternBitMasks;

       Input:pansichar;
       InputLength:longint;

       ByteMap:TFLREByteMap;
       UnByteMap:TFLREByteMap;
       ByteCharSetMap:TFLRECharClass;
       ByteMapCount:longint;

       OnePassNFANodes:PFLREOnePassNFAState;
       OnePassNFANodesCount:longint;
       OnePassNFAStart:PFLREOnePassNFAState;
       OnePassNFAStateSize:longint;
       OnePassNFAWorkCaptures:TFLREOnePassNFACaptures;
       OnePassNFAMatchCaptures:TFLREOnePassNFACaptures;
       OnePassNFACharClassActions:PFLREOnePassNFAStateCharClassAction;
       OnePassNFAReady:longbool;

       BitStateNFAVisited:TFLREBitStateNFAVisited;
       BitStateNFACountVisited:longint;
       BitStateNFAJobs:TFLREBitStateNFAJobs;
       BitStateNFACountJobs:longint;
       BitStateNFAMaxJob:longint;
       BitStateNFAWorkCaptures:TFLREBitStateNFACaptures;
       BitStateNFAMatchCaptures:TFLREBitStateNFACaptures;
       BitStateNFAReady:longbool;

       DFAStackInstructions:TPFLREInstructions;
       DFAStateCache:TFLREDFAStateHashMap;
       DFAAnchoredStartState:PFLREDFAState;
       DFAUnanchoredStartState:PFLREDFAState;
       DFAReversedStartState:PFLREDFAState;
       DFADeadState:PFLREDFAState;
       DFATemporaryState:TFLREDFAState;
       DFANewState:TFLREDFAState;
       DFACountStatesCached:longint;
       DFANextStatesSize:TFLREPtrInt;
       DFAStateSize:TFLREPtrInt;
       DFAStatePoolUsed:PFLREDFAStatePool;
       DFAStatePoolFree:PFLREDFAStatePool;
       DFAStatePoolSize:TFLREPtrUInt;
       DFAStatePoolSizePowerOfTwo:TFLREPtrUInt;
       DFANeedVerification:longbool;

       BeginningJump:longbool;
       BeginningSplit:longbool;
       BeginningWildCard:longbool;
       BeginningAnchor:longbool;
       BeginningWildcardLoop:longbool;

       DoUnanchoredStart:longbool;

       NamedGroupStringList:TStringList;
       NamedGroupStringIntegerPairHashMap:TFLREStringIntegerPairHashMap;

       function NewNode(const NodeType:longint;const Left,Right,Extra:PFLRENode;const Value:longint):PFLRENode;
       procedure FreeNode(var Node:PFLRENode);
       function AreNodesEqual(NodeA,NodeB:PFLRENode):boolean;
       function AreNodesEqualSafe(NodeA,NodeB:PFLRENode):boolean;
       function Concat(NodeLeft,NodeRight:PFLRENode):PFLRENode;
       function NewAlt(NodeLeft,NodeRight:PFLRENode):PFLRENode;
       function NewPlus(Node:PFLRENode;Kind:longint):PFLRENode;
       function NewStar(Node:PFLRENode;Kind:longint):PFLRENode;
       function NewQuest(Node:PFLRENode;Kind:longint):PFLRENode;

       function IsStarNullable(Node:PFLRENode):boolean;
       function StarDenull(Node:PFLRENode):PFLRENode;
       function OptimizeNode(NodeEx:PPFLRENode):boolean;
       procedure Parse;
       procedure Compile;
       procedure CompilePrefix;
       procedure CompileFixedStringSearch;
       procedure CompilePrefixCharClasses;
       procedure CompileByteMapForOnePassNFAAndDFA;
       procedure CompileOnePassNFA;
       function NewSubMatches(const Count:longint;const BitState:longword):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
       procedure DecRef(const SubMatches:PFLRESubMatches); {$ifdef caninline}inline;{$endif}
       function IncRef(const SubMatches:PFLRESubMatches):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
       function Update(const SubMatches:PFLRESubMatches;const Index,Position:longint):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
       function NewThread(const Instruction:PFLREInstruction;const SubMatches:PFLRESubMatches):TFLREThread; {$ifdef caninline}inline;{$endif}
       function IsWordChar(const Position:longint):boolean; {$ifdef caninline}inline;{$endif}
       procedure AddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction;SubMatches:PFLRESubMatches;const Position:longint);
       function DFACacheState(const State:PFLREDFAState):PFLREDFAState; {$ifdef caninline}inline;{$endif}
       procedure DFAAddInstructionThread(const State:PFLREDFAState;Instruction:PFLREInstruction);
       function DFAProcessNextState(State:PFLREDFAState;const CurrentChar:ansichar;const Reversed:boolean):PFLREDFAState;
       procedure DFADestroyStatePool(StatePool:PFLREDFAStatePool);
       procedure DFAFreeUsedStatePool;
       function DFAAllocateNewStatePool:PFLREDFAStatePool;
       function DFAGetState:PFLREDFAState;
       function DFATakeOverState(TakeOverFrom:PFLREDFAState):PFLREDFAState;
       procedure DFAFreeState(State:PFLREDFAState);
       procedure DFAReset;
       function SearchMatchParallelNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):boolean;
       function SearchMatchOnePassNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint):boolean;
       function SearchMatchBitStateNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):longint;
       function SearchMatchDFA(const StartPosition,UntilExcludingPosition:longint;out MatchEnd:longint;const UnanchoredStart:boolean):longint;
       function SearchMatchReversedDFA(const StartPosition,UntilIncludingPosition:longint;out MatchBegin:longint):longint;
       function SearchMatch(var Captures:TFLRECaptures;StartPosition,UntilExcludingPosition:longint;UnanchoredStart:boolean):boolean;
      public
       constructor Create(const ARegularExpression:ansistring;const AFlags:TFLREFlags=[]);
       destructor Destroy; override;
       function PtrMatch(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
       function PtrMatchNext(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
       function PtrMatchAll(const AInput:pointer;const AInputLength:longint;var Captures:TFLREMultiCaptures;const StartPosition:longint=0;Limit:longint=-1):boolean;
       function PtrReplaceAll(const AInput:pointer;const AInputLength:longint;const AReplacement:pointer;const AReplacementLength:longint;const StartPosition:longint=0;Limit:longint=-1):ansistring;
       function Match(const AInput:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
       function MatchNext(const AInput:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
       function MatchAll(const AInput:ansistring;var Captures:TFLREMultiCaptures;const StartPosition:longint=1;Limit:longint=-1):boolean;
       function ReplaceAll(const AInput,AReplacement:ansistring;const StartPosition:longint=1;Limit:longint=-1):ansistring;
       property NamedGroups:TStringList read NamedGroupStringList;
       property NamedGroupIndices:TFLREStringIntegerPairHashMap read NamedGroupStringIntegerPairHashMap;
     end;

implementation

const MaxDFAStates=16384;

      // State flags
      sfEmptyBeginLine=1 shl 0;
      sfEmptyEndLine=1 shl 1;
      sfEmptyBeginText=1 shl 2;
      sfEmptyEndText=1 shl 3;
      sfEmptyWordBoundary=1 shl 4;
      sfEmptyNonWordBoundary=1 shl 5;
      sfEmptyAllFlags=(1 shl 6)-1;
      sfIndexShift=16;
      sfEmptyShift=6;
      sfRealCapShift=sfEmptyShift+1;
      sfRealMaxCap=((sfIndexShift-sfRealCapShift) shr 1)*2;
      sfCapShift=sfRealCapShift;
      sfMaxCap=sfRealMaxCap;
      sfMatchWins=1 shl sfEmptyShift;
      sfCapMask=((1 shl sfRealMaxCap)-1) shl sfRealCapShift;
      sfImpossible=sfEmptyWordBoundary or sfEmptyNonWordBoundary;
      sfDFAMatchWins=$1000;
      sfDFALastWord=$2000;
      sfDFADead=$4000;
      sfDFAStart=$8000;
      sfDFANeedShift=16;
      sfDFAStartShifted=$80000000;

      // Node types
      ntALT=0;
      ntCAT=1;
      ntCHAR=2;
      ntANY=3;
      ntPAREN=4;
      ntQUEST=5;
      ntSTAR=6;
      ntPLUS=7;
      ntEXACT=8;
      ntBOL=9;
      ntEOL=10;
      ntBOT=11;
      ntEOT=12;
      ntBRK=13;
      ntNBRK=14;

      // Opcodes
      opSINGLECHAR=0;
      opCHAR=1;
      opANY=2;
      opMATCH=3;
      opJMP=4;
      opSPLIT=5;
      opSAVE=6;
      opBOL=7;
      opEOL=8;
      opBOT=10;
      opEOT=11;
      opBRK=12;
      opNBRK=13;

      // Qualifier kind
      qkGREEDY=0;
      qkLAZY=1;

      // DFA result return values
      DFAError=-1;
      DFAFail=0;
      DFAMatch=1;

      // Bitstate NFA result return values
      BitStateNFAError=-1;
      BitStateNFAFail=0;
      BitStateNFAMatch=1;

      // For hash map
      CELL_EMPTY=-1;
      CELL_DELETED=-2;

      // For hash map
      ENT_EMPTY=-1;
      ENT_DELETED=-2;

      ucrWORDS=0;
      ucrDIGITS=1;
      ucrWHITESPACES=2;
      ucrLAST=ucrWHITESPACES;

      suDONOTKNOW=-1;
      suNOUTF8=0;
      suPOSSIBLEUTF8=1;
      suISUTF8=2;

      ucACCEPT=0;
      ucERROR=16;

      FLREInitialized:longbool=false;

{$ifndef fpc}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
type qword=uint64;

     ptruint=NativeUInt;
     ptrint=NativeInt;
{$else}
type qword=int64;

{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$ifend}
{$endif}

type PUnicodeCharRange=^TUnicodeCharRange;
     TUnicodeCharRange=array[0..1] of longword;

     PUnicodeCharRanges=^TUnicodeCharRanges;
     TUnicodeCharRanges=array of TUnicodeCharRange;

     PUnicodeCharRangeClasses=^TUnicodeCharRangeClasses;
     TUnicodeCharRangeClasses=array[0..ucrLAST] of TUnicodeCharRanges;

     TUTF8Chars=array[ansichar] of byte;

     TUTF8Bytes=array[byte] of byte;

{$ifdef PLCEStrictUTF8}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$else}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$endif}

var UTF8DFACharClasses:TUTF8Chars;
    UTF8DFATransitions:TUTF8Bytes;

    UnicodeCharRangeClasses:TUnicodeCharRangeClasses;

    UnicodeClassHashMap:TFLREStringIntegerPairHashMap;
    UnicodeScriptHashMap:TFLREStringIntegerPairHashMap;
    UnicodeBlockHashMap:TFLREStringIntegerPairHashMap;
                           
function RoundUpToPowerOfTwo(x:ptruint):ptruint; {$ifdef caninline}inline;{$endif}
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$ifdef cpu64}
 x:=x or (x shr 32);
{$endif}
 result:=x+1;
end;

procedure GetMemAligned(var p;Size:longint;Align:longint=16);
var Original,Aligned:pointer;
    Mask:ptruint;
begin
 if (Align and (Align-1))<>0 then begin
  Align:=RoundUpToPowerOfTwo(Align);
 end;
 Mask:=Align-1;
 inc(Size,((Align shl 1)+sizeof(pointer)));
 GetMem(Original,Size);
 FillChar(Original^,Size,#0);
 Aligned:=pointer(ptruint(ptruint(Original)+sizeof(pointer)));
 if (Align>1) and ((ptruint(Aligned) and Mask)<>0) then begin
  inc(ptruint(Aligned),ptruint(ptruint(Align)-(ptruint(Aligned) and Mask)));
 end;
 pointer(pointer(ptruint(ptruint(Aligned)-sizeof(pointer)))^):=Original;
 pointer(pointer(@p)^):=Aligned;
end;

procedure FreeMemAligned(const p);
var pp:pointer;
begin
 pp:=pointer(pointer(@p)^);
 if assigned(pp) then begin
  pp:=pointer(pointer(ptruint(ptruint(pp)-sizeof(pointer)))^);
  FreeMem(pp);
 end;
end;

{$ifdef cpuamd64}
function InterlockedCompareExchange128Ex(Target,NewValue,Comperand:pointer):boolean; assembler; register;
asm
 push rbx
{$ifdef win64}
 push rdi
 push rsi
 mov rdi,rcx
 mov rsi,rdx
 mov rdx,qword ptr [r8+4]
 mov rax,qword ptr [r8+0]
{$else}
 mov rax,qword ptr [rdx+0]
 mov rdx,qword ptr [rdx+4]
{$endif}
 mov rcx,qword ptr [rsi+4]
 mov rbx,qword ptr [rsi+0]
 lock cmpxchg16b [rdi]
 setz al
{$ifdef win64}
 pop rsi
 pop rdi
{$endif}
 pop rbx
end;
{$endif}

{$ifdef cpu386}
{$ifndef ver130}
function InterlockedCompareExchange64Ex(Target,NewValue,Comperand:pointer):boolean; assembler; register;
asm
 push ebx
 push edi
 push esi
 mov edi,eax
 mov esi,edx
 mov edx,dword ptr [ecx+4]
 mov eax,dword ptr [ecx+0]
 mov ecx,dword ptr [esi+4]
 mov ebx,dword ptr [esi+0]
 lock cmpxchg8b [edi]
 setz al
 pop esi
 pop edi
 pop ebx
end;

function InterlockedCompareExchange64(var Target:int64;NewValue:int64;Comperand:int64):int64; assembler; register;
asm
 push ebx
 push edi
 mov edi,eax
 mov edx,dword ptr [Comperand+4]
 mov eax,dword ptr [Comperand+0]
 mov ecx,dword ptr [NewValue+4]
 mov ebx,dword ptr [NewValue+0]
 lock cmpxchg8b [edi]
 pop edi
 pop ebx
end;
{$endif}
{$endif}

{$ifndef fpc}
{$ifdef cpu386}
function InterlockedDecrement(var Target:longint):longint; assembler; register;
asm
 mov edx,$ffffffff
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 dec eax
end;

function InterlockedIncrement(var Target:longint):longint; assembler; register;
asm
 mov edx,1
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 inc eax
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; assembler; register;
asm
 lock xchg dword ptr [eax],edx
 mov eax,edx
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; assembler; register;
asm
 xchg edx,eax
 lock xadd dword ptr [edx],eax
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; assembler; register;
asm
 xchg ecx,eax
 lock cmpxchg dword ptr [ecx],edx
end;
{$else}
function InterlockedDecrement(var Target:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=Windows.InterlockedDecrement(Target);
end;

function InterlockedIncrement(var Target:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=Windows.InterlockedIncrement(Target);
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=Windows.InterlockedExchange(Target,Source);
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=Windows.InterlockedExchangeAdd(Target,Source);
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=Windows.InterlockedCompareExchange(Target,NewValue,Comperand);
end;
{$endif}
{$else}
function InterlockedDecrement(var Target:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=System.InterlockedDecrement(Target);
end;

function InterlockedIncrement(var Target:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=System.InterlockedIncrement(Target);
end;

function InterlockedExchange(var Target:longint;Source:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=System.InterlockedExchange(Target,Source);
end;

function InterlockedExchangeAdd(var Target:longint;Source:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=System.InterlockedExchangeAdd(Target,Source);
end;

function InterlockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; {$ifdef caninline}inline;{$endif}
begin
 result:=System.InterlockedCompareExchange(Target,NewValue,Comperand);
end;
{$endif}

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

function UTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$ifdef FLREStrictUTF8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef FLREStrictUTF8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef FLREStrictUTF8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

function FLREUTF32CharToUTF8Len(CharValue:longword):longint;
begin
 if CharValue<=$7f then begin
  result:=1;
 end else if CharValue<=$7ff then begin
  result:=2;
 end else if CharValue<=$ffff then begin
  result:=3;
 end else if CharValue<=$1fffff then begin
  result:=4;
{$ifndef FLREStrictUTF8}
 end else if CharValue<=$3ffffff then begin
  result:=5;
 end else if CharValue<=$7fffffff then begin
  result:=6;
{$endif}
 end else begin
  result:=3;
 end;
end;

function FLREIsUTF8(const s:ansistring):boolean;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=false;
    exit;
   end;
  end;
 end;
 result:=(State=ucACCEPT) and (length(s)<>CodePoints);
end;

function UTF8Validate(const s:ansistring):boolean;
var CodeUnit:longint;
    State:longword;
begin
 State:=ucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  if State=ucERROR then begin
   result:=false;
   exit;
  end;
 end;
 result:=State=ucACCEPT;
end;

function UTF8Get(const s:ansistring):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=suNOUTF8;
    exit;
   end;
  end;
 end;
 if State=ucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=suISUTF8;
  end else begin
   result:=suPOSSIBLEUTF8;
  end;
 end else begin
  result:=suNOUTF8;
 end;
end;

function UTF8PtrGet(const s:pansichar;Len:longint):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=0 to Len-1 do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=suNOUTF8;
    exit;
   end;
  end;
 end;
 if State=ucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=suISUTF8;
  end else begin
   result:=suPOSSIBLEUTF8;
  end;
 end else begin
  result:=suNOUTF8;
 end;
end;

procedure UTF8SafeInc(const s:ansistring;var CodeUnit:longint);
var Len:longint;
    StartCodeUnit,State:longword;
begin
 Len:=length(s);
 if CodeUnit>0 then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure UTF8Inc(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure UTF8PtrInc(const s:pansichar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure UTF8Dec(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=(length(s)+1)) then begin
  dec(CodeUnit);
  while CodeUnit>0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure UTF8PtrDec(const s:pansichar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  dec(CodeUnit);
  while CodeUnit>=0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure UTF8Delete(var s:ansistring;CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function UTF8Length(const s:ansistring):longint; {$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,dword ptr [esi-4]
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function UTF8PtrLength(const s:ansistring;Len:longint):longint;
{$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,edx
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=0 to Len-1 do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function UTF8LengthEx(const s:ansistring):longint;
var State:longword;
    CodeUnit:longint;
begin
 result:=0;
 State:=ucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(result);
   end;
   ucERROR:begin
    result:=0;
    exit;
   end;
  end;
 end;
 if State=ucERROR then begin
  result:=0;
 end;
end;

function UTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
var CurrentCodeUnit,Len:longint;
begin
 if CodeUnit<1 then begin
  result:=-1;
 end else begin
  result:=0;
  CurrentCodeUnit:=1;
  Len:=length(s);
  while (CurrentCodeUnit<=Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function UTF8PtrGetCodePoint(const s:pansichar;Len,CodeUnit:longint):longint;
var CurrentCodeUnit:longint;
begin
 result:=-1;
 if CodeUnit<0 then begin
  CurrentCodeUnit:=0;
  while (CurrentCodeUnit<Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function UTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
var CurrentCodePoint,Len:longint;
begin
 if CodePoint<0 then begin
  result:=0;
 end else begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<=Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function UTF8PtrGetCodeUnit(const s:ansistring;Len,CodePoint:longint):longint;
var CurrentCodePoint:longint;
begin
 result:=-1;
 if CodePoint>=0 then begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function UTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=byte(ansichar(s[CodeUnit]));
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8PtrCodeUnitGetChar(const s:pansichar;Len,CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=byte(ansichar(s[CodeUnit]));
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8PtrCodeUnitGetCharAndInc(const s:pansichar;Len:longint;var CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
  end;
 end;
end;

function UTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function UTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
begin
 result:=UTF8CodeUnitGetChar(s,UTF8GetCodeUnit(s,CodePoint));
end;

function UTF8GetCharLen(const s:ansistring;i:longint):longword;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=UTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function UTF8Pos(const FindStr,InStr:ansistring):longint;
var i,j,l:longint;
    ok:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InStr) do begin
  l:=i+length(FindStr)-1;
  if l>length(InStr) then begin
   exit;
  end;
  ok:=true;
  for j:=1 to length(FindStr) do begin
   if InStr[i+j-1]<>FindStr[j] then begin
    ok:=false;
    break;
   end;
  end;
  if ok then begin
   result:=i;
   exit;
  end;
  inc(i,UTF8CharSteps[InStr[i]]);
 end;
end;

function UTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
var CodeUnit:longint;
begin
 result:='';
 CodeUnit:=1;
 while (CodeUnit<=length(Str)) and (Start>0) do begin
  inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
  dec(Start);
 end;
 if Start=0 then begin
  Start:=CodeUnit;
  while (CodeUnit<=length(Str)) and (Len>0) do begin
   inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
   dec(Len);
  end;
  if Start<CodeUnit then begin
   result:=copy(Str,Start,CodeUnit-Start);
  end;
 end;
end;

function UTF8UpperCase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef FLREStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr FLREUnicodeUpperCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+FLREUnicodeUpperCaseDeltaArrayBlockData[FLREUnicodeUpperCaseDeltaArrayIndexBlockData[FLREUnicodeUpperCaseDeltaArrayIndexIndexData[Value shr FLREUnicodeUpperCaseDeltaArrayIndexBlockBits],Value and FLREUnicodeUpperCaseDeltaArrayIndexBlockMask],CharValue and FLREUnicodeUpperCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef FLREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef FLREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8LowerCase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef FLREStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr FLREUnicodeLowerCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+FLREUnicodeLowerCaseDeltaArrayBlockData[FLREUnicodeLowerCaseDeltaArrayIndexBlockData[FLREUnicodeLowerCaseDeltaArrayIndexIndexData[Value shr FLREUnicodeLowerCaseDeltaArrayIndexBlockBits],Value and FLREUnicodeLowerCaseDeltaArrayIndexBlockMask],CharValue and FLREUnicodeLowerCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef FLREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef FLREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8Trim(const Str:ansistring):ansistring;
var i,j:longint;
begin
 i:=1;
 while UnicodeIsWhiteSpace(UTF8CodeUnitGetChar(Str,i)) do begin
  inc(i,UTF8CharSteps[Str[i]]);
 end;
 j:=length(Str)+1;
 UTF8Dec(Str,j);
 while UnicodeIsWhiteSpace(UTF8CodeUnitGetChar(Str,j)) do begin
  UTF8Dec(Str,j);
 end;
 if (j<=length(Str)) and (Str[j]>=#80) then begin
  inc(j,longint(UTF8GetCharLen(Str,j))-1);
 end;
 if i<=j then begin
  result:=copy(Str,i,(j-i)+1);
 end else begin
  result:='';
 end;
end;

function UTF8Correct(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 if (length(Str)=0) or UTF8Validate(Str) then begin
  result:=Str;
 end else begin
  result:='';
  CodeUnit:=1;
  Len:=length(Str);
  SetLength(result,Len*{$ifdef FLREStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef FLREStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef FLREStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);            
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8FromLatin1(const Str:ansistring):ansistring;
var CodeUnit:longint;
begin
 if UTF8Validate(Str) then begin
  result:=Str;
 end else begin
  result:='';
  for CodeUnit:=1 to length(Str) do begin
   result:=result+UTF32CharToUTF8(byte(ansichar(Str[CodeUnit])));
  end;
 end;
end;

function UTF8LevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Deletion,Insertion,Substitution:longint;
    si,tj:longword;
begin
 n:=UTF8LengthEx(s);
 m:=UTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,oi)=UTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  UTF8Dec(s,ci);
  UTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,ci)=UTF8CodeUnitGetChar(t,cj)) do begin
   UTF8Dec(s,ci);
   UTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  for i:=1 to n do begin
   si:=UTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   for j:=1 to m do begin
    tj:=UTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Deletion:=d[i-1,j]+1;
     Insertion:=d[i,j-1]+1;
     Substitution:=d[i-1,j-1]+1;
     if Deletion<Insertion then begin
      if Deletion<Substitution then begin
       d[i,j]:=Deletion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end else begin
      if Insertion<Substitution then begin
       d[i,j]:=Insertion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end;
    end else begin
     d[i,j]:=d[i-1,j-1];
    end;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function UTF8DamerauLevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Cost,Deletion,Insertion,Substitution,Transposition,Value:longint;
    si,tj,lsi,ltj:longword;
begin
 n:=UTF8LengthEx(s);
 m:=UTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,oi)=UTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  UTF8Dec(s,ci);
  UTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,ci)=UTF8CodeUnitGetChar(t,cj)) do begin
   UTF8Dec(s,ci);
   UTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  si:=0;
  for i:=1 to n do begin
   lsi:=si;
   si:=UTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   tj:=0;
   for j:=1 to m do begin
    ltj:=tj;
    tj:=UTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Cost:=1;
    end else begin
     Cost:=0;
    end;
    Deletion:=d[i-1,j]+1;
    Insertion:=d[i,j-1]+1;
    Substitution:=d[i-1,j-1]+Cost;
    if Deletion<Insertion then begin
     if Deletion<Substitution then begin
      Value:=Deletion;
     end else begin
      Value:=Substitution;
     end;
    end else begin
     if Insertion<Substitution then begin
      Value:=Insertion;
     end else begin
      Value:=Substitution;
     end;
    end;
    if ((i>1) and (j>1)) and ((si=ltj) and (lsi=tj)) then begin
     Transposition:=d[i-2,j-2]+Cost;
     if Transposition<Value then begin
      Value:=Transposition;
     end;
    end;
    d[i,j]:=Value;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function FLREStringLength(const s:ansistring):longint;
begin
 if FLREIsUTF8(s) then begin
  result:=UTF8Length(s);
 end else begin
  result:=length(s);
 end;
end;

function PopFirstOneBit(var Value:longword):longword;{$ifdef cpu386}assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
 push esi
 mov esi,Value
 xor eax,eax
 bsf ecx,dword ptr [esi]
 jz @Found
 xor eax,ecx
 xor edx,edx
 inc edx
 shl edx,cl
 xor dword ptr [esi],edx
 @Found:
 pop esi
end;
{$else}
{$ifdef cpuamd64}assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
{$ifdef win64}
 mov eax,dword ptr [rcx]
{$else}
 mov eax,dword ptr [rdi]
{$endif}
 lea edx,[eax-1]
 bsf eax,eax
{$ifdef win64}
 and dword ptr [rcx],edx
{$else}
 and dword ptr [rdi],edx
{$endif}
end;
{$else}
begin
{$ifdef fpc}
 result:=BsfByte(Value);
{$else}
 result:=(Value and (-Value))-1;
 result:=result-((result shr 1) and $55555555);
 result:=(result and $33333333)+((result shr 2) and $33333333);
 result:=(result+(result shr 4)) and $0f0f0f0f;
 inc(result,result shr 8);
 inc(result,result shr 16);
 result:=result and $1f;
{$endif}
 Value:=Value and (Value-1);
end;
{$endif}
{$endif}

function PtrPosChar(const Pattern:ansichar;const Text:pansichar;TextLength:longint;Offset:longint=0):longint;
type pptruint=^ptruint;
const MaskA=ptruint({$ifdef cpu64}$fefefefefefefeff{$else}$fefefeff{$endif}); // it is: 0-$01010101 / 0-$0101010101010101
      MaskB=ptruint({$ifdef cpu64}$8080808080808080{$else}$80808080{$endif});
var CurrentChar:pansichar;
    CurrentChunk:pptruint;
    XorMask,XoredChunk,Size:ptruint;
begin
 result:=-1;

 Size:=(TextLength-Offset)+1;
 if Size>0 then begin

  XorMask:=byte(Pattern);
  XorMask:=XorMask or (XorMask shl 8);
  XorMask:=XorMask or (XorMask shl 16);
{$ifdef cpu64}
  XorMask:=XorMask or (XorMask shl 32);
{$endif}

  CurrentChar:=@Text[Offset];

  if Size>(SizeOf(ptruint)*2) then begin

   // Alignment initialization
   CurrentChunk:=pointer(ptruint(ptruint(CurrentChar) and not (SizeOf(ptruint)-1)));

   // Try to get first chunk
   if ptruint(CurrentChunk)>=ptruint(Text) then begin
    // Yes, we can the get first chunk
    XoredChunk:=CurrentChunk^ xor XorMask;
   end else begin
    // No, so return dummy value to force to check the few first characters
    XoredChunk:=0;
   end;

   // Jump to next chunk
   inc(CurrentChunk);

   // Subtract the first chunk from size
   dec(ptruint(Size),ptruint(CurrentChunk)-ptruint(CurrentChar));

   // Scan first chunk
   if (((XoredChunk+MaskA) and not XoredChunk) and MaskB)<>0 then begin
    while ptruint(CurrentChar)<ptruint(CurrentChunk) do begin
     if CurrentChar^=Pattern then begin
      result:=ptruint(CurrentChar)-ptruint(Text);
      exit;
     end;
     inc(CurrentChar);
    end;
   end;

   // Scan until the last whole chunk
   while Size>=SizeOf(ptruint) do begin
    XoredChunk:=CurrentChunk^ xor XorMask;
    if (((XoredChunk+MaskA) and not XoredChunk) and MaskB)<>0 then begin
{$ifdef POSCHARSAFECHECK}
    CurrentChar:=pointer(CurrentChunk);
    if CurrentChar[0]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[0]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[1]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[1]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[2]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[2]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[3]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[3]))-ptruint(Text);
      exit;
     end;
{$ifdef cpu64}
     if CurrentChar[4]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[4]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[5]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[5]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[6]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[6]))-ptruint(Text);
      exit;
     end;
     if CurrentChar[7]=Pattern then begin
      result:=ptruint(pointer(@CurrentChar[7]))-ptruint(Text);
      exit;
     end;
{$endif}
{$else}
     CurrentChar:=pointer({$ifdef BIG_ENDIAN}ptruint(ptruint(CurrentChunk)+ptruint(SizeOf(ptruint)-1)){$else}CurrentChunk{$endif});
     XoredChunk:=XoredChunk xor XorMask;
     while (XoredChunk<>0) and ((XoredChunk and $ff)<>byte(Pattern)) do begin
      XoredChunk:=XoredChunk shr 8;
      {$ifdef BIG_ENDIAN}dec{$else}inc{$endif}(CurrentChar);
     end;
     if XoredChunk<>0 then begin
      result:=ptruint(pointer(CurrentChar))-ptruint(Text);
      exit;
     end;
{$endif}
    end;
    inc(CurrentChunk);
    dec(Size,SizeOf(ptruint));
   end;

   // Set chunkwise to charwise pointer
   CurrentChar:=pointer(CurrentChunk);
  end;

  // Scan rest of the remained characters, if there are any
  while Size>0 do begin
   if CurrentChar^=Pattern then begin
    result:=ptruint(pointer(CurrentChar))-ptruint(Text);
    exit;
   end;
   inc(CurrentChar);
   dec(Size);
  end;

 end;
end;

function PtrPosBoyerMoore(const Pattern:ansistring;const Text:pansichar;const TextLength:longint;const Skip:TFLRECharPatternBitMasks;const Next:TFLREBoyerMooreNext;Position:longint=0):longint;
var PatternPosition,BadSkip,GoodSkip,PatternLength:longint;
begin
 PatternLength:=length(Pattern);
 result:=-1;
 if PatternLength<>0 then begin
  Position:=PtrPosChar(Pattern[1],Text,TextLength,Position);
  if Position>=0 then begin
   inc(Position,PatternLength-1);
   while Position<TextLength do begin
    PatternPosition:=0;
    while (PatternPosition<PatternLength) and (Text[Position-PatternPosition]=Pattern[PatternLength-PatternPosition]) do begin
     inc(PatternPosition);
    end;
    if PatternPosition<>PatternLength then begin
     BadSkip:=Skip[Text[Position-PatternPosition]];
     GoodSkip:=Next[PatternPosition];
     if BadSkip>GoodSkip then begin
      inc(Position,BadSkip-PatternPosition);
     end else begin
      inc(Position,GoodSkip);
     end;
    end else begin
     result:=(Position-PatternLength)+1;
     exit;
    end;
   end;
  end;
 end;
end;

function PtrPosPattern(const PatternLength:longint;const Text:pansichar;TextLength:longint;const PatternBitMasks:TFLRECharPatternBitMasks;Position:longint=0):longint;
var CheckPosition:longint;
    State:longword;
begin
 case PatternLength of
  0:begin
   // Nothing
  end;
  1:begin
   // Naive single char class set search
   while Position<TextLength do begin
    if PatternBitMasks[Text[Position]]<>0 then begin
     result:=Position;
     exit;
    end else begin
     inc(Position);
    end;
   end;
  end;
  2:begin
   // SBNDMQ1
   inc(Position,PatternLength-1);
   while Position<TextLength do begin
    State:=PatternBitMasks[Text[Position]];
    if State<>0 then begin
     CheckPosition:=Position-PatternLength;
     repeat
      dec(Position);
      if Position<0 then begin
       break;
      end;
      State:=(State shr 1) and PatternBitMasks[Text[Position]];
     until State=0;
     if Position=CheckPosition then begin
      result:=CheckPosition+1;
      exit;
     end;
    end;
    inc(Position,PatternLength);
   end;
  end;
  else begin
   // SBNDMQ2
   inc(Position,PatternLength-2);
   dec(TextLength);
   while Position<TextLength do begin
    State:=(PatternBitMasks[Text[Position+1]] shr 1) and PatternBitMasks[Text[Position]];
    if State<>0 then begin
     CheckPosition:=Position-(PatternLength-1);
     repeat
      dec(Position);
      if Position<0 then begin
       break;
      end;
      State:=(State shr 1) and PatternBitMasks[Text[Position]];
     until State=0;
     if Position=CheckPosition then begin
      result:=CheckPosition+1;
      exit;
     end;
    end;
    inc(Position,PatternLength-1);
   end;
  end;
 end;
 result:=-1;
end;

function PtrCopy(const Src:PAnsiChar;From,Len:longint):ansistring;
begin
 SetLength(result,Len);
 if Len>0 then begin
  Move(Src[From],result[1],Len);
 end;
end;

function HashString(const Str:ansistring):longword;
{$ifdef cpuarm}
var b:pansichar;
    len,h,i:longword;
begin
 result:=2166136261;
 len:=length(Str);
 h:=len;
 if len>0 then begin
  b:=pansichar(Str);
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
var b:pansichar;
    h,k,len:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 len:=length(Str);
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=pansichar(Str);
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

function HashData(const Data:pointer;Len:longword):longword;
{$ifdef cpuarm}
var b:pansichar;
    h,i:longword;
begin
 result:=$811c9dc5;
 h:=Len;
 if Len>0 then begin
  b:=Data;
  while Len>3 do begin
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
   dec(Len,4);
  end;
  if Len>1 then begin
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
   dec(Len,2);
  end;
  if Len>0 then begin
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
var b:pansichar;
    h,k:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=Data;
  while Len>7 do begin
   begin
    p:=longword(pointer(b)^)*qword(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*qword(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=longword(pointer(b)^)*qword(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*qword(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*qword(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

function UTF8RangeToRegEx(Lo,Hi:longword):ansistring;
type TString6Chars=string[6];
const Seq0010ffff:array[0..6,0..4,0..1] of longint=((($00,$7f),(-1,-1),(-1,-1),(-1,-1),(-1,-1)),        // 00-7F
                                                    (($c2,$df),($80,$bf),(-1,-1),(-1,-1),(-1,-1)),      // C2-DF 80-BF
                                                    (($e0,$e0),($a0,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E0-E0 A0-BF 80-BF
                                                    (($e1,$ef),($80,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E1-EF 80-BF 80-BF
                                                    (($f0,$f0),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F0-F0 90-BF 80-BF 80-BF
                                                    (($f1,$f3),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F1-F3 80-BF 80-BF 80-BF
                                                    (($f4,$f4),($80,$bf),($80,$bf),($80,$bf),(-1,-1))); // F4-F4 80-8F 80-BF 80-BF
      HexChars:array[$0..$f] of ansichar='0123456789ABCDEF';
var OutputCharSequence:ansistring;
 function ToString(CharValue:longword):TString6Chars;
 begin
  case CharValue of
   $00000000..$0000007f:begin
    SetLength(result,1);
    result[1]:=ansichar(byte(CharValue));
   end;
   $00000080..$000007ff:begin
    SetLength(result,2);
    result[1]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    result[2]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
// {$ifdef PLREStrictUTF8}$00000800..$0000d7ff,$0000e000..$0000ffff{$else}$00000800..$0000ffff{$endif}:begin
   $00000800..$0000ffff:begin
    SetLength(result,3);
    result[1]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[3]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00010000..$0010ffff:begin
    SetLength(result,4);
    result[1]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[4]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00200000..$03ffffff:begin
    SetLength(result,5);
    result[1]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[5]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $04000000..$7fffffff:begin
    SetLength(result,6);
    result[1]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[5]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[6]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   else begin
    SetLength(result,3);
    result[1]:=#$ef;
    result[2]:=#$bf;
    result[3]:=#$bd;
   end;
  end;
 end;
 procedure AddRange(Lo,Hi:byte);
 var Data:array[0..11] of ansichar;
 begin
  Data:='[\x00-\x00]';
  Data[3]:=HexChars[(Lo shr 4) and $f];
  Data[4]:=HexChars[Lo and $f];
  Data[8]:=HexChars[(Hi shr 4) and $f];
  Data[9]:=HexChars[Hi and $f];
  OutputCharSequence:=OutputCharSequence+Data;
 end;
 procedure ProcessRange(Lo,Hi:longword);
 var i,m:longword;
     StrLo,StrHi:TString6Chars;
 begin
  if Hi>$0010ffff then begin
   Hi:=$0010ffff;
  end;
  if Lo<=Hi then begin
    if (Lo=$00000000) and (Hi=$0010ffff) then begin
    for m:=low(Seq0010ffff) to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     OutputCharSequence:=OutputCharSequence+'|';
    end;
   end else if (Lo=$00000080) and (Hi=$0010ffff) then begin
    for m:=1 to high(Seq0010ffff) do begin
     for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
      if Seq0010ffff[m,i,0]<0 then begin
       break;
      end;
      AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
     end;
     OutputCharSequence:=OutputCharSequence+'|';
    end;
   end else begin
    for i:=1 to 3 do begin
     if i=1 then begin
      m:=7;
     end else begin
      m:=(7-i)+(6*(i-1));
     end;
     m:=(1 shl m)-1;
     if (Lo<=m) and (m<Hi) then begin
      ProcessRange(Lo,m);
      ProcessRange(m+1,Hi);
      exit;
     end;
    end;
    if Hi<128 then begin
     AddRange(Lo,Hi);
     OutputCharSequence:=OutputCharSequence+'|';
    end else begin
     for i:=1 to 3 do begin
      m:=(1 shl (6*i))-1;
      if (Lo and not m)<>(Hi and not m) then begin
       if (Lo and m)<>0 then begin
        ProcessRange(Lo,Lo or m);
        ProcessRange((Lo or m)+1,Hi);
        exit;
       end else if (Hi and m)<>m then begin
        ProcessRange(Lo,(Hi and not m)-1);
        ProcessRange(Hi and not m,Hi);
        exit;
       end;
      end;
     end;
     StrLo:=ToString(Lo);
     StrHi:=ToString(Hi);
     if length(StrLo)=length(StrHi) then begin
      for i:=1 to length(StrLo) do begin
       AddRange(byte(StrLo[i]),byte(StrHi[i]));
      end;
      OutputCharSequence:=OutputCharSequence+'|';
     end;
    end;
   end;
  end;
 end;
begin
 OutputCharSequence:='';
 ProcessRange(Lo,Hi);
 result:=copy(OutputCharSequence,1,length(OutputCharSequence)-1);
end;

function HashDFAState(const Key:PFLREDFAState):longword;
begin
 if assigned(Key) and (Key.CountInstructions>0) then begin
  result:=((HashData(@Key.Instructions[0],Key.CountInstructions*sizeof(PFLREInstruction))) xor ((longword(Key.CountInstructions) shr 16) or (longword(Key.CountInstructions) shl 16))) xor ((Key.Flags shl 19) or (Key.Flags shr 13));
  if result=0 then begin
   result:=$ffffffff;
  end;
 end else begin
  result:=0;
 end;
end;

function CompareDFAState(const a,b:PFLREDFAState):boolean;
var i:integer;
begin
 result:=a=b;
 if not result then begin
  if (assigned(a) and assigned(b)) and ((a.CountInstructions=b.CountInstructions) and (a.Flags=b.Flags)) then begin
   result:=true;
   for i:=0 to a.CountInstructions-1 do begin
    if a.Instructions[i]<>b.Instructions[i] then begin
     result:=false;
     exit;
    end;
   end;
  end;
 end;
end;

constructor TFLREDFAStateHashMap.Create;
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

destructor TFLREDFAStateHashMap.Destroy;
begin
 Clear;
 SetLength(Entities,0);
 inherited Destroy;
end;

procedure TFLREDFAStateHashMap.Clear;
begin
 RealSize:=0;
 LogSize:=0;
 Size:=0;
 SetLength(Entities,0);
 SetLength(EntityToCellIndex,0);
 SetLength(CellToEntityIndex,0);
 Resize;
end;

function TFLREDFAStateHashMap.FindCell(const Key:PFLREDFAState):longword;
var HashCode,Mask,Step:longword;
    Entity:longint;
begin
 HashCode:=HashDFAState(Key);
 Mask:=(2 shl LogSize)-1;
 Step:=((HashCode shl 1)+1) and Mask;
 if LogSize<>0 then begin
  result:=HashCode shr (32-LogSize);
 end else begin
  result:=0;
 end;
 repeat
  Entity:=CellToEntityIndex[result];
  if (Entity=ENT_EMPTY) or ((Entity<>ENT_DELETED) and CompareDFAState(Entities[Entity].Key,Key)) then begin
   exit;
  end;
  result:=(result+Step) and Mask;
 until false;
end;

function TFLREDFAStateHashMap.Add(const Key:PFLREDFAState):PFLREDFAStateHashMapEntity;
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
 end;
end;

procedure TFLREDFAStateHashMap.Resize;
var NewLogSize,NewSize,Cell,Entity,Counter:longint;
    OldEntities:TFLREDFAStateHashMapEntities;
    OldCellToEntityIndex:TFLREDFAStateHashMapEntityIndices;
    OldEntityToCellIndex:TFLREDFAStateHashMapEntityIndices;
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
    Add(OldEntities[Counter].Key);
   end;
  end;
 end;
end;

function TFLREDFAStateHashMap.Get(const Key:PFLREDFAState):PFLREDFAState;
var Entity:longint;
    Cell:longword;
begin
 result:=nil;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  result:=Entities[Entity].Key;
 end;
end;

function TFLREDFAStateHashMap.Delete(const Key:PFLREDFAState):boolean;
var Entity:longint;
    Cell:longword;
begin
 result:=false;
 Cell:=FindCell(Key);
 Entity:=CellToEntityIndex[Cell];
 if Entity>=0 then begin
  Entities[Entity].Key:=nil;
  Entities[Entity].Value:=0;
  EntityToCellIndex[Entity]:=CELL_DELETED;
  CellToEntityIndex[Cell]:=ENT_DELETED;
  dec(RealSize);
  result:=true;
 end;
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

function TFLREStringIntegerPairHashMap.FindCell(const Key:ansistring):longword;
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

function TFLREStringIntegerPairHashMap.Add(const Key:ansistring;Value:TFLREStringIntegerPairHashMapData):PFLREStringIntegerPairHashMapEntity;
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

function TFLREStringIntegerPairHashMap.Get(const Key:ansistring;CreateIfNotExist:boolean=false):PFLREStringIntegerPairHashMapEntity;
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

function TFLREStringIntegerPairHashMap.Delete(const Key:ansistring):boolean;
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

function TFLREStringIntegerPairHashMap.GetValue(const Key:ansistring):TFLREStringIntegerPairHashMapData;
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

procedure TFLREStringIntegerPairHashMap.SetValue(const Key:ansistring;const Value:TFLREStringIntegerPairHashMapData);
begin
 Add(Key,Value);
end;

constructor TFLRE.Create(const ARegularExpression:ansistring;const AFlags:TFLREFlags=[]);
var FLREDFAStateCreateTempDFAState:TFLREDFAState;
begin
 inherited Create;

 Generation:=0;
 CountParens:=0;

 AnchoredRootNode:=nil;

 UnanchoredRootNode:=nil;

 Nodes:=TList.Create;

 ForwardInstructions:=nil;
 CountForwardInstructions:=0;

 BackwardInstructions:=nil;
 CountBackwardInstructions:=0;

 AnchoredStartInstruction:=nil;
 UnanchoredStartInstruction:=nil;
 ReversedStartInstruction:=nil;

 CharClasses:=nil;
 CountCharClasses:=0;

 ThreadLists[0].Threads:=nil;
 ThreadLists[1].Threads:=nil;

 FreeSubMatches:=nil;

 AllSubMatches:=TList.Create;

 RegularExpression:=ARegularExpression;

 Flags:=AFlags;

 FixedString:='';
 FixedStringBoyerMooreNext:=nil;

 OnePassNFANodes:=nil;
 OnePassNFANodesCount:=0;
 OnePassNFAStart:=nil;
 OnePassNFAStateSize:=0;
 OnePassNFAWorkCaptures:=nil;
 OnePassNFAMatchCaptures:=nil;
 OnePassNFACharClassActions:=nil;
 OnePassNFAReady:=false;

 BitStateNFACountVisited:=0;
 BitStateNFAJobs:=nil;
 BitStateNFACountJobs:=0;
 BitStateNFAMaxJob:=0;
 BitStateNFAWorkCaptures:=nil;
 BitStateNFAMatchCaptures:=nil;
 BitStateNFAReady:=false;

 DFAStackInstructions:=nil;
 DFAStateCache:=TFLREDFAStateHashMap.Create;
 DFAAnchoredStartState:=nil;
 DFAUnanchoredStartState:=nil;
 DFAReversedStartState:=nil;
 DFADeadState:=nil;
 DFANextStatesSize:=0;
 DFAStateSize:=0;
 DFAStatePoolUsed:=nil;
 DFAStatePoolFree:=nil;
 DFAStatePoolSize:=0;
 DFAStatePoolSizePowerOfTwo:=0;
 DFANeedVerification:=false;

 BeginningJump:=false;
 BeginningSplit:=false;
 BeginningWildCard:=false;
 BeginningAnchor:=false;

 NamedGroupStringList:=TStringList.Create;
 NamedGroupStringIntegerPairHashMap:=TFLREStringIntegerPairHashMap.Create;

 try

  try

   Parse;

   Compile;

  finally
   SetLength(CharClasses,CountCharClasses);
  end;

  CountSubMatches:=(CountParens+1)*2;

  SetLength(ThreadLists[0].Threads,(CountForwardInstructions+1)*4);
  SetLength(ThreadLists[1].Threads,(CountForwardInstructions+1)*4);

  CompilePrefix;

  CompileFixedStringSearch;

  CompilePrefixCharClasses;

  CompileByteMapForOnePassNFAAndDFA;

  CompileOnePassNFA;

  if OnePassNFAReady then begin
   SetLength(OnePassNFAWorkCaptures,CountParens*2);
   SetLength(OnePassNFAMatchCaptures,CountParens*2);
  end;

  BitStateNFAReady:=(CountForwardInstructions>0) and (CountForwardInstructions<512);
  if BitStateNFAReady then begin
   SetLength(BitStateNFAWorkCaptures,CountParens*2);
   SetLength(BitStateNFAMatchCaptures,CountParens*2);
  end;

  begin
   DFACountStatesCached:=0;

   DFAStackInstructions:=nil;
   SetLength(DFAStackInstructions,(CountForwardInstructions+1) shl 1);

   DFAStatePoolUsed:=nil;
   DFAStatePoolFree:=nil;

   DFANextStatesSize:=ByteMapCount*sizeof(PFLREDFAState);
   DFAStateSize:=ptrint(ptruint(pointer(@FLREDFAStateCreateTempDFAState.NextStates))-ptruint(pointer(@FLREDFAStateCreateTempDFAState)))+DFANextStatesSize;

   DFAStatePoolSize:=(65536 div DFAStateSize)*DFAStateSize;
   if DFAStatePoolSize=0 then begin
    DFAStatePoolSize:=DFAStateSize*64;
   end;

   DFAStatePoolSizePowerOfTwo:=RoundUpToPowerOfTwo(DFAStatePoolSize);

   DFAAllocateNewStatePool;

   FillChar(DFATemporaryState,SizeOf(TFLREDFAState),AnsiChar(#0));
   FillChar(DFANewState,SizeOf(TFLREDFAState),AnsiChar(#0));

   inc(Generation);
   GetMem(DFAAnchoredStartState,DFAStateSize);
   FillChar(DFAAnchoredStartState^,DFAStateSize,AnsiChar(#0));
   DFAAddInstructionThread(DFAAnchoredStartState,AnchoredStartInstruction);
   DFAAnchoredStartState^.Flags:=DFAAnchoredStartState^.Flags or sfDFAStart;
   DFAStateCache.Add(DFAAnchoredStartState);
   inc(DFACountStatesCached);

   inc(Generation);
   GetMem(DFAUnanchoredStartState,DFAStateSize);
   FillChar(DFAUnanchoredStartState^,DFAStateSize,AnsiChar(#0));
   DFAAddInstructionThread(DFAUnanchoredStartState,UnanchoredStartInstruction);
   DFAUnanchoredStartState^.Flags:=DFAUnanchoredStartState^.Flags or sfDFAStart;
   DFAStateCache.Add(DFAUnanchoredStartState);
   inc(DFACountStatesCached);

   inc(Generation);
   GetMem(DFAReversedStartState,DFAStateSize);
   FillChar(DFAReversedStartState^,DFAStateSize,AnsiChar(#0));
   DFAAddInstructionThread(DFAReversedStartState,ReversedStartInstruction);
   DFAReversedStartState^.Flags:=DFAReversedStartState^.Flags or sfDFAStart;
   DFAStateCache.Add(DFAReversedStartState);
   inc(DFACountStatesCached);

   inc(Generation);
   GetMem(DFADeadState,DFAStateSize);
   FillChar(DFADeadState^,DFAStateSize,AnsiChar(#0));
   DFAAddInstructionThread(DFADeadState,AnchoredStartInstruction);
   DFADeadState^.Flags:=DFADeadState^.Flags or sfDFADead;
   DFAStateCache.Add(DFADeadState);
   inc(DFACountStatesCached);

  end;

  BeginningWildcardLoop:=BeginningJump and BeginningSplit and BeginningWildcard;
  if rfLAZY in Flags then begin
   DoUnanchoredStart:=false;
  end else if rfGREEDY in Flags then begin
   DoUnanchoredStart:=not BeginningAnchor;
  end else begin
   DoUnanchoredStart:=(FixedStringLength=0) and (CountObviousPrefixCharClasses<CountPrefixCharClasses) and not BeginningAnchor;
  end;

 finally
 end;

end;

destructor TFLRE.Destroy;
var Index:longint;
    SubMatches:PFLRESubMatches;
    NextCharClassAction:PFLREOnePassNFAStateCharClassAction;
    Flags:longword;
begin

 for Index:=0 to Nodes.Count-1 do begin
  FreeMem(Nodes[Index]);
 end;
 FreeAndNil(Nodes);

 SetLength(ForwardInstructions,0);

 SetLength(BackwardInstructions,0);

 for Index:=0 to CountCharClasses-1 do begin
  FreeMem(CharClasses[Index]);
 end;
 SetLength(CharClasses,0);
 CountCharClasses:=0;

 SetLength(ThreadLists[0].Threads,0);
 SetLength(ThreadLists[1].Threads,0);

 while assigned(FreeSubMatches) do begin
  SubMatches:=FreeSubMatches;
  FreeSubMatches:=FreeSubMatches^.Next;
  SetLength(SubMatches^.SubMatches,0);
  Finalize(SubMatches^);
  FreeMem(SubMatches);
 end;
 FreeSubMatches:=nil;

 FreeAndNil(AllSubMatches);

 if assigned(OnePassNFANodes) then begin
  FreeMem(OnePassNFANodes);
  OnePassNFANodes:=nil;
 end;
 OnePassNFANodesCount:=0;
 OnePassNFAStart:=nil;
 OnePassNFAStateSize:=0;
 SetLength(OnePassNFAWorkCaptures,0);
 SetLength(OnePassNFAMatchCaptures,0);

 while assigned(OnePassNFACharClassActions) do begin
  NextCharClassAction:=OnePassNFACharClassActions^.AllNext;
  FreeMem(OnePassNFACharClassActions);
  OnePassNFACharClassActions:=NextCharClassAction;
 end;
 OnePassNFACharClassActions:=nil;

 SetLength(BitStateNFAJobs,0);
 SetLength(BitStateNFAWorkCaptures,0);
 SetLength(BitStateNFAMatchCaptures,0);

 SetLength(FixedStringBoyerMooreNext,0);

 DFADestroyStatePool(DFAStatePoolUsed);
 DFADestroyStatePool(DFAStatePoolFree);
 DFAStatePoolUsed:=nil;
 DFAStatePoolFree:=nil;
 DFAFreeState(@DFATemporaryState);
 DFAFreeState(@DFANewState);
 if assigned(DFAAnchoredStartState) then begin
  DFAFreeState(DFAAnchoredStartState);
  FreeMem(DFAAnchoredStartState);
 end;
 if assigned(DFAUnanchoredStartState) then begin
  DFAFreeState(DFAUnanchoredStartState);
  FreeMem(DFAUnanchoredStartState);
 end;
 if assigned(DFAReversedStartState) then begin
  DFAFreeState(DFAReversedStartState);
  FreeMem(DFAReversedStartState);
 end;
 if assigned(DFADeadState) then begin
  DFAFreeState(DFADeadState);
  FreeMem(DFADeadState);
 end;
 FreeAndNil(DFAStateCache);
 SetLength(DFAStackInstructions,0);

 NamedGroupStringList.Free;
 NamedGroupStringIntegerPairHashMap.Free;

 inherited Destroy;
end;

function TFLRE.NewNode(const NodeType:longint;const Left,Right,Extra:PFLRENode;const Value:longint):PFLRENode;
begin
 GetMem(result,SizeOf(TFLRENode));
 FillChar(result^,SizeOf(TFLRENode),#0);
 result^.NodeType:=NodeType;
 result^.Left:=Left;
 result^.Right:=Right;
 result^.Extra:=Extra;
 result^.Value:=Value;
 result^.Index:=Nodes.Add(result);
end;

procedure TFLRE.FreeNode(var Node:PFLRENode);
begin
 if assigned(Node) then begin
  if assigned(Node^.Left) then begin
   FreeNode(Node^.Left);
  end;
  if assigned(Node^.Right) then begin
   FreeNode(Node^.Right);
  end;
  if assigned(Node^.Extra) then begin
   FreeNode(Node^.Extra);
  end;
  FreeMem(Node);
  Node:=nil;
 end;
end;

function TFLRE.AreNodesEqual(NodeA,NodeB:PFLRENode):boolean;
begin
 result:=(NodeA=NodeB) or
         ((((assigned(NodeA) and assigned(NodeB))) and
          ((NodeA^.NodeType=NodeB^.NodeType) and
           ((NodeA^.Value=NodeB^.Value) and
            (NodeA^.CharClass=NodeB^.CharClass) and
            ((AreNodesEqual(NodeA^.Left,NodeB^.Left) and AreNodesEqual(NodeA^.Right,NodeB^.Right)) and AreNodesEqual(NodeA^.Extra,NodeB^.Extra))))) or
          not (assigned(NodeA) or assigned(NodeB)));
end;

function TFLRE.AreNodesEqualSafe(NodeA,NodeB:PFLRENode):boolean;
begin
 result:=(NodeA=NodeB) or
         ((((assigned(NodeA) and assigned(NodeB))) and
           (((NodeA^.NodeType=NodeB^.NodeType) and not (NodeB^.NodeType in [ntPAREN])) and
            ((NodeA^.Value=NodeB^.Value) and
             (NodeA^.CharClass=NodeB^.CharClass) and
             ((AreNodesEqualSafe(NodeA^.Left,NodeB^.Left) and
               AreNodesEqualSafe(NodeA^.Right,NodeB^.Right)) and
               AreNodesEqualSafe(NodeA^.Extra,NodeB^.Extra))))) or
          not (assigned(NodeA) or assigned(NodeB)));
end;

function TFLRE.Concat(NodeLeft,NodeRight:PFLRENode):PFLRENode;
var NodeTemp:PFLRENode;
begin
 if assigned(NodeLeft) and assigned(NodeRight) then begin
  if ((NodeLeft^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (NodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(NodeLeft^.Left,NodeRight^.Left) and (NodeLeft^.Value=0) and (NodeRight^.Value=0) then begin
   result:=NodeRight;
  end else if ((NodeLeft^.NodeType in [ntSTAR,ntPLUS]) and (NodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(NodeLeft^.Left,NodeRight^.Left) and (NodeLeft^.Value=0) and (NodeRight^.Value=0) then begin
   result:=NodeLeft;
  end else if (NodeLeft^.NodeType=ntCAT) and assigned(NodeLeft^.Left) and assigned(NodeLeft^.Right) then begin
   if ((NodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (NodeRight^.NodeType=ntPLUS)) and AreNodesEqualSafe(NodeLeft^.Right^.Left,NodeRight^.Left) and (NodeLeft^.Right^.Value=0) and (NodeRight^.Value=0) then begin
    NodeLeft^.Right:=NodeRight;
    result:=NodeLeft;
   end else if ((NodeLeft^.Right^.NodeType in [ntSTAR,ntPLUS]) and (NodeRight^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(NodeLeft^.Right^.Left,NodeRight^.Left) and (NodeLeft^.Right^.Value=0) and (NodeRight^.Value=0) then begin
    result:=NodeLeft;
   end else begin
    result:=NewNode(ntCAT,NodeLeft,NodeRight,nil,0);
   end;
  end else begin
   result:=NewNode(ntCAT,NodeLeft,NodeRight,nil,0);
  end;
 end else begin
  if assigned(NodeLeft) then begin
   result:=NodeLeft;
  end else if assigned(NodeRight) then begin
   result:=NodeRight;
  end else begin
   result:=nil;
  end;
 end;
 while (assigned(result) and (result^.NodeType=ntCAT)) and (assigned(result^.Left) and assigned(result^.Right)) do begin
  if (result^.Left^.NodeType=ntCAT) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and assigned(result^.Left^.Right) and (result^.Right^.Value=0) then begin
   if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
    result^.Left^.Right:=result^.Right;
    result:=result^.Left;
    continue;
   end else if ((result^.Left^.Right^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Right^.Left,result^.Right^.Left) then begin
    result:=result^.Left;
    continue;
   end;
  end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntCAT) and assigned(result^.Right^.Left) and (result^.Left^.Value=0) then begin
   if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.Left^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
    result:=result^.Right;
    continue;
   end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.Left^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left^.Left) and (result^.Right^.Left^.Value=0) then begin
    result^.Right^.Left:=result^.Left;
    result:=result^.Right;
    continue;
   end;
  end else if (result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Left^.Value=0) and (result^.Right^.Value=0) then begin
   if ((result^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (result^.Right^.NodeType=ntPLUS)) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
    result:=result^.Right;
    continue;
   end else if ((result^.Left^.NodeType in [ntSTAR,ntPLUS]) and (result^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(result^.Left^.Left,result^.Right^.Left) then begin
    result:=result^.Left;
    continue;
   end;
  end;
  break;
 end;
end;

function TFLRE.NewAlt(NodeLeft,NodeRight:PFLRENode):PFLRENode;
var NodeEx,pl,pr:PPFLRENode;
    Node,l,r:PFLRENode;
    c:TFLRECharClass;
begin
 if assigned(NodeLeft) and assigned(NodeRight) then begin
  if (NodeLeft^.NodeType=ntCAT) and (NodeRight^.NodeType=ntCAT) then begin
   result:=NewNode(ntALT,NodeLeft,NodeRight,nil,0);
   NodeEx:=@result;
   while (((assigned(NodeEx) and assigned(NodeEx^)) and (NodeEx^^.NodeType=ntALT)) and (assigned(NodeEx^^.Left) and assigned(NodeEx^^.Right))) and ((NodeEx^^.Left^.NodeType=ntCAT) and (NodeEx^^.Right^.NodeType=ntCAT)) do begin
    Node:=NodeEx^;
    pl:=@Node^.Left;
    l:=Node^.Left;
    while (assigned(l) and assigned(l^.Left)) and (l^.Left^.NodeType=ntCAT) do begin
     pl:=@l^.Left;
     l:=l^.Left;
    end;
    pr:=@Node^.Right;
    r:=Node^.Right;
    while (assigned(r) and assigned(r^.Left)) and (r^.Left^.NodeType=ntCAT) do begin
     pr:=@r^.Left;
     r:=r^.Left;
    end;
    if ((assigned(l^.Left) and assigned(l^.Right)) and (assigned(r^.Left) and assigned(r^.Right))) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
     NodeEx^:=l;
     pl^:=l^.Right;
     pr^:=r^.Right;
     l^.Right:=Node;
     NodeEx:=@l^.Right;
     continue;
    end;
    break;
   end;
  end else if AreNodesEqualSafe(NodeLeft,NodeRight) then begin
   result:=NodeLeft;
  end else if (NodeLeft^.NodeType=ntCHAR) and (NodeRight^.NodeType=ntCHAR) then begin
   result:=NewNode(ntCHAR,nil,nil,nil,0);
   result^.CharClass:=NodeLeft^.CharClass+NodeRight^.CharClass;
// result:=ConvertUTF8CharClassNode(NewNode(ntCHAR,nil,nil,nil,RegExp.AddCharClass(c)));
  end else begin
   result:=NewNode(ntALT,NodeLeft,NodeRight,nil,0);
  end;
 end else begin
  if assigned(NodeLeft) then begin
   result:=NodeLeft;
  end else if assigned(NodeRight) then begin
   result:=NodeRight;
  end else begin
   result:=nil;
  end;
 end;
end;

{ * (a*)* equals a*
  * (a+)+ equals a+
  * (a?)? equals a?
  * (a*)+ equals (a+)* equals a*
  * (a*)? equals (a?)* equals a*
  * (a+)? equals (a?)+ equals a*
}
function TFLRE.NewPlus(Node:PFLRENode;Kind:longint):PFLRENode;
begin
 if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntSTAR])) and (Node^.Left^.Value=qkGREEDY))) then begin
  result:=Node;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPLUS)) and (Node^.Value=qkGREEDY)) then begin
  result:=Node;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntSTAR,ntQUEST])) and (Node^.Value=qkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntPLUS,Node,nil,nil,Kind);
 end;
end;

function TFLRE.NewStar(Node:PFLRENode;Kind:longint):PFLRENode;
begin
  if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (Node^.Left^.Value=qkGREEDY))) then begin
  result:=Node;
  result^.Left^.NodeType:=ntSTAR;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntPLUS,ntQUEST,ntSTAR])) and (Node^.Value=qkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntSTAR,Node,nil,nil,Kind);
 end;
end;

function TFLRE.NewQuest(Node:PFLRENode;Kind:longint):PFLRENode;
begin
 if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType=ntQUEST)) and (Node^.Left^.Value=qkGREEDY))) then begin
  result:=Node;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntPAREN)) and ((assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and (Node^.Left^.Value=qkGREEDY))) then begin
  result:=Node;
  result^.Left^.NodeType:=ntSTAR;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType=ntQUEST)) and (Node^.Value=qkGREEDY)) then begin
  result:=Node;
 end else if (Kind=qkGREEDY) and ((assigned(Node) and (Node^.NodeType in [ntPLUS,ntSTAR])) and (Node^.Value=qkGREEDY)) then begin
  result:=Node;
  result^.NodeType:=ntSTAR;
 end else begin
  result:=NewNode(ntQUEST,Node,nil,nil,Kind);
 end;
end;

function TFLRE.IsStarNullable(Node:PFLRENode):boolean;
begin
 if assigned(Node) then begin
  case Node^.NodeType of
   ntSTAR:begin
    result:=Node^.Value=qkGREEDY;
   end;
   ntALT:begin
    result:=(IsStarNullable(Node^.Left) or IsStarNullable(Node^.Right)) or not (assigned(Node^.Left) and assigned(Node^.Right));
   end;
   ntCAT:begin
    result:=IsStarNullable(Node^.Left) and IsStarNullable(Node^.Right);
   end;
   else begin
    result:=false;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

function TFLRE.StarDenull(Node:PFLRENode):PFLRENode;
begin
 result:=Node;
 if IsStarNullable(result) then begin
  case result^.NodeType of
   ntSTAR:begin
    result:=result^.Left;
   end;
   ntCAT:begin
    result:=NewAlt(StarDenull(result^.Left),StarDenull(result^.Right));
{   result^.NodeType:=ntALT;
    result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);}
   end;
   ntALT:begin
    result:=NewAlt(StarDenull(result^.Left),StarDenull(result^.Right));
{   result^.Left:=StarDenull(result^.Left);
    result^.Right:=StarDenull(result^.Right);}
   end;
  end;
 end;
end;

function TFLRE.OptimizeNode(NodeEx:PPFLRENode):boolean;
 procedure ParseNodes(NodeList:TList;n:PFLRENode);
 begin
  while assigned(n) do begin
   if n^.NodeType=ntCAT then begin
    if assigned(n^.Right) then begin
     if n^.Right^.NodeType=ntCAT then begin
      ParseNodes(NodeList,n^.Right);
     end else begin
      NodeList.Add(n^.Right);
     end;
    end;
    if assigned(n^.Left) then begin
     if n^.Left^.NodeType=ntCAT then begin
      n:=n^.Left;
     end else begin
      NodeList.Add(n^.Left);
      break;
     end;
    end else begin
     break;
    end;
   end else begin
    NodeList.Add(n);
    break;
   end;
  end;
 end;
var Node,l,r:PFLRENode;
    pr,pl:PPFLRENode;
    DoContinue,Optimized:boolean;
    NodeList:TList;
    NodeIndex:longint;
    c:TFLRECharClass;
begin
 result:=false;
 DoContinue:=true;
 while DoContinue and (assigned(NodeEx) and assigned(NodeEx^)) do begin
  DoContinue:=false;
  Node:=NodeEx^;
  if assigned(Node) then begin
   case Node^.NodeType of
    ntCHAR,ntANY,ntBOL,ntEOL,ntBOT,ntEOT,ntBRK,ntNBRK:begin
    end;
    ntPAREN,ntEXACT:begin
     NodeEx:=@Node^.Left;
     DoContinue:=true;
    end;
    ntPLUS:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntPLUS,ntSTAR])) and ((Node^.Left^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPLUS)) and ((Node^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntSTAR,ntQUEST])) and ((Node^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntSTAR:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and ((Node^.Left^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.Left^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntQUEST,ntSTAR])) and ((Node^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      if IsStarNullable(Node^.Left) then begin
       Node:=StarDenull(Node^.Left);
       result:=true;
      end;
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntQUEST:begin
     if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType=ntQUEST)) and ((Node^.Left^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntPAREN)) and (assigned(Node^.Left^.Left) and (Node^.Left^.Left^.NodeType in [ntSTAR,ntPLUS{,ntQUEST}])) and ((Node^.Left^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.Left^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType=ntQUEST)) and ((Node^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      DoContinue:=true;
      result:=true;
     end else if (assigned(Node^.Left) and (Node^.Left^.NodeType in [ntPLUS,ntSTAR])) and ((Node^.Left^.Value=qkGREEDY) and (Node^.Value=qkGREEDY)) then begin
      NodeEx^:=Node^.Left;
      NodeEx^^.NodeType:=ntSTAR;
      DoContinue:=true;
      result:=true;
     end else begin
      NodeEx:=@Node^.Left;
      DoContinue:=true;
     end;
    end;
    ntCAT:begin
     if assigned(Node^.Left) and assigned(Node^.Right) then begin
      if ((Node^.Left^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (Node^.Right^.NodeType=ntPLUS)) and AreNodesEqual(Node^.Left^.Left,Node^.Right^.Left) then begin
       NodeEx^:=Node^.Right;
       DoContinue:=true;
       result:=true;
      end else if ((Node^.Left^.NodeType in [ntSTAR,ntPLUS]) and (Node^.Right^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqual(Node^.Left^.Left,Node^.Right^.Left) then begin
       NodeEx^:=Node^.Left;
       DoContinue:=true;
       result:=true;
      end else begin
       NodeList:=TList.Create;
       try
        ParseNodes(NodeList,Node);
        Optimized:=false;
        DoContinue:=true;
        while DoContinue do begin
         DoContinue:=false;
         NodeIndex:=NodeList.Count-1;
         while NodeIndex>0 do begin
          l:=PFLRENode(NodeList[NodeIndex]);
          r:=PFLRENode(NodeList[NodeIndex-1]);
          if ((l^.NodeType in [ntSTAR,ntPLUS,ntQUEST]) and (r^.NodeType=ntPLUS)) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
           NodeList.Delete(NodeIndex);
           if NodeIndex>=NodeList.Count then begin
            NodeIndex:=NodeList.Count-1;
           end;
           DoContinue:=true;
           Optimized:=true;
          end else if ((l^.NodeType in [ntSTAR,ntPLUS]) and (r^.NodeType in [ntSTAR,ntQUEST])) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
           NodeList.Delete(NodeIndex-1);
           if NodeIndex>=NodeList.Count then begin
            NodeIndex:=NodeList.Count-1;
           end;
           DoContinue:=true;
           Optimized:=true;
          end else begin
           dec(NodeIndex);
          end;
         end;
        end;
        if Optimized and (NodeList.Count>0) then begin
         NodeEx^:=NodeList[NodeList.Count-1];
         for NodeIndex:=NodeList.Count-2 downto 0 do begin
          NodeEx^:=NewNode(ntCAT,NodeEx^,NodeList[NodeIndex],nil,0);
         end;
         DoContinue:=true;
         result:=true;
        end;
       finally
        FreeAndNil(NodeList);
       end;
       if not Optimized then begin
        if OptimizeNode(@Node^.Right) then begin
         result:=true;
        end;
        NodeEx:=@Node^.Left;
        DoContinue:=true;
       end;
      end;
     end else begin
      if assigned(Node^.Left) then begin
       NodeEx:=@Node^.Left;
      end else begin
       NodeEx:=@Node^.Right;
      end;
      DoContinue:=true;
     end;
    end;
    ntALT:begin
     if assigned(Node^.Left) and assigned(Node^.Right) then begin
      if (Node^.Left^.NodeType=ntCAT) and (Node^.Right^.NodeType=ntCAT) then begin
       pl:=@Node^.Left;
       l:=Node^.Left;
       while (assigned(l) and assigned(l^.Left)) and (l^.Left^.NodeType=ntCAT) do begin
        pl:=@l^.Left;
        l:=l^.Left;
       end;
       pr:=@Node^.Right;
       r:=Node^.Right;
       while (assigned(r) and assigned(r^.Left)) and (r^.Left^.NodeType=ntCAT) do begin
        pr:=@r^.Left;
        r:=r^.Left;
       end;
       if ((assigned(l^.Left) and assigned(l^.Right)) and (assigned(r^.Left) and assigned(r^.Right))) and AreNodesEqualSafe(l^.Left,r^.Left) then begin
        NodeEx^:=l;
        pl^:=l^.Right;
        pr^:=r^.Right;
        l^.Right:=Node;
        DoContinue:=true;
        result:=true;
       end;
      end else if AreNodesEqualSafe(Node^.Left,Node^.Right) then begin
       NodeEx^:=Node^.Left;
       DoContinue:=true;
       result:=true;
      end else if (Node^.Left^.NodeType=ntCHAR) and (Node^.Right^.NodeType=ntCHAR) then begin
       Node^.NodeType:=ntCHAR;
       Node^.CharClass:=Node^.Left^.CharClass+Node^.Right^.CharClass;
       Node^.Left:=nil;
       Node^.Right:=nil;
       DoContinue:=true;
       result:=true;
      end else begin
       if OptimizeNode(@Node^.Right) then begin
        result:=true;
       end;
       NodeEx:=@Node^.Left;
       DoContinue:=true;
      end;
     end else begin
      if assigned(Node^.Left) then begin
       NodeEx:=@Node^.Left;
      end else begin
       NodeEx:=@Node^.Right;
      end;
      DoContinue:=true;
     end;
    end;
   end;
  end;
 end;
end;

procedure TFLRE.Parse;
const AllCharClass:TFLRECharClass=[#0..#255];
var SourcePosition,SourceLength:longint;
    Source:ansistring;
 function Hex2Value(const c:ansichar):longword;
 begin
  case c of
   '0'..'9':begin
    result:=byte(ansichar(c))-byte(ansichar('0'))
   end;
   'a'..'f':begin
    result:=(byte(ansichar(c))-byte(ansichar('a')))+$a;
   end;
   'A'..'F':begin
    result:=(byte(ansichar(c))-byte(ansichar('A')))+$a;
   end;
   else begin
    result:=0;
   end;
  end;
 end;
 function ParseDisjunction:PFLRENode; forward;
 function GetCharClass(const CurrentChar:ansichar;out IsSingle:boolean;out SingleChar:ansichar):TFLRECharClass;
 begin
  case CurrentChar of
   '0':begin
    result:=[#0];
    IsSingle:=true;
    SingleChar:=#0;
   end;
   't':begin
    result:=[#9];
    IsSingle:=true;
    SingleChar:=#9;
   end;
   'n':begin
    result:=[#10];
    IsSingle:=true;
    SingleChar:=#10;
   end;
   'v':begin
    result:=[#11];
    IsSingle:=true;
    SingleChar:=#11;
   end;
   'f':begin
    result:=[#12];
    IsSingle:=true;
    SingleChar:=#12;
   end;
   'r':begin
    result:=[#13];
    IsSingle:=true;
    SingleChar:=#13;
   end;
   'a':begin
    result:=['a'..'z','A'..'Z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'A':begin
    result:=AllCharClass-['a'..'z','A'..'Z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'w':begin
    result:=['a'..'z','A'..'Z','0'..'9','_'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'W':begin
    result:=AllCharClass-['a'..'z','A'..'Z','0'..'9','_'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   's':begin
    result:=[#9,#10,#13,#32];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'S':begin
    result:=AllCharClass-[#9,#10,#13,#32];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'd':begin
    result:=['0'..'9'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'D':begin
    result:=AllCharClass-['0'..'9'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'h':begin
    result:=['0'..'9','a'..'f','A'..'F'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'H':begin
    result:=AllCharClass-['0'..'9','a'..'f','A'..'F'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'c':begin
    result:=[#0..#31,#127];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'C':begin
    result:=AllCharClass-[#0..#31,#127];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'p':begin
    result:=['!','"','#','%','&','''','(',')',';','<','=','>','?','[','\',']','*','+',',','-','.','/',':','^','_','{','|','}','~'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'P':begin
    result:=AllCharClass-['!','"','#','%','&','''','(',')',';','<','=','>','?','[','\',']','*','+',',','-','.','/',':','^','_','{','|','}','~'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'l':begin
    result:=['a'..'z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'L':begin
    result:=AllCharClass-['a'..'z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'u':begin
    result:=['A'..'Z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   'U':begin
    result:=AllCharClass-['A'..'Z'];
    IsSingle:=false;
    SingleChar:=#0;
   end;
   else begin
    result:=[CurrentChar];
    IsSingle:=true;
    SingleChar:=CurrentChar;
   end;
  end;
 end;
 function GetCharClassPerName(const Name:ansistring):TFLRECharClass;
 begin
  if Name='alnum' then begin
   result:=['a'..'z','A'..'Z','0'..'9'];
  end else if Name='alpha' then begin
   result:=['a'..'z','A'..'Z'];
  end else if Name='ascii' then begin
   result:=[#$00..#$7f];
  end else if Name='blank' then begin
   result:=[#9,#32];
  end else if Name='cntrl' then begin
   result:=[#$00..#$1f,#$7f];
  end else if Name='digits' then begin
   result:=['0'..'9'];
  end else if Name='graph' then begin
   result:=[#$21..#$7e];
  end else if Name='lower' then begin
   result:=['a'..'z'];
  end else if Name='print' then begin
   result:=[#$20..#$7e];
  end else if Name='punct' then begin
   result:=['!','"','#','$','%','&','''','(',')','*','+',
                       ',','\','-','.','/',':',';','<','=','>','?',
                       '@','[','\',']','^','_','`','{','|','}','~'];
  end else if Name='space' then begin
   result:=[#9,#10..#13,#32];
  end else if Name='upper' then begin
   result:=['A'..'Z'];
  end else if Name='word' then begin
   result:=['a'..'z','A'..'Z','0'..'9','_'];
  end else if Name='xdigit' then begin
   result:=['a'..'f','A'..'F','0'..'9'];
  end else begin
   result:=[];
   raise EFLRE.Create('Syntax error');
  end;
 end;
 procedure SkipFreeSpacingWhiteSpace;
 var TemporarySourcePosition:longint;
 begin
  if rfFREESPACING in Flags then begin
   if rfUTF8 in Flags then begin
    while SourcePosition<=SourceLength do begin
     TemporarySourcePosition:=SourcePosition;
     case UTF8CodeUnitGetCharAndIncFallback(Source,TemporarySourcePosition) of
      $0009..$000d,$0020,$00a0,$1680,$180e,$2000..$200b,$2028..$2029,$202f,$205f,$3000,$fffe,$feff:begin
       SourcePosition:=TemporarySourcePosition;
      end;
      ord('#'):begin
       SourcePosition:=TemporarySourcePosition;
       while SourcePosition<=SourceLength do begin
        case UTF8CodeUnitGetCharAndIncFallback(Source,TemporarySourcePosition) of
         $000a,$000d,$2028,$2029:begin
          SourcePosition:=TemporarySourcePosition;
          while SourcePosition<=SourceLength do begin
           case UTF8CodeUnitGetCharAndIncFallback(Source,TemporarySourcePosition) of
            $000a,$000d,$2028,$2029:begin
             SourcePosition:=TemporarySourcePosition;
            end;
            else begin
             break;
            end;
           end;
          end;
          break;
         end;
         else begin
          SourcePosition:=TemporarySourcePosition;
         end;
        end;
       end;
      end;
      else begin
       break;
      end;
     end;
    end;
   end else begin
    while SourcePosition<=SourceLength do begin
     case Source[SourcePosition] of
      #$09..#$0d,#$20,#$a0:begin
       inc(SourcePosition);
      end;
      '#':begin
       inc(SourcePosition);
       while SourcePosition<=SourceLength do begin
        case Source[SourcePosition] of
         #$0a,#$0d:begin
          inc(SourcePosition);
          while (SourcePosition<=SourceLength) and (Source[SourcePosition] in [#$0a,#$0d]) do begin
           inc(SourcePosition);
          end;
          break;
         end;
         else begin
          inc(SourcePosition);
         end;
        end;
       end;
      end;
      else begin
       break;
      end;
     end;
    end;
   end;
  end;
 end;
 function ParseAtom:PFLRENode;
 var Value,Index:longint;
     Negate,IsSingle,Done,IsNegative,First,Num:boolean;
     StartChar,EndChar:ansichar;
     CharClass:TFLRECharClass;
     OldFlags:TFLREFlags;
     Name,TemporaryString:ansistring;
     UnicodeChar,LowerCaseUnicodeChar,UpperCaseUnicodeChar:longword;
     TemporaryNodes:array[0..3] of PFLRENode;
 begin
  result:=nil;
  try
   repeat
    Done:=true;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '*','+','?',')',']','{','}','|':begin
       raise EFLRE.Create('Syntax error');
      end;
      '(':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        if SourcePosition<=SourceLength then begin
         case Source[SourcePosition] of
          '#':begin
           inc(SourcePosition);
           while (SourcePosition<=SourceLength) and (Source[SourcePosition]<>')') do begin
            inc(SourcePosition);
           end;
           Done:=false;
          end;
          '+','-','a'..'z':begin
           OldFlags:=Flags;
           IsNegative:=false;
           First:=true;
           Num:=false;
           while SourcePosition<=SourceLength do begin
            case Source[SourcePosition] of
             ')':begin
              break;
             end;
             '-':begin
              inc(SourcePosition);
              IsNegative:=true;
              Num:=First;
             end;
             '+':begin
              inc(SourcePosition);
              IsNegative:=false;
              Num:=First;
             end;
             'i':begin
              inc(SourcePosition);
              if IsNegative then begin
               Exclude(Flags,rfCASEINSENSITIVE);
              end else begin
               Include(Flags,rfCASEINSENSITIVE);
              end;
             end;
             'm':begin
              inc(SourcePosition);
              if IsNegative then begin
               Exclude(Flags,rfMULTILINE);
              end else begin
               Include(Flags,rfMULTILINE);
              end;
             end;
             's':begin
              inc(SourcePosition);
              if IsNegative then begin
               Exclude(Flags,rfSINGLELINE);
              end else begin
               Include(Flags,rfSINGLELINE);
              end;
             end;
             'x':begin
              inc(SourcePosition);
              if IsNegative then begin
               Exclude(Flags,rfFREESPACING);
              end else begin
               Include(Flags,rfFREESPACING);
              end;
             end;
             'p':begin
              inc(SourcePosition);
              if IsNegative then begin
               Exclude(Flags,rfLONGEST);
              end else begin
               Include(Flags,rfLONGEST);
              end;
             end;
             ':':begin
              inc(SourcePosition);
              result:=ParseDisjunction;
              Flags:=OldFlags;
              break;
             end;
             else begin
              inc(SourcePosition);
              raise EFLRE.Create('Syntax error');
             end;
            end;
           end;
          end;
          ':':begin
           inc(SourcePosition);
           result:=ParseDisjunction;
          end;
          '''':begin
           inc(SourcePosition);
           Name:='';
           while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
            Name:=Name+Source[SourcePosition];
            inc(SourcePosition);
           end;
           if (SourcePosition<=SourceLength) and (Source[SourcePosition]='''') then begin
            inc(SourcePosition);
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
           Value:=CountParens;
           inc(CountParens);
           NamedGroupStringIntegerPairHashMap.Add(Name,Value);
           if NamedGroupStringList.IndexOf(Name)<0 then begin
            NamedGroupStringList.Add(Name);
           end else begin
            raise EFLRE.Create('Duplicate named group');
           end;
           result:=NewNode(ntPAREN,ParseDisjunction,nil,nil,0);
           result^.Value:=Value;
          end;
          '<':begin
           inc(SourcePosition);
           if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['!','=']) then begin
            raise EFLRE.Create('Syntax error');
           end else begin
            Name:='';
            while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
             Name:=Name+Source[SourcePosition];
             inc(SourcePosition);
            end;
            if (SourcePosition<=SourceLength) and (Source[SourcePosition]='>') then begin
             inc(SourcePosition);
            end else begin
             raise EFLRE.Create('Syntax error');
            end;
            Value:=CountParens;
            inc(CountParens);
            NamedGroupStringIntegerPairHashMap.Add(Name,Value);
            if NamedGroupStringList.IndexOf(Name)<0 then begin
             NamedGroupStringList.Add(Name);
            end else begin
             raise EFLRE.Create('Duplicate named group');
            end;
            result:=NewNode(ntPAREN,ParseDisjunction,nil,nil,0);
            result^.Value:=Value;
           end;
          end;
          'P':begin
           inc(SourcePosition);
           if (SourcePosition<=SourceLength) and (Source[SourcePosition]='<') then begin
            inc(SourcePosition);
            Name:='';
            while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','A'..'Z','a'..'z','_']) do begin
             Name:=Name+Source[SourcePosition];
             inc(SourcePosition);
            end;
            if (SourcePosition<=SourceLength) and (Source[SourcePosition]='>') then begin
             inc(SourcePosition);
            end else begin
             raise EFLRE.Create('Syntax error');
            end;
            Value:=CountParens;
            inc(CountParens);
            NamedGroupStringIntegerPairHashMap.Add(Name,Value);
            if NamedGroupStringList.IndexOf(Name)<0 then begin
             NamedGroupStringList.Add(Name);
            end else begin
             raise EFLRE.Create('Duplicate named group');
            end;
            result:=NewNode(ntPAREN,ParseDisjunction,nil,nil,0);
            result^.Value:=Value;
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
          end;
          else begin
           raise EFLRE.Create('Syntax error');
          end;
         end;
        end else begin
         raise EFLRE.Create('Syntax error');
        end;
       end else begin
        Value:=CountParens;
        inc(CountParens);
        result:=NewNode(ntPAREN,ParseDisjunction,nil,nil,0);
        result^.Value:=Value;
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]=')') then begin
        inc(SourcePosition);
       end else begin
        raise EFLRE.Create('Syntax error');
       end;
      end;
      '\':begin
       inc(SourcePosition);
       if SourcePosition<=SourceLength then begin
        case Source[SourcePosition] of
         'b':begin
          result:=NewNode(ntBRK,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'B':begin
          result:=NewNode(ntNBRK,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'x','X':begin
          inc(SourcePosition);
          if ((SourcePosition+1)<=SourceLength) and
             (Source[SourcePosition+0] in ['0'..'9','a'..'f','A'..'F']) and
             (Source[SourcePosition+1] in ['0'..'9','a'..'f','A'..'F']) then begin
           result:=NewNode(ntCHAR,nil,nil,nil,0);
           result^.CharClass:=[ansichar(byte(longword((Hex2Value(Source[SourcePosition+0]) shl 8) or Hex2Value(Source[SourcePosition+1]))))];
           inc(SourcePosition,2);
          end else begin
           raise EFLRE.Create('Syntax error');
          end;
         end;
         else begin
          result:=NewNode(ntCHAR,nil,nil,nil,0);
          result^.CharClass:=GetCharClass(Source[SourcePosition],IsSingle,StartChar);
          inc(SourcePosition);
         end;
        end;
       end else begin
        raise EFLRE.Create('Syntax error');
       end;
      end;
      '.':begin
       if rfSINGLELINE in Flags then begin
        result:=NewNode(ntANY,nil,nil,nil,0);
       end else begin
        result:=NewNode(ntCHAR,nil,nil,nil,0);
        result^.CharClass:=AllCharClass-[#10,#13];
        inc(SourcePosition);
       end;
       inc(SourcePosition);
      end;
      '^':begin
       if rfMULTILINE in Flags then begin
        result:=NewNode(ntBOL,nil,nil,nil,0);
       end else begin
        result:=NewNode(ntBOT,nil,nil,nil,0);
       end;
       inc(SourcePosition);
      end;
      '$':begin
       if rfMULTILINE in Flags then begin
        result:=NewNode(ntEOL,nil,nil,nil,0);
       end else begin
        result:=NewNode(ntEOT,nil,nil,nil,0);
       end;
       inc(SourcePosition);
      end;
      '[':begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
        inc(SourcePosition);
        Name:='';
        while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['a'..'z']) do begin
         Name:=Name+Source[SourcePosition];
         inc(SourcePosition);
        end;
        if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]=':') and (Source[SourcePosition+1]=']')) then begin
         inc(SourcePosition,2);
         result:=NewNode(ntCHAR,nil,nil,nil,0);
         result^.CharClass:=GetCharClassPerName(Name);
        end else begin
         raise EFLRE.Create('Syntax error');
        end;
       end else begin
        result:=NewNode(ntCHAR,nil,nil,nil,0);
        result^.CharClass:=[];
        if (SourcePosition<=SourceLength) and (Source[SourcePosition]='^') then begin
         inc(SourcePosition);
         Negate:=true;
        end else begin
         Negate:=false;
        end;
        StartChar:=#0;
        EndChar:=#0;
        while SourcePosition<=SourceLength do begin
         case Source[SourcePosition] of
          ']':begin
           break;
          end;
          '^':begin
           raise EFLRE.Create('Syntax error');
          end;
          '\':begin
           inc(SourcePosition);
           if SourcePosition<=SourceLength then begin
            case Source[SourcePosition] of
             'x','X':begin
              inc(SourcePosition);
              if ((SourcePosition+1)<=SourceLength) and
                 (Source[SourcePosition+0] in ['0'..'9','a'..'f','A'..'F']) and
                 (Source[SourcePosition+1] in ['0'..'9','a'..'f','A'..'F']) then begin
               StartChar:=ansichar(byte(longword((Hex2Value(Source[SourcePosition+0]) shl 8) or Hex2Value(Source[SourcePosition+1]))));
               result^.CharClass:=result^.CharClass+[ansichar(byte(longword((Hex2Value(Source[SourcePosition+0]) shl 8) or Hex2Value(Source[SourcePosition+1]))))];
               inc(SourcePosition,2);
              end else begin
               raise EFLRE.Create('Syntax error');
              end;
             end;
             else begin
              IsSingle:=false;
              result^.CharClass:=result^.CharClass+GetCharClass(Source[SourcePosition],IsSingle,StartChar);
              inc(SourcePosition);
              if not IsSingle then begin
               continue;
              end;
             end;
            end;
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
          end;
          '[':begin
           inc(SourcePosition);
           if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
            inc(SourcePosition);
            Name:='';
            while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['a'..'z']) do begin
             Name:=Name+Source[SourcePosition];
             inc(SourcePosition);
            end;
            if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]=':') and (Source[SourcePosition+1]=']')) then begin
             inc(SourcePosition,2);
             result^.CharClass:=result^.CharClass+GetCharClassPerName(Name);
             continue;
            end else begin
             raise EFLRE.Create('Syntax error');
            end;
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
          end;
          '|','*','+','?','(',')','{','}',':','$':begin
           raise EFLRE.Create('Syntax error');
          end;
          else begin
           StartChar:=Source[SourcePosition];
           inc(SourcePosition);
          end;
         end;
         if (SourcePosition<=SourceLength) and (Source[SourcePosition]='-') then begin
          inc(SourcePosition);
          case Source[SourcePosition] of
           '\':begin
            inc(SourcePosition);
            if SourcePosition<=SourceLength then begin
             case Source[SourcePosition] of
              'x','X':begin
               inc(SourcePosition);
               if ((SourcePosition+1)<=SourceLength) and
                  (Source[SourcePosition+0] in ['0'..'9','a'..'f','A'..'F']) and
                  (Source[SourcePosition+1] in ['0'..'9','a'..'f','A'..'F']) then begin
                EndChar:=ansichar(byte(longword((Hex2Value(Source[SourcePosition+0]) shl 8) or Hex2Value(Source[SourcePosition+1]))));
                inc(SourcePosition,2);
               end else begin
                raise EFLRE.Create('Syntax error');
               end;
              end;
              else begin
               IsSingle:=false;
               result^.CharClass:=result^.CharClass+GetCharClass(Source[SourcePosition],IsSingle,EndChar);
               inc(SourcePosition);
               if not IsSingle then begin
                raise EFLRE.Create('Syntax error');
               end;
              end;
             end;
            end;
           end;
           '|','*','+','?','(',')','[',']','{','}',':','.','^','$':begin
            raise EFLRE.Create('Syntax error');
           end;
           else begin
            EndChar:=Source[SourcePosition];
            inc(SourcePosition);
           end;
          end;
          if EndChar<StartChar then begin
           raise EFLRE.Create('Syntax error');
          end else begin
           result^.CharClass:=result^.CharClass+[StartChar..EndChar];
          end;
         end else begin
          Include(result^.CharClass,StartChar);
         end;
        end;
        if (SourcePosition<=SourceLength) and (Source[SourcePosition]=']') then begin
         inc(SourcePosition);
         if Negate then begin
          result^.CharClass:=AllCharClass-result^.CharClass;
         end;
         if result^.CharClass=[] then begin
          raise EFLRE.Create('Syntax error');
         end;
        end else begin
         raise EFLRE.Create('Syntax error');
        end;
       end;
      end;
      #128..#255:begin
       if rfUTF8 in Flags then begin
        UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
        LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
        UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
        if (rfCASEINSENSITIVE in Flags) and (LowerCaseUnicodeChar<>UpperCaseUnicodeChar) then begin
         begin
          TemporaryNodes[0]:=nil;
          TemporaryString:=UTF32CharToUTF8(LowerCaseUnicodeChar);
          for Index:=1 to length(TemporaryString) do begin
           TemporaryNodes[3]:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNodes[3]^.CharClass:=[TemporaryString[Index]];
           if assigned(TemporaryNodes[0]) then begin
            TemporaryNodes[0]:=Concat(TemporaryNodes[0],TemporaryNodes[3]);
           end else begin
            TemporaryNodes[0]:=TemporaryNodes[3];
           end;
          end;
         end;
         begin
          TemporaryNodes[1]:=nil;
          TemporaryString:=UTF32CharToUTF8(UpperCaseUnicodeChar);
          for Index:=1 to length(TemporaryString) do begin
           TemporaryNodes[3]:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNodes[3]^.CharClass:=[TemporaryString[Index]];
           if assigned(TemporaryNodes[0]) then begin
            TemporaryNodes[1]:=Concat(TemporaryNodes[0],TemporaryNodes[3]);
           end else begin
            TemporaryNodes[1]:=TemporaryNodes[3];
           end;
          end;
         end;
         result:=NewAlt(TemporaryNodes[0],TemporaryNodes[1]);
        end else begin
         TemporaryString:=UTF32CharToUTF8(UnicodeChar);
         for Index:=1 to length(TemporaryString) do begin
          result:=NewNode(ntCHAR,nil,nil,nil,0);
          result^.CharClass:=[TemporaryString[Index]];
         end;
        end;
       end else begin
        result:=NewNode(ntCHAR,nil,nil,nil,0);
        result^.CharClass:=[Source[SourcePosition]];
        inc(SourcePosition);
       end;
      end;
      else begin
       if (rfCASEINSENSITIVE in Flags) and (Source[SourcePosition] in ['a'..'z','A'..'Z']) then begin
        UnicodeChar:=byte(ansichar(Source[SourcePosition]));
        LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
        UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
        result:=NewNode(ntCHAR,nil,nil,nil,0);
        result^.CharClass:=[ansichar(byte(LowerCaseUnicodeChar)),ansichar(byte(UpperCaseUnicodeChar))];
       end else begin
        result:=NewNode(ntCHAR,nil,nil,nil,0);
        result^.CharClass:=[Source[SourcePosition]];
        inc(SourcePosition);
       end;
      end;
     end;
    end else begin
     raise EFLRE.Create('Syntax error');
    end;
    if Done or assigned(result) or ((SourcePosition<=SourceLength) and (Source[SourcePosition] in ['|',')'])) then begin
     break;
    end else begin
     SkipFreeSpacingWhiteSpace;
    end;
   until false;
  except
   raise;
  end;
 end;
 function ParseTerm:PFLRENode;
 var MinCount,MaxCount:longint;
 begin
  result:=nil;
  try
   if SourcePosition<=SourceLength then begin
    result:=ParseAtom;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '*':begin
       inc(SourcePosition);
       if not assigned(result) then begin
        raise EFLRE.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewStar(result,qkLAZY);
        if rfLONGEST in Flags then begin
         raise EFLRE.Create('Syntax error');
        end;
       end else begin
        if rfLONGEST in Flags then begin
         result:=NewStar(result,qkLAZY);
        end else begin
         result:=NewStar(result,qkGREEDY);
        end;
       end;
      end;
      '+':begin
       inc(SourcePosition);
       if not assigned(result) then begin
        raise EFLRE.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewPlus(result,qkLAZY);
        if rfLONGEST in Flags then begin
         raise EFLRE.Create('Syntax error');
        end;
       end else begin
        if rfLONGEST in Flags then begin
         result:=NewPlus(result,qkLAZY);
        end else begin
         result:=NewPlus(result,qkGREEDY);
        end;
       end;
      end;
      '?':begin
       inc(SourcePosition);
       if not assigned(result) then begin
        raise EFLRE.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
        inc(SourcePosition);
        result:=NewQuest(result,qkLAZY);
        if rfLONGEST in Flags then begin
         raise EFLRE.Create('Syntax error');
        end;
       end else begin
        if rfLONGEST in Flags then begin
         result:=NewQuest(result,qkLAZY);
        end else begin
         result:=NewQuest(result,qkGREEDY);
        end;
       end;
      end;
      '{':begin
       inc(SourcePosition);
       if not assigned(result) then begin
        raise EFLRE.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) then begin
        MinCount:=0;
        while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) do begin
         MinCount:=(MinCount*10)+(byte(ansichar(Source[SourcePosition]))-byte(ansichar('0')));
         inc(SourcePosition);
        end;
       end else begin
        MinCount:=-1;
        raise EFLRE.Create('Syntax error');
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]=',') then begin
        inc(SourcePosition);
        if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) then begin
         MaxCount:=0;
         while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) do begin
          MaxCount:=(MaxCount*10)+(byte(ansichar(Source[SourcePosition]))-byte(ansichar('0')));
          inc(SourcePosition);
         end;
         if MinCount>MaxCount then begin
          raise EFLRE.Create('Syntax error');
         end;
        end else begin
         MaxCount:=-1;
        end;
       end else begin
        MaxCount:=MinCount;
       end;
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='}') then begin
        inc(SourcePosition);
        result:=NewNode(ntEXACT,result,nil,nil,0);
        if (SourcePosition<=SourceLength) and (Source[SourcePosition]='?') then begin
         inc(SourcePosition);
         result^.Value:=1;
         if rfLONGEST in Flags then begin
          raise EFLRE.Create('Syntax error');
         end;
        end else begin
         if rfLONGEST in Flags then begin
          result^.Value:=1;
         end else begin
          result^.Value:=0;
         end;
        end;
        result^.MinCount:=MinCount;
        result^.MaxCount:=MaxCount;
       end else begin
        raise EFLRE.Create('Syntax error');
       end;
      end;
     end;
    end;
   end;
  except
   raise;
  end;
 end;
 function ParseAlternative:PFLRENode;
 var Node:PFLRENode;
 begin
  result:=nil;
  try
   SkipFreeSpacingWhiteSpace;
   while SourcePosition<=SourceLength do begin
    Node:=ParseTerm;
    SkipFreeSpacingWhiteSpace;
    if assigned(result) then begin
     result:=Concat(result,Node);
    end else begin
     result:=Node;
    end;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '|',')':begin
       break;
      end;
     end;
    end else begin
     break;
    end;
   end;
  except
   raise;
  end;
 end;
 function ParseDisjunction:PFLRENode;
 var Node:PFLRENode;
 begin
  result:=nil;
  try
   SkipFreeSpacingWhiteSpace;
   while SourcePosition<=SourceLength do begin
    Node:=ParseAlternative;
    SkipFreeSpacingWhiteSpace;
    if assigned(result) then begin
     result:=NewAlt(result,Node);
    end else begin
     result:=Node;
    end;
    if SourcePosition<=SourceLength then begin
     case Source[SourcePosition] of
      '|':begin
       inc(SourcePosition);
      end;
      ')':begin
       break;
      end;
     end;
    end else begin
     break;
    end;
   end;
  except
   raise;
  end;
 end;
var Counter:longint;
    CurrentChar:ansichar;
    Node:PFLRENode;
begin
 Source:=RegularExpression;
 SourcePosition:=1;
 SourceLength:=length(Source);
 if SourcePosition<=SourceLength then begin
  CountParens:=1;
  AnchoredRootNode:=NewNode(ntPAREN,ParseDisjunction,nil,nil,0);
  while assigned(AnchoredRootNode) and OptimizeNode(@AnchoredRootNode) do begin
  end;
  if rfLONGEST in Flags then begin
   UnanchoredRootNode:=NewNode(ntCAT,NewNode(ntSTAR,NewNode(ntANY,nil,nil,nil,0),nil,nil,qkGREEDY),AnchoredRootNode,nil,0);
  end else begin
   UnanchoredRootNode:=NewNode(ntCAT,NewNode(ntSTAR,NewNode(ntANY,nil,nil,nil,0),nil,nil,qkLAZY),AnchoredRootNode,nil,0);
  end;
  DFANeedVerification:=false;
  BeginningAnchor:=false;
  for Counter:=0 to Nodes.Count-1 do begin
   Node:=Nodes[Counter];
   if Node^.NodeType in [ntBOL,ntEOL,ntBOT,ntEOT,ntBRK,ntNBRK] then begin
    DFANeedVerification:=true;
    if Node^.NodeType=ntBOT then begin
     BeginningAnchor:=true;
    end;
    break;
   end;
  end;
  if rfCASEINSENSITIVE in Flags then begin
   for Counter:=0 to Nodes.Count-1 do begin
    Node:=Nodes[Counter];
    if Node^.NodeType=ntCHAR then begin
     for CurrentChar:=ansichar('a') to ansichar('z') do begin
      if CurrentChar in Node^.CharClass then begin
       Include(Node^.CharClass,ansichar(byte(byte(ansichar(CurrentChar))+(byte(ansichar('A'))-byte(ansichar('a'))))));
      end;
     end;
     for CurrentChar:=ansichar('A') to ansichar('Z') do begin
      if CurrentChar in Node^.CharClass then begin
       Include(Node^.CharClass,ansichar(byte(byte(ansichar(CurrentChar))+(byte(ansichar('a'))-byte(ansichar('A'))))));
      end;
     end;
    end;
   end;
  end;
  if SourcePosition<=SourceLength then begin
   raise EFLRE.Create('Syntax error');
  end;
 end;
end;

procedure TFLRE.Compile;
 procedure GenerateInstructions(var Instructions:TFLREInstructions;var CountInstructions:longint;const Reversed:boolean);
  function NewInstruction(Opcode:longword):longint;
  begin
   result:=CountInstructions;
   inc(CountInstructions);
   if CountInstructions>length(Instructions) then begin
    SetLength(Instructions,CountInstructions*2);
   end;
   Instructions[result].IndexAndOpcode:=(longword(result) shl 8) or (Opcode and $ff);
   Instructions[result].Next:=pointer(ptrint(-1));
   Instructions[result].OtherNext:=pointer(ptrint(-1));
  end;
  procedure Emit(Node:PFLRENode);
  var i0,i1,Counter,Count,Index:longint;
      Last:array of longint;
      CurrentChar,SingleChar:ansichar;
  begin
   while assigned(Node) do begin
    case Node^.NodeType of
     ntALT:begin
      i0:=NewInstruction(opSPLIT);
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
      Emit(Node^.Left);
      i1:=NewInstruction(opJMP);
      Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
      Emit(Node^.Right);
      Instructions[i1].Next:=pointer(ptrint(CountInstructions));
     end;
     ntCAT:begin
      if Reversed then begin
       Emit(Node^.Right);
       Node:=Node^.Left;
      end else begin
       Emit(Node^.Left);
       Node:=Node^.Right;
      end;
      continue;
     end;
     ntCHAR:begin
      SingleChar:=#0;
      Count:=0;
      for CurrentChar:=#0 to #255 do begin
       if CurrentChar in Node^.CharClass then begin
        if Count=0 then begin
         SingleChar:=CurrentChar;
         inc(Count);
        end else begin
         inc(Count);
         break;
        end;
       end;
      end;
      if Count=1 then begin
       i0:=NewInstruction(opSINGLECHAR);
       Instructions[i0].Value:=byte(ansichar(SingleChar));
      end else begin
       i0:=NewInstruction(opCHAR);
       Index:=-1;
       for Counter:=0 to CountCharClasses-1 do begin
        if CharClasses[Counter]^=Node^.CharClass then begin
         Index:=Counter;
         break;
        end;
       end;
       if Index<0 then begin
        Index:=CountCharClasses;
        inc(CountCharClasses);
        if CountCharClasses>length(CharClasses) then begin
         SetLength(CharClasses,CountCharClasses*2);
        end;
        GetMem(CharClasses[Index],SizeOf(TFLRECharClass));
        CharClasses[Index]^:=Node^.CharClass;
       end;
       Instructions[i0].Value:=ptruint(pointer(CharClasses[Index]));
      end;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntANY:begin
      i0:=NewInstruction(opANY);
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntPAREN:begin
      i0:=NewInstruction(opSAVE);
      Instructions[i0].Value:=Node^.Value shl 1;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
      Emit(Node^.Left);
      i0:=NewInstruction(opSAVE);
      Instructions[i0].Value:=(Node^.Value shl 1) or 1;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntQUEST:begin
      i0:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
       Emit(Node^.Left);
       Instructions[i0].Next:=pointer(ptrint(CountInstructions));
      end else begin
       // Greedy
       Instructions[i0].Next:=pointer(ptrint(CountInstructions));
       Emit(Node^.Left);
       Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
      end;
     end;
     ntSTAR:begin
      i0:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
       Emit(Node^.Left);
       i1:=NewInstruction(opJMP);
       Instructions[i1].Next:=pointer(ptrint(i0));
       Instructions[i0].Next:=pointer(ptrint(CountInstructions));
      end else begin
       // Greedy
       Instructions[i0].Next:=pointer(ptrint(CountInstructions));
       Emit(Node^.Left);
       i1:=NewInstruction(opJMP);
       Instructions[i1].Next:=pointer(ptrint(i0));
       Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
      end;
     end;
     ntPLUS:begin
      i0:=CountInstructions;
      Emit(Node^.Left);
      i1:=NewInstruction(opSPLIT);
      if Node^.Value<>0 then begin
       // Non-greedy
       Instructions[i1].OtherNext:=pointer(ptrint(i0));
       Instructions[i1].Next:=pointer(ptrint(CountInstructions));
      end else begin
       // Greedy
       Instructions[i1].Next:=pointer(ptrint(i0));
       Instructions[i1].OtherNext:=pointer(ptrint(CountInstructions));
      end;
     end;
     ntEXACT:begin
      if (Node^.MinCount=0) and (Node^.MaxCount=0) then begin
       // nothing
      end else if (Node^.MinCount=0) and (Node^.MaxCount=1) then begin
       i0:=NewInstruction(opSPLIT);
       if Node^.Value<>0 then begin
        // Non-greedy
        Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
        Emit(Node^.Left);
        Instructions[i0].Next:=pointer(ptrint(CountInstructions));
       end else begin
        // Greedy
        Instructions[i0].Next:=pointer(ptrint(CountInstructions));
        Emit(Node^.Left);
        Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
       end;
      end else if Node^.MaxCount<0 then begin
       if Node^.MinCount>0 then begin
        // Infinity with minimum connt
        for Counter:=1 to Node^.MinCount-1 do begin
         Emit(Node^.Left);
        end;
        i0:=CountInstructions;
        Emit(Node^.Left);
        i1:=NewInstruction(opSPLIT);
        if Node^.Value<>0 then begin
         // Non-greedy
         Instructions[i1].OtherNext:=pointer(ptrint(i0));
         Instructions[i1].Next:=pointer(ptrint(CountInstructions));
        end else begin
         // Greedy
         Instructions[i1].Next:=pointer(ptrint(i0));
         Instructions[i1].OtherNext:=pointer(ptrint(CountInstructions));
        end;
       end else begin
        // Infinity without minimum connt
        i0:=NewInstruction(opSPLIT);
        if Node^.Value<>0 then begin
         // Non-greedy
         Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
         Emit(Node^.Left);
         i1:=NewInstruction(opJMP);
         Instructions[i1].Next:=pointer(ptrint(i0));
         Instructions[i0].Next:=pointer(ptrint(CountInstructions));
        end else begin
         // Greedy
         Instructions[i0].Next:=pointer(ptrint(CountInstructions));
         Emit(Node^.Left);
         i1:=NewInstruction(opJMP);
         Instructions[i1].Next:=pointer(ptrint(i0));
         Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
        end;
       end;
      end else begin
       for Counter:=1 to Node^.MinCount do begin
        Emit(Node^.Left);
       end;
       if Node^.MinCount<Node^.MaxCount then begin
        if (Node^.MaxCount-Node^.MinCount)<1024 then begin
         SetLength(Last,Node^.MaxCount-Node^.MinCount);
         try
          for Counter:=Node^.MinCount to Node^.MaxCount-1 do begin
           i0:=NewInstruction(opSPLIT);
           Last[Counter-Node^.MinCount]:=i0;
           if Node^.Value<>0 then begin
            // Non-greedy
            Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
           end else begin
            // Greedy
            Instructions[i0].Next:=pointer(ptrint(CountInstructions));
           end;
           Emit(Node^.Left);
          end;
          for Counter:=Node^.MaxCount-1 downto Node^.MinCount do begin
           i0:=Last[Counter-Node^.MinCount];
           if Node^.Value<>0 then begin
            // Non-greedy
            Instructions[i0].Next:=pointer(ptrint(CountInstructions));
           end else begin
            // Greedy
            Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
           end;
          end;
         finally
          SetLength(Last,0);
         end;
        end else begin
         for Counter:=Node^.MinCount to Node^.MaxCount-1 do begin
          i0:=NewInstruction(opSPLIT);
          if Node^.Value<>0 then begin
           // Non-greedy
           Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
           Emit(Node^.Left);
           Instructions[i0].Next:=pointer(ptrint(CountInstructions));
          end else begin
           // Greedy
           Instructions[i0].Next:=pointer(ptrint(CountInstructions));
           Emit(Node^.Left);
           Instructions[i0].OtherNext:=pointer(ptrint(CountInstructions));
          end;
         end;
        end;
       end;
      end;
     end;
     ntBOL:begin
      if Reversed then begin
       i0:=NewInstruction(opEOL);
      end else begin
       i0:=NewInstruction(opBOL);
      end;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntEOL:begin
      if Reversed then begin
       i0:=NewInstruction(opBOL);
      end else begin
       i0:=NewInstruction(opEOL);
      end;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntBOT:begin
      if Reversed then begin
       i0:=NewInstruction(opEOT);
      end else begin
       i0:=NewInstruction(opBOT);
      end;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntEOT:begin
      if Reversed then begin
       i0:=NewInstruction(opBOT);
      end else begin
       i0:=NewInstruction(opEOT);
      end;
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntBRK:begin
      i0:=NewInstruction(opBRK);
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     ntNBRK:begin
      i0:=NewInstruction(opNBRK);
      Instructions[i0].Next:=pointer(ptrint(CountInstructions));
     end;
     else begin
      raise EFLRE.Create('Internal error');
     end;
    end;
    break;
   end;
  end;
 var Counter:longint;
     Instruction:PFLREInstruction;
 begin
  SetLength(Instructions,4096);
  CountInstructions:=0;
  try
   try
    if Reversed then begin
     Emit(AnchoredRootNode);
    end else begin
     Emit(UnanchoredRootNode);
    end;
   except
    CountInstructions:=0;
    raise;
   end;
   NewInstruction(opMATCH);
  finally
   SetLength(Instructions,CountInstructions);
   for Counter:=0 to CountInstructions-1 do begin
    Instruction:=@Instructions[Counter];
    if Instruction^.Next<>pointer(ptrint(-1)) then begin
     Instruction^.Next:=@Instructions[ptrint(Instruction^.Next)];
    end else begin
     Instruction^.Next:=nil;
    end;
    if Instruction^.OtherNext<>pointer(ptrint(-1)) then begin
     Instruction^.OtherNext:=@Instructions[ptrint(Instruction^.OtherNext)];
    end else begin
     Instruction^.OtherNext:=nil;
    end;
   end;
   if Reversed then begin
    ReversedStartInstruction:=@Instructions[0];
   end else begin
    AnchoredStartInstruction:=@Instructions[0];
    UnanchoredStartInstruction:=@Instructions[0];
    for Counter:=0 to CountInstructions-1 do begin
     Instruction:=@Instructions[Counter];
     if ((Instruction^.IndexAndOpcode and $ff)=opSAVE) and (Instruction^.Value=0) then begin
      AnchoredStartInstruction:=Instruction;
      break;
     end;
    end;
   end;
  end;
 end;
begin
 GenerateInstructions(ForwardInstructions,CountForwardInstructions,false);
 GenerateInstructions(BackwardInstructions,CountBackwardInstructions,true);
end;

procedure TFLRE.CompilePrefix;
type TStackItem=record
      Node:PFLRENode;
      Argument:ptrint;
     end;
var Node:PFLRENode;
    Argument:ptrint;
    StackPointer,Counter:longint;
    NodeStrings:array of ansistring;
    Stack:array of TStackItem;
    Stop,First,IsSingle:boolean;
    SingleChar,CurrentChar:ansichar;
begin
 NodeStrings:=nil;
 Stack:=nil;
 FixedString:='';
 FixedStringIsWholeRegExp:=false;
 try
  SetLength(NodeStrings,Nodes.Count);
  StackPointer:=Nodes.Count;
  for Counter:=0 to Nodes.Count-1 do begin
   Node:=Nodes[Counter];
   if assigned(Node) then begin
    case Node^.NodeType of
     ntCAT:begin
      inc(StackPointer,2);
     end;
    end;
   end;
  end;
  SetLength(Stack,StackPointer+1);
  for Counter:=0 to length(NodeStrings)-1 do begin
   NodeStrings[Counter]:='';
  end;
  StackPointer:=0;
  if assigned(UnanchoredRootNode) then begin
   Stack[StackPointer].Node:=UnanchoredRootNode;
   Stack[StackPointer].Argument:=0;
   inc(StackPointer);
   FixedStringIsWholeRegExp:=true;
  end;
  Stop:=false;
  while StackPointer>0 do begin
   dec(StackPointer);
   Node:=Stack[StackPointer].Node;
   Argument:=Stack[StackPointer].Argument;
   while assigned(Node) do begin
    case Node^.NodeType of
     ntPAREN:begin
      case Argument of
       1:begin
        if assigned(Node^.Left) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index];
        end;
       end;
       else begin
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end;
       end;
      end;
     end;
     ntCAT:begin
      case Argument of
       2:begin
        if assigned(Node^.Left) and assigned(Node^.Right) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index]+NodeStrings[Node^.Right^.Index];
        end else if assigned(Node^.Left) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index];
        end else if assigned(Node^.Right) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Right^.Index];
        end;
       end;
       1:begin
        if assigned(Node^.Right) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=2;
         inc(StackPointer);
         Node:=Node^.Right;
         Argument:=0;
        end else begin
         Argument:=2;
        end;
        continue;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end else begin
         if assigned(Node^.Right) and not Stop then begin
          Stack[StackPointer].Node:=Node;
          Stack[StackPointer].Argument:=2;
          inc(StackPointer);
          Node:=Node^.Right;
          Argument:=0;
          continue;
         end;
        end;
       end;
      end;
     end;
     ntEXACT:begin
      case Argument of
       1:begin
        if assigned(Node^.Left) then begin
         for Counter:=1 to Node^.MinCount do begin
          NodeStrings[Node^.Index]:=NodeStrings[Node^.Index]+NodeStrings[Node^.Left^.Index];
         end;
        end;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end;
       end;
      end;
     end;
     ntPLUS:begin
      case Argument of
       1:begin
        if assigned(Node^.Left) then begin
         NodeStrings[Node^.Index]:=NodeStrings[Node^.Left^.Index];
        end;
        FixedStringIsWholeRegExp:=false;
        Stop:=true;
       end;
       else {0:}begin
        NodeStrings[Node^.Index]:='';
        if assigned(Node^.Left) and not Stop then begin
         Stack[StackPointer].Node:=Node;
         Stack[StackPointer].Argument:=1;
         inc(StackPointer);
         Node:=Node^.Left;
         Argument:=0;
         continue;
        end;
       end;
      end;
     end;
     ntCHAR:begin
      First:=true;
      IsSingle:=false;
      SingleChar:=#0;
      for CurrentChar:=#0 to #255 do begin
       if CurrentChar in Node^.CharClass then begin
        if First then begin
         IsSingle:=true;
         First:=false;
         SingleChar:=CurrentChar;
        end else begin
         IsSingle:=false;
        end;
       end;
      end;
      if IsSingle then begin
       NodeStrings[Node^.Index]:=SingleChar;
      end else begin
       NodeStrings[Node^.Index]:='';
       FixedStringIsWholeRegExp:=false;
       Stop:=true;
      end;
     end;
     ntBOL,ntEOL,ntBOT,ntEOT,ntBRK,ntNBRK:begin
      // No-op instruction here, so don't stop but mark it as non-pure-string-literal regular expression
      FixedStringIsWholeRegExp:=false;
     end;
     else begin
      FixedStringIsWholeRegExp:=false;
      Stop:=true;
     end;
    end;
    break;
   end;
  end;
  if assigned(UnanchoredRootNode) then begin
   FixedString:=NodeStrings[UnanchoredRootNode^.Index];
  end else begin
   FixedString:='';
  end;
  FixedStringIsWholeRegExp:=FixedStringIsWholeRegExp and (length(FixedString)>0);
 finally
  SetLength(NodeStrings,0);
  SetLength(Stack,0);
 end;
end;

procedure TFLRE.CompileFixedStringSearch;
var c:ansichar;
    i,j,k:integer;
    HasMatch:boolean;
begin
 FixedStringLength:=length(FixedString);
 if FixedStringLength>1 then begin
  if FixedStringLength<32 then begin
   for c:=low(ansichar) to high(ansichar) do begin
    FixedStringPatternBitMasks[c]:=0;
   end;
   for i:=1 to FixedStringLength do begin
    FixedStringPatternBitMasks[FixedString[i]]:=FixedStringPatternBitMasks[FixedString[i]] or (1 shl (i-1));
   end;
  end else begin
   for c:=low(ansichar) to high(ansichar) do begin
    FixedStringBoyerMooreSkip[c]:=FixedStringLength;
   end;
   for i:=1 to FixedStringLength do begin
    FixedStringBoyerMooreSkip[FixedString[i]]:=((FixedStringLength-(i-1))-1);
   end;
   SetLength(FixedStringBoyerMooreNext,FixedStringLength+1);
   for j:=0 to FixedStringLength do begin
    i:=FixedStringLength-1;
    while i>=1 do begin
     HasMatch:=true;
     for k:=1 to j do begin
      if (i-k)<0 then begin
       break;
      end;
      if FixedString[(FixedStringLength-k)+1]<>FixedString[(i-k)+1] then begin
       HasMatch:=false;
       break;
      end;
     end;
     if HasMatch then begin
      break;
     end;
     dec(i);
    end;
    FixedStringBoyerMooreNext[j]:=FixedStringLength-i;
   end;
  end;
 end;
end;

procedure TFLRE.CompilePrefixCharClasses;
var CurrentPosition:longint;
 procedure AddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction);
 begin
  while assigned(Instruction) do begin
   if Instruction^.Generation=Generation then begin
    break;
   end else begin
    Instruction^.Generation:=Generation;
    case Instruction^.IndexAndOpcode and $ff of
     opJMP:begin
      if (CurrentPosition=0) and ((Instruction^.Next^.IndexAndOpcode shr 8)<=(Instruction^.IndexAndOpcode shr 8)) then begin
       BeginningJump:=true;
      end;
      Instruction:=Instruction^.Next;
      continue;
     end;
     opSPLIT:begin
      if CurrentPosition=0 then begin
       BeginningSplit:=true;
      end;
      AddThread(ThreadList,Instruction^.Next);
      Instruction:=Instruction^.OtherNext;
      continue;
     end;
     opSAVE,opBOL,opEOL,opBOT,opEOT,opBRK,opNBRK:begin
      Instruction:=Instruction^.Next;
      continue;
     end;
     else begin
      ThreadList^.Threads[ThreadList^.Count].Instruction:=Instruction;
      inc(ThreadList^.Count);
      break;
     end;
    end;
   end;
  end;
 end;
 procedure CompilePrefixPattern;
 var CurrentPosition:longint;
     CurrentChar:ansichar;
 begin
  for CurrentChar:=#0 to #255 do begin
   PrefixPatternBitMasks[CurrentChar]:=0;
  end;
  for CurrentPosition:=0 to CountPrefixCharClasses-1 do begin
   for CurrentChar:=#0 to #255 do begin
    if CurrentChar in PrefixCharClasses[CurrentPosition] then begin
     PrefixPatternBitMasks[CurrentChar]:=PrefixPatternBitMasks[CurrentChar] or longword(longword(1) shl CurrentPosition);
    end;
   end;
  end;
 end;
var ThreadIndex,Count:longint;
    CurrentThreadList,NewThreadList,TemporaryThreadList:PFLREThreadList;
    CurrentThread:PFLREThread;
    Instruction:PFLREInstruction;
    CurrentChar:ansichar;
begin

 for CurrentPosition:=0 to MaxPrefixCharClasses-1 do begin
  PrefixCharClasses[CurrentPosition]:=[];
 end;

 CountPrefixCharClasses:=0;

 CurrentThreadList:=@ThreadLists[0];
 NewThreadList:=@ThreadLists[1];

 CurrentThreadList^.Count:=0;
 NewThreadList^.Count:=0;

 Generation:=1;
 AddThread(CurrentThreadList,AnchoredStartInstruction);

 Count:=0;

 for CurrentPosition:=0 to MaxPrefixCharClasses do begin
  if CurrentThreadList^.Count=0 then begin
   break;
  end;
  inc(Generation);
  inc(Count);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR:begin
     if CurrentPosition<MaxPrefixCharClasses then begin
      PrefixCharClasses[CurrentPosition]:=PrefixCharClasses[CurrentPosition]+[ansichar(byte(Instruction^.Value))];
     end;
     AddThread(NewThreadList,Instruction^.Next);
    end;
    opCHAR:begin
     if CurrentPosition<MaxPrefixCharClasses then begin
      PrefixCharClasses[CurrentPosition]:=PrefixCharClasses[CurrentPosition]+PFLRECharClass(pointer(ptruint(Instruction^.Value)))^;
     end;
     AddThread(NewThreadList,Instruction^.Next);
    end;
    opANY:begin
     if CurrentPosition=0 then begin
      BeginningWildCard:=true;
     end;
     if CurrentPosition<MaxPrefixCharClasses then begin
      PrefixCharClasses[CurrentPosition]:=PrefixCharClasses[CurrentPosition]+[#0..#255];
     end;
     AddThread(NewThreadList,Instruction^.Next);
    end;
    opMATCH:begin
     if CountPrefixCharClasses=0 then begin
      CountPrefixCharClasses:=CurrentPosition;
     end;
    end;
   end;
  end;
  TemporaryThreadList:=CurrentThreadList;
  CurrentThreadList:=NewThreadList;
  NewThreadList:=TemporaryThreadList;
  NewThreadList^.Count:=0;
 end;

 if CountPrefixCharClasses=0 then begin
  CountPrefixCharClasses:=Count;
  if CountPrefixCharClasses>MaxPrefixCharClasses then begin
   CountPrefixCharClasses:=MaxPrefixCharClasses;
  end;
 end;

 Generation:=0;
 for CurrentPosition:=0 to CountForwardInstructions-1 do begin
  ForwardInstructions[CurrentPosition].Generation:=0;
 end;

 CountObviousPrefixCharClasses:=0;
 for CurrentPosition:=0 to CountPrefixCharClasses-1 do begin
  Count:=0;
  for CurrentChar:=#0 to #255 do begin
   if CurrentChar in PrefixCharClasses[CurrentPosition] then begin
    inc(Count);
   end;
  end;
  if (Count>0) and (Count<=128) then begin
   inc(CountObviousPrefixCharClasses);
  end;
 end;

 CompilePrefixPattern;

end;

procedure TFLRE.CompileByteMapForOnePassNFAAndDFA;
var Node:PFLRENode;
    i,j,ByteCount:integer;
    CharSetMap:TFLRECharClass;
    CharClass:TFLRECharClass;
    c:ansichar;
begin
 FillChar(ByteMap,SizeOf(TFLREByteMap),#0);
 FillChar(UnByteMap,SizeOf(TFLREByteMap),#0);
 ByteCount:=0;
 CharSetMap:=[];
 for i:=0 to Nodes.Count-1 do begin
  Node:=Nodes[i];
  if assigned(Node) then begin
   case Node^.NodeType of
    ntCHAR:begin
     CharClass:=Node^.CharClass;
     if CharClass<>[#0..#255] then begin
      for j:=0 to 255 do begin
       c:=ansichar(byte(j));
       if (c in CharClass) and not (c in CharSetMap) then begin
        System.Include(CharSetMap,c);
        ByteMap[byte(c)]:=ByteCount;
        UnByteMap[ByteCount]:=byte(c);
        inc(ByteCount);
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 if ByteCount<256 then begin
  for i:=0 to 255 do begin
   if ansichar(byte(i)) in CharSetMap then begin
    inc(ByteMap[i]);
   end;
  end;
  inc(ByteCount);
 end;
 ByteCharSetMap:=CharSetMap;
 ByteMapCount:=ByteCount;
end;

procedure TFLRE.CompileOnePassNFA;
type TStackItem=record
      Instruction:PFLREInstruction;
      Condition:longword;
     end;
     TStack=array of TStackItem;
var Instruction:PFLREInstruction;
    ToVisit,WorkQueue:TList;
    Stack:TStack;
    StackPointer,Len,MaxNodes,StateSize,i,NextIndex,NodesCount,b,ToVisitIndex,IndexValue:longint;
    Condition,Action,NewAction:longword;
    NodeByID:array of longint;
    Nodes:PFLREOnePassNFAState;
    Node:PFLREOnePassNFAState;
    Matched,HasMatch:boolean;
    CharClass:TFLRECharClass;
    CharClassAction:PFLREOnePassNFAStateCharClassAction;
    DestCharClassAction:PFLREOnePassNFAStateCharClassAction;
begin
 OnePassNFAReady:=true;
 Node:=nil;
 Instruction:=AnchoredStartInstruction;
 Len:=CountForwardInstructions;
 MaxNodes:=Len+2;
 StateSize:=SizeOf(TFLREOnePassNFAState)+((ByteMapCount+1)*SizeOf(longword));
 HasMatch:=false;
 Stack:=nil;
 try
  SetLength(Stack,Len*4);
  NodeByID:=nil;
  try
   SetLength(NodeByID,Len+2);
   FillChar(NodeByID[0],length(NodeByID)*SizeOf(longint),#$ff);
   Nodes:=nil;
   try
    GetMem(Nodes,StateSize*MaxNodes);
    FillChar(Nodes^,StateSize*MaxNodes,#$00);
    NodeByID[AnchoredStartInstruction^.IndexAndOpcode shr 8]:=0;
    NodesCount:=1;
    Condition:=0;
    ToVisit:=TList.Create;
    try
     WorkQueue:=TList.Create;
     try
      ToVisit.Add(AnchoredStartInstruction);
      ToVisitIndex:=0;
      while OnePassNFAReady and (ToVisitIndex<ToVisit.Count) do begin
       Instruction:=ToVisit[ToVisitIndex];
       inc(ToVisitIndex);
       if assigned(Instruction) then begin
        Node:=pointer(@pansichar(Nodes)[StateSize*longint(NodeByID[Instruction^.IndexAndOpcode shr 8])]);
        for i:=0 to ByteMapCount-1 do begin
         Node^.Action[i]:=sfImpossible;
        end;
        Node^.MatchCondition:=sfImpossible;
        Node^.NoAction:=sfImpossible;
        Matched:=false;
        WorkQueue.Clear;
        Stack[0].Instruction:=Instruction;
        Stack[0].Condition:=0;
        StackPointer:=1;
        while OnePassNFAReady and (StackPointer>0) do begin
         dec(StackPointer);
         Instruction:=Stack[StackPointer].Instruction;
         Condition:=Stack[StackPointer].Condition;
         case Instruction^.IndexAndOpcode and $ff of
          opJMP:begin
           if WorkQueue.IndexOf(Instruction^.Next)>=0 then begin
            OnePassNFAReady:=false;
            break;
           end;
           WorkQueue.Add(Instruction^.Next);
           Stack[StackPointer].Instruction:=Instruction^.Next;
           Stack[StackPointer].Condition:=Condition;
           inc(StackPointer);
          end;
          opSINGLECHAR,opCHAR,opANY:begin
           NextIndex:=NodeByID[Instruction^.Next^.IndexAndOpcode shr 8];
           if NextIndex<0 then begin
            if NodesCount>=MaxNodes then begin
             OnePassNFAReady:=false;
             break;
            end;
            NextIndex:=NodesCount;
            NodeByID[Instruction^.Next^.IndexAndOpcode shr 8]:=NextIndex;
            inc(NodesCount);
            ToVisit.Add(Instruction^.Next);
           end;
           if Matched then begin
            Condition:=Condition or sfMatchWins;
           end;
           NewAction:=longword(NextIndex shl sfIndexShift) or Condition;
           begin
            case Instruction^.IndexAndOpcode and $ff of
             opSINGLECHAR:begin
              CharClass:=[ansichar(byte(Instruction^.Value))];
             end;
             opCHAR:begin
              CharClass:=PFLRECharClass(pointer(ptruint(Instruction^.Value)))^;
             end;
             else {FLREoANY:}begin
              CharClass:=[#0..#255];
             end;
            end;
            DestCharClassAction:=nil;
            CharClassAction:=Node^.CharClassAction;
            while assigned(CharClassAction) do begin
             if (CharClassAction.CharClass*CharClass)<>[] then begin
              DestCharClassAction:=CharClassAction;
              break;
             end;
             CharClassAction:=CharClassAction^.Next;
            end;
            if assigned(DestCharClassAction) then begin
             if DestCharClassAction^.Condition<>NewAction then begin
              OnePassNFAReady:=false;
              break;
             end;
            end else begin
             New(DestCharClassAction);
             FillChar(DestCharClassAction^,SizeOf(TFLREOnePassNFAStateCharClassAction),#0);
             DestCharClassAction^.AllNext:=OnePassNFACharClassActions;
             OnePassNFACharClassActions:=DestCharClassAction;
             DestCharClassAction^.Next:=Node^.CharClassAction;
             Node^.CharClassAction:=DestCharClassAction;
             DestCharClassAction^.CharClass:=CharClass;
             DestCharClassAction^.Condition:=NewAction;
            end;
           end;
           begin
            case Instruction^.IndexAndOpcode and $ff of
             opSINGLECHAR:begin
              b:=ByteMap[Instruction^.Value and $ff];
              Action:=Node^.Action[b];
              if (Action and sfImpossible)=sfImpossible then begin
               Node^.Action[b]:=NewAction;
              end else if Action<>NewAction then begin
               OnePassNFAReady:=false;
               break;
              end;
             end;
             opCHAR:begin
              if PFLRECharClass(pointer(ptruint(Instruction^.Value)))^=[#0..#255] then begin
               for i:=0 to ByteMapCount-1 do begin
                Action:=Node^.Action[i];
                if (Action and sfImpossible)=sfImpossible then begin
                 Node^.Action[i]:=NewAction;
                end else if Action<>NewAction then begin
                 OnePassNFAReady:=false;
                 break;
                end;
               end;
              end else begin
               for i:=0 to 255 do begin
                if ansichar(byte(i)) in PFLRECharClass(pointer(ptruint(Instruction^.Value)))^ then begin
                 b:=ByteMap[i];
                 Action:=Node^.Action[b];
                 if (Action and sfImpossible)=sfImpossible then begin
                  Node^.Action[b]:=NewAction;
                 end else if Action<>NewAction then begin
                  OnePassNFAReady:=false;
                  break;
                 end;
                end;
               end;
              end;
              if not OnePassNFAReady then begin
               break;
              end;
             end;
             else {FLREoANY:}begin
              for i:=0 to ByteMapCount-1 do begin
               Action:=Node^.Action[i];
               if (Action and sfImpossible)=sfImpossible then begin
                Node^.Action[i]:=NewAction;
               end else if Action<>NewAction then begin
                OnePassNFAReady:=false;
                break;
               end;
              end;
              if not OnePassNFAReady then begin
               break;
              end;
             end;
            end;
           end;
          end;
          opMATCH:begin
           if Matched then begin
            OnePassNFAReady:=false;
            break;
           end;
           Matched:=true;
           HasMatch:=true;
           Node^.MatchCondition:=Condition;
           if NodesCount>=MaxNodes then begin
            OnePassNFAReady:=false;
            break;
           end else begin
            NextIndex:=0;
            NewAction:=longword(NextIndex shl sfIndexShift) or (Condition or sfMatchWins);
            if (Node^.NoAction shr sfIndexShift)=0 then begin
             if (Node^.NoAction and sfImpossible)=sfImpossible then begin
              Node^.NoAction:=NewAction;
             end else if Node^.NoAction<>NewAction then begin
              OnePassNFAReady:=false;
              break;
             end;
            end;
            for i:=0 to ByteMapCount-1 do begin
             Action:=Node^.Action[i];
             if (Action shr sfIndexShift)=0 then begin
              if (Action and sfImpossible)=sfImpossible then begin
               Node^.Action[i]:=NewAction;
              end else if Action<>NewAction then begin
               OnePassNFAReady:=false;
               break;
              end;
             end;
            end;
           end;
          end;
          opSPLIT:begin
           if (WorkQueue.IndexOf(Instruction^.Next)>=0) or
              (WorkQueue.IndexOf(Instruction^.OtherNext)>=0) then begin
            OnePassNFAReady:=false;
            break;
           end;
           WorkQueue.Add(Instruction^.Next);
           WorkQueue.Add(Instruction^.OtherNext);
           Stack[StackPointer].Instruction:=Instruction^.OtherNext;
           Stack[StackPointer].Condition:=Condition;
           inc(StackPointer);
           Stack[StackPointer].Instruction:=Instruction^.Next;
           Stack[StackPointer].Condition:=Condition;
           inc(StackPointer);
          end;
          opSAVE:begin
           IndexValue:=Instruction^.Value;
           if IndexValue>=sfMaxCap then begin
            OnePassNFAReady:=false;
            break;
           end;
           Condition:=Condition or ((1 shl sfCapShift) shl IndexValue);
           if WorkQueue.IndexOf(Instruction^.Next)>=0 then begin
            OnePassNFAReady:=false;
            break;
           end;
           WorkQueue.Add(Instruction^.Next);
           Stack[StackPointer].Instruction:=Instruction^.Next;
           Stack[StackPointer].Condition:=Condition;
           inc(StackPointer);
          end;
          opBOL,opEOL,opBOT,opEOT,opBRK,opNBRK:begin
           case Instruction^.IndexAndOpcode and $ff of
            opBOL:begin
             Condition:=Condition or sfEmptyBeginLine;
            end;
            opEOL:begin
             Condition:=Condition or sfEmptyEndLine;
            end;
            opBOT:begin
             Condition:=Condition or sfEmptyBeginText;
            end;
            opEOT:begin
             Condition:=Condition or sfEmptyEndText;
            end;
            opBRK:begin
             Condition:=Condition or sfEmptyWordBoundary;
            end;
            opNBRK:begin
             Condition:=Condition or sfEmptyNonWordBoundary;
            end;
           end;
           if WorkQueue.IndexOf(Instruction^.Next)>=0 then begin
            OnePassNFAReady:=false;
            break;
           end;
           WorkQueue.Add(Instruction^.Next);
           Stack[StackPointer].Instruction:=Instruction^.Next;
           Stack[StackPointer].Condition:=Condition;
           inc(StackPointer);
          end;
          else begin
           OnePassNFAReady:=false;
           break;
          end;
         end;
        end;
       end else begin
        break;
       end;
      end;
      if not HasMatch then begin
       OnePassNFAReady:=false;
      end;
      if OnePassNFAReady then begin
       if NodesCount>=MaxNodes then begin
        OnePassNFAReady:=false;
       end else begin
        NextIndex:=0;
        NewAction:=longword(NextIndex shl sfIndexShift) or (Condition or sfMatchWins);
        if (Node^.NoAction shr sfIndexShift)=0 then begin
         if (Node^.NoAction and sfImpossible)=sfImpossible then begin
          Node^.NoAction:=NewAction;
         end else if Node^.NoAction<>NewAction then begin
          OnePassNFAReady:=false;
         end;
        end;
        if OnePassNFAReady then begin
         for i:=0 to ByteMapCount-1 do begin
          Action:=Node^.Action[i];
          if (Action shr sfIndexShift)=0 then begin
           if (Action and sfImpossible)=sfImpossible then begin
            Node^.Action[i]:=NewAction;
           end else if Action<>NewAction then begin
            OnePassNFAReady:=false;
            break;
           end;
          end;
         end;
        end;
       end;
      end;
      if OnePassNFAReady then begin
       GetMem(OnePassNFANodes,NodesCount*StateSize);
       OnePassNFANodesCount:=NodesCount;
       Move(Nodes^,OnePassNFANodes^,NodesCount*StateSize);
       OnePassNFAStart:=pointer(@pansichar(OnePassNFANodes)[StateSize*longint(NodeByID[AnchoredStartInstruction^.IndexAndOpcode shr 8])]);
       OnePassNFAStateSize:=StateSize;
      end;
     finally
      WorkQueue.Free;
     end;
    finally
     ToVisit.Free;
    end;
   finally
    if assigned(Nodes) then begin
     FreeMem(Nodes);
    end;
   end;
  finally
   SetLength(NodeByID,0);
  end;
 finally
  SetLength(Stack,0);
 end;
end;

function TFLRE.NewSubMatches(const Count:longint;const BitState:longword):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
begin
 if assigned(FreeSubMatches) then begin
  result:=FreeSubMatches;
  FreeSubMatches:=result^.Next;
 end else begin
  GetMem(result,SizeOf(TFLRESubMatches));
  FillChar(result^,SizeOf(TFLRESubMatches),#0);
  SetLength(result^.SubMatches,CountSubMatches);
  AllSubMatches.Add(result);
 end;
 result^.ReferenceCounter:=1;
 result^.Count:=Count;
 result^.BitState:=BitState;
end;

procedure TFLRE.DecRef(const SubMatches:PFLRESubMatches); {$ifdef caninline}inline;{$endif}
begin
 dec(SubMatches^.ReferenceCounter);
 if SubMatches^.ReferenceCounter=0 then begin
  SubMatches^.Next:=FreeSubMatches;
  FreeSubMatches:=SubMatches;
 end;
end;

function TFLRE.IncRef(const SubMatches:PFLRESubMatches):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
begin
 inc(SubMatches^.ReferenceCounter);
 result:=SubMatches;
end;

function TFLRE.Update(const SubMatches:PFLRESubMatches;const Index,Position:longint):PFLRESubMatches; {$ifdef caninline}inline;{$endif}
var Counter:longint;
    BitState:longword;
begin
 result:=SubMatches;
 if result^.ReferenceCounter>1 then begin
  result:=NewSubMatches(SubMatches^.Count,SubMatches^.BitState);
{$ifdef cpu386}
  asm
   push ebx
   push esi
   push edi
    mov ebx,dword ptr result
    mov ecx,dword ptr [ebx+TFLRESubMatches.BitState]
    test ecx,ecx // or test ecx,$80000000; jnz @CopyAll 
    js @CopyAll
    jecxz @Done
     mov esi,dword ptr SubMatches
     mov esi,dword ptr [esi+TFLRESubMatches.SubMatches]
     mov edi,dword ptr [ebx+TFLRESubMatches.SubMatches]
     @CopySelectedLoop:
       bsf eax,ecx
       mov edx,dword ptr [esi+eax*4]
       mov dword ptr [edi+eax*4],edx
       lea edx,[ecx-1]
       and ecx,edx
      jnz @CopySelectedLoop
     jmp @Done
     @CopyAll:
      mov esi,dword ptr SubMatches
      mov ecx,dword ptr [esi+TFLRESubMatches.Count]
      mov esi,dword ptr [esi+TFLRESubMatches.SubMatches]
      mov edi,dword ptr [ebx+TFLRESubMatches.SubMatches]
      rep movsd
    @Done:
   pop edi
   pop esi
   pop ebx
  end;
{$else}
  if (result^.BitState and longword($80000000))=0 then begin
   BitState:=result^.BitState;
   while BitState<>0 do begin
    Counter:=PopFirstOneBit(BitState);
    result^.SubMatches[Counter]:=SubMatches^.SubMatches[Counter];
   end;
  end else begin
   Move(SubMatches^.SubMatches[0],result^.SubMatches[0],SubMatches^.Count*SizeOf(TFLRESubMatchesItem));
  end;
{$endif}
  dec(SubMatches^.ReferenceCounter);
 end;
{$ifdef cpu386}
 result^.BitState:=result^.BitState or ((longword(1) shl Index) or longword(-longword(longword(-(Index-30)) shr 31)));
{$else}
 if (result^.BitState and longword($80000000))=0 then begin
  if Index>30 then begin
   result^.BitState:=$ffffffff;
  end else begin
   result^.BitState:=result^.BitState or (longword(1) shl Index);
  end;
 end;
{$endif}
 result^.SubMatches[Index]:=Position;
end;

function TFLRE.NewThread(const Instruction:PFLREInstruction;const SubMatches:PFLRESubMatches):TFLREThread; {$ifdef caninline}inline;{$endif}
begin
 result.Instruction:=Instruction;
 result.SubMatches:=SubMatches;
end;

function TFLRE.IsWordChar(const Position:longint):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((Position>=0) and (Position<InputLength)) and (Input[Position] in ['A'..'Z','a'..'z']);
end;

procedure TFLRE.AddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction;SubMatches:PFLRESubMatches;const Position:longint);
var Thread:PFLREThread;
begin
 while assigned(Instruction) do begin
  if Instruction^.Generation=Generation then begin
   DecRef(SubMatches);
   break;
  end else begin
   Instruction^.Generation:=Generation;
   case Instruction^.IndexAndOpcode and $ff of
    opJMP:begin
     Instruction:=Instruction^.Next;
     continue;
    end;
    opSPLIT:begin
     AddThread(ThreadList,Instruction^.Next,IncRef(SubMatches),Position);
     Instruction:=Instruction^.OtherNext;
     continue;
    end;
    opSAVE:begin
     SubMatches:=Update(SubMatches,Instruction^.Value,Position);
     Instruction:=Instruction^.Next;
     continue;
    end;
    opBOL:begin
     if (Position=0) or ((Position>0) and (Input[Position-1] in [#10,#13])) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    opEOL:begin
     if ((Position+1)>=InputLength) or (((Position+1)<InputLength) and (Input[Position+1] in [#10,#13])) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    opBOT:begin
     if Position=0 then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    opEOT:begin
     if (Position+1)>=InputLength then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    opBRK:begin
     if IsWordChar(Position-1)<>IsWordChar(Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    opNBRK:begin
     if IsWordChar(Position-1)=IsWordChar(Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      DecRef(SubMatches);
      break;
     end;
    end;
    else begin
     Thread:=@ThreadList^.Threads[ThreadList^.Count];
     inc(ThreadList^.Count);
     Thread^.Instruction:=Instruction;
     Thread^.SubMatches:=SubMatches;
     break;
    end;
   end;
  end;
 end;
end;

function TFLRE.DFACacheState(const State:PFLREDFAState):PFLREDFAState; {$ifdef caninline}inline;{$endif}
begin

 // Is it already cached?
 result:=DFAStateCache[State];
 if not assigned(result) then begin
  // No, it is not already cached yet

  // Do we need reset the states?
  if DFACountStatesCached>=MaxDFAStates then begin
   // Yes, so do it
   DFAReset;
  end;

  // Move state in an own new memory instance
  result:=DFATakeOverState(State);

  // Add state to state cache hash map
  DFAStateCache.Add(result);
  inc(DFACountStatesCached);

 end;

end;

procedure TFLRE.DFAAddInstructionThread(const State:PFLREDFAState;Instruction:PFLREInstruction);
var StackPointer:longint;
    Stack:PPFLREInstructionsStatic;
begin
 Stack:=@DFAStackInstructions[0];
 StackPointer:=0;
 Stack^[StackPointer]:=Instruction;
 inc(StackPointer);
 while StackPointer>0 do begin
  dec(StackPointer);
  Instruction:=Stack[StackPointer];
  while assigned(Instruction) and (Instruction^.Generation<>Generation) do begin
   Instruction^.Generation:=Generation;
   case Instruction^.IndexAndOpcode and $ff of
    opJMP:begin
     Instruction:=Instruction^.Next;
    end;
    opSAVE:begin
{    if Instruction^.Value=0 then begin
      State.Flags:=State.Flags or sfDFAMatchBegins;
     end;{}
     Instruction:=Instruction^.Next;
    end;
    opSPLIT:begin
     Stack^[StackPointer]:=Instruction^.OtherNext;
     inc(StackPointer);
     Instruction:=Instruction^.Next;
    end;
    else begin
     if State.CountInstructions>=length(State.Instructions) then begin
      SetLength(State.Instructions,(State.CountInstructions+1)*2);
     end;
     State.Instructions[State.CountInstructions]:=Instruction;
     inc(State.CountInstructions);
     case Instruction^.IndexAndOpcode and $ff of
      opMATCH:begin
       State.Flags:=State.Flags or sfDFAMatchWins;
      end;
     end;
     break;
    end;
   end;
  end;
 end;
end;
                                           
function TFLRE.DFAProcessNextState(State:PFLREDFAState;const CurrentChar:ansichar;const Reversed:boolean):PFLREDFAState;
var Counter:longint;
    Instruction:PFLREInstruction;
begin

 // Process state instructions
 inc(Generation);
 DFANewState.CountInstructions:=0;
 DFANewState.Flags:=(State^.Flags and sfDFAStart) shl sfDFANeedShift;
 for Counter:=0 To State^.CountInstructions-1 do begin
  Instruction:=State^.Instructions[Counter];
  case Instruction^.IndexAndOpcode and $ff of
   opSINGLECHAR:begin
    if byte(ansichar(CurrentChar))=Instruction^.Value then begin
     DFAAddInstructionThread(@DFANewState,Instruction^.Next);
    end;
   end;
   opCHAR:begin
    if CurrentChar in PFLRECharClass(pointer(ptruint(Instruction^.Value)))^ then begin
     DFAAddInstructionThread(@DFANewState,Instruction^.Next);
    end;
   end;
   opANY:begin
    DFAAddInstructionThread(@DFANewState,Instruction^.Next);
   end;
   opMATCH:begin
    if not ((rfLONGEST in Flags) or Reversed) then begin
     // We can stop processing the instruction list queue here since we found a match (for the PCRE leftmost first valid match behaviour)
     break;
    end;
   end;
   else begin
    // Oops?! Invalid byte code instruction for the DFA processing context! So abort and fallback to NFA...
    result:=nil;
    exit;
   end;
  end;
 end;

 // Dead state? If yes, ...
 if DFANewState.CountInstructions=0 then begin
  // ... drop it and take the dead state as the next state
  result:=DFADeadState;
 end else begin  
  // .. otherwise try caching it
  result:=DFACacheState(@DFANewState);
 end;

 // Connect the last state to the new state with the current char
 State.NextStates[ByteMap[byte(ansichar(CurrentChar))]]:=result;
end;

procedure TFLRE.DFADestroyStatePool(StatePool:PFLREDFAStatePool);
var Pool,NextPool:PFLREDFAStatePool;
    State:PFLREDFAState;
begin
 Pool:=StatePool;
 while assigned(Pool) do begin
  NextPool:=Pool^.Next;
  State:=Pool^.States;
  while assigned(State) and (State<>Pool^.EndState) do begin
   DFAFreeState(State);
   inc(ptruint(State),DFAStateSize);
  end;
  FreeMem(Pool^.States);
  FreeMem(Pool);
  Pool:=NextPool;
 end;
end;

procedure TFLRE.DFAFreeUsedStatePool;
var Pool,NextPool:PFLREDFAStatePool;
    State:PFLREDFAState;
begin
 Pool:=DFAStatePoolUsed;
 DFAStatePoolUsed:=nil;
 while assigned(Pool) do begin
  NextPool:=Pool^.Next;
  State:=Pool^.States;
  while assigned(State) and (State<>Pool^.EndState) do begin
   DFAFreeState(State);
   inc(ptruint(State),DFAStateSize);
  end;
  FillChar(Pool^.States^,DFAStatePoolSize,AnsiChar(#0));
  Pool^.Next:=DFAStatePoolFree;
  DFAStatePoolFree:=Pool;
  Pool:=NextPool;
 end;
end;

function TFLRE.DFAAllocateNewStatePool:PFLREDFAStatePool;
begin
 if assigned(DFAStatePoolFree) then begin
  result:=DFAStatePoolFree;
  DFAStatePoolFree:=result^.Next;
 end else begin
  GetMem(result,SizeOf(TFLREDFAStatePool));
  FillChar(result^,SizeOf(TFLREDFAStatePool),AnsiChar(#0));
  GetMem(result^.States,DFAStatePoolSizePowerOfTwo);
  FillChar(result^.States^,DFAStatePoolSize,AnsiChar(#0));
  result^.EndState:=pointer(ptruint(ptruint(result^.States)+ptruint(DFAStatePoolSize)));
 end;
 result^.Next:=DFAStatePoolUsed;
 DFAStatePoolUsed:=result;
 result^.NextState:=result^.States;
end;

function TFLRE.DFAGetState:PFLREDFAState;
begin
 if DFAStatePoolUsed^.NextState=DFAStatePoolUsed^.EndState then begin
  DFAAllocateNewStatePool;
 end;
 result:=DFAStatePoolUsed^.NextState;
 inc(ptruint(DFAStatePoolUsed^.NextState),DFAStateSize);
end;

function TFLRE.DFATakeOverState(TakeOverFrom:PFLREDFAState):PFLREDFAState;
begin
 result:=DFAGetState;
 result^.Instructions:=TakeOverFrom^.Instructions;
 result^.CountInstructions:=TakeOverFrom^.CountInstructions;
 result^.Flags:=TakeOverFrom^.Flags;
 TakeOverFrom^.Instructions:=nil;
 TakeOverFrom^.CountInstructions:=0;
 TakeOverFrom^.Flags:=0;
end;

procedure TFLRE.DFAFreeState(State:PFLREDFAState);
begin
 if assigned(State) then begin
  SetLength(State^.Instructions,0);
  if assigned(DFAStatePoolUsed) and
     ((ptruint(ptruint(State)-ptruint(DFAStatePoolUsed^.States))<DFAStatePoolSize) and
      (pointer(ptruint(ptruint(DFAStatePoolUsed^.NextState)-ptruint(DFAStateSize)))=State)) then begin
   FillChar(State^,DFAStateSize,AnsiChar(#0));
   dec(ptruint(DFAStatePoolUsed^.NextState),DFAStateSize);
  end;
 end;
end;

procedure TFLRE.DFAReset;
var State:PFLREDFAState;
begin

 // Reset state pools
 DFAFreeUsedStatePool;
 DFAAllocateNewStatePool;

 // Reset state cache
 DFAStateCache.Clear;
 DFACountStatesCached:=0;

 // Reset and recaching start states
 if assigned(DFAAnchoredStartState) then begin
  State:=DFAAnchoredStartState;
  FillChar(State^.NextStates,DFANextStatesSize,AnsiChar(#0));
  DFAStateCache.Add(State);
  inc(DFACountStatesCached);
 end;
 if assigned(DFAUnanchoredStartState) then begin
  State:=DFAUnanchoredStartState;
  FillChar(State^.NextStates,DFANextStatesSize,AnsiChar(#0));
  DFAStateCache.Add(State);
  inc(DFACountStatesCached);
 end;
 if assigned(DFAReversedStartState) then begin
  State:=DFAReversedStartState;
  FillChar(State^.NextStates,DFANextStatesSize,AnsiChar(#0));
  DFAStateCache.Add(State);
  inc(DFACountStatesCached);
 end;
 if assigned(DFADeadState) then begin
  State:=DFADeadState;
  FillChar(State^.NextStates,DFANextStatesSize,AnsiChar(#0));
  DFAStateCache.Add(State);
  inc(DFACountStatesCached);
 end;

end;

function TFLRE.SearchMatchParallelNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):boolean;
var CurrentPosition,Counter,ThreadIndex,CurrentLength,LastPosition:longint;
    CurrentThreadList,NewThreadList,TemporaryThreadList:PFLREThreadList;
    SubMatches,Matched,BestSubMatches:PFLRESubMatches;
    CurrentThread:PFLREThread;
    Thread:TFLREThread;
    Instruction:PFLREInstruction;
    CurrentChar:ansichar;
    Capture:PFLRECapture;
    BitState:longword;
    LocalInput:pansichar;
begin
 result:=false;

 LocalInput:=Input;

 CurrentThreadList:=@ThreadLists[0];
 NewThreadList:=@ThreadLists[1];

 CurrentThreadList^.Count:=0;
 NewThreadList^.Count:=0;

 SubMatches:=NewSubMatches(CountSubMatches,0);

 inc(Generation);
 if UnanchoredStart then begin
  AddThread(CurrentThreadList,UnanchoredStartInstruction,SubMatches,StartPosition);
 end else begin
  AddThread(CurrentThreadList,AnchoredStartInstruction,SubMatches,StartPosition);
 end;

 Matched:=nil;

 BestSubMatches:=nil;

 LastPosition:=-1;

 for CurrentPosition:=StartPosition to UntilExcludingPosition-1 do begin
  if CurrentThreadList^.Count=0 then begin
   break;
  end;
  CurrentChar:=LocalInput[CurrentPosition];
  inc(Generation);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   SubMatches:=CurrentThread^.SubMatches;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR:begin
     if (CurrentPosition>=InputLength) or (byte(ansichar(CurrentChar))<>Instruction^.Value) then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1);
     end;
    end;
    opCHAR:begin
     if (CurrentPosition>=InputLength) or not (CurrentChar in PFLRECharClass(pointer(ptruint(Instruction^.Value)))^) then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1);
     end;
    end;
    opANY:begin
     if CurrentPosition>=InputLength then begin
      DecRef(SubMatches);
     end else begin
      AddThread(NewThreadList,Instruction^.Next,SubMatches,CurrentPosition+1);
     end;
    end;
    opMATCH:begin
     if rfLONGEST in Flags then begin
      if not assigned(BestSubMatches) then begin
       BestSubMatches:=NewSubMatches(CountSubMatches,SubMatches^.BitState);
      end;
      if SubMatches^.BitState<>0 then begin
       if LastPosition<CurrentPosition then begin
        LastPosition:=CurrentPosition;
        BestSubMatches^.BitState:=SubMatches^.BitState;
        Move(SubMatches^.SubMatches[0],BestSubMatches^.SubMatches[0],SubMatches^.Count*SizeOf(TFLRESubMatchesItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       DecRef(Matched);
      end;
      Matched:=SubMatches;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       DecRef(CurrentThreadList^.Threads[Counter].SubMatches);
      end;
      break;
     end;
    end;
   end;
  end;
  TemporaryThreadList:=CurrentThreadList;
  CurrentThreadList:=NewThreadList;
  NewThreadList:=TemporaryThreadList;
  NewThreadList^.Count:=0;
 end;

 if CurrentThreadList^.Count<>0 then begin
  inc(Generation);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   SubMatches:=CurrentThread^.SubMatches;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR,opCHAR,opANY:begin
     DecRef(SubMatches);
    end;
    opMATCH:begin
     if rfLONGEST in Flags then begin
      if not assigned(BestSubMatches) then begin
       BestSubMatches:=NewSubMatches(CountSubMatches,SubMatches^.BitState);
      end;
      if SubMatches^.BitState<>0 then begin
       if LastPosition<UntilExcludingPosition then begin
        LastPosition:=UntilExcludingPosition;
        BestSubMatches^.BitState:=SubMatches^.BitState;
        Move(SubMatches^.SubMatches[0],BestSubMatches^.SubMatches[0],SubMatches^.Count*SizeOf(TFLRESubMatchesItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       DecRef(Matched);
      end;
      Matched:=SubMatches;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       DecRef(CurrentThreadList^.Threads[Counter].SubMatches);
      end;
      break;
     end;
    end;
   end;
  end;
 end;

 if assigned(BestSubMatches) then begin
  if assigned(Matched) then begin
   DecRef(Matched);
  end;
  Matched:=BestSubMatches;
 end;

 if assigned(Matched) then begin
  SetLength(Captures,CountParens);
  BitState:=Matched^.BitState;
  for Counter:=0 to CountParens-1 do begin
   Capture:=@Captures[Counter];
   if (BitState and longword($80000000))<>0 then begin
    CurrentPosition:=Matched^.SubMatches[Counter shl 1];
    CurrentLength:=Matched^.SubMatches[(Counter shl 1) or 1]-CurrentPosition;
   end else begin
    if (BitState and (longword(1) shl (Counter shl 1)))<>0 then begin
     CurrentPosition:=Matched^.SubMatches[Counter shl 1];
    end else begin
     CurrentPosition:=0;
    end;
    if (BitState and (longword(1) shl ((Counter shl 1) or 1)))<>0 then begin
     CurrentLength:=Matched^.SubMatches[(Counter shl 1) or 1]-CurrentPosition;
    end else begin
     CurrentLength:=0;
    end;
   end;
   if CurrentLength<1 then begin
    Capture^.Start:=0;
    Capture^.Length:=0;
   end else begin
    Capture^.Start:=CurrentPosition;
    Capture^.Length:=CurrentLength;
   end;
  end;
  DecRef(Matched);
  result:=true;
 end;

end;

function TFLRE.SearchMatchOnePassNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint):boolean;
var State,Nodes:PFLREOnePassNFAState;
    CurrentPosition,StateSize,TwoCountOfCaptures,Counter:longint;
    LocalByteMap:PFLREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex:longword;
    LocalInput:pansichar;
 function Satisfy(Condition:longword):boolean;
 var Flags:longword;
 begin
  Flags:=0;
  if CurrentPosition<=0 then begin
   Flags:=Flags or (sfEmptyBeginText or sfEmptyBeginLine);
  end else if Input[CurrentPosition-1] in [#10,#13] then begin
   Flags:=Flags or sfEmptyBeginLine;
  end;
  if CurrentPosition>InputLength then begin
   Flags:=Flags or (sfEmptyEndText or sfEmptyEndLine);
  end else if (CurrentPosition<InputLength) and (Input[CurrentPosition] in [#10,#13]) then begin
   Flags:=Flags or sfEmptyEndLine;
  end;
  if InputLength>0 then begin
   if CurrentPosition=0 then begin
    if IsWordChar(0) then begin
     Flags:=Flags or sfEmptyWordBoundary;
    end;
   end else if CurrentPosition>=InputLength then begin
    if IsWordChar(CurrentPosition-1) then begin
     Flags:=Flags or sfEmptyWordBoundary;
    end;
   end else if IsWordChar(CurrentPosition-1)<>IsWordChar(CurrentPosition) then begin
    Flags:=Flags or sfEmptyWordBoundary;
   end;
  end;
  if (Flags and sfEmptyWordBoundary)=0 then begin
   Flags:=Flags or sfEmptyNonWordBoundary;
  end;
  result:=((Condition and sfEmptyAllFlags) and not Flags)=0;
 end;
begin
 TwoCountOfCaptures:=CountParens*2;

 State:=OnePassNFAStart;
 Nodes:=OnePassNFANodes;
 StateSize:=OnePassNFAStateSize;
 LocalByteMap:=@ByteMap;

 result:=false;
 Done:=false;

 NextMatchCondition:=State^.MatchCondition;
 Condition:=0;
 CurrentPosition:=StartPosition;
 LocalInput:=Input;
 while CurrentPosition<UntilExcludingPosition do begin
  Condition:=State^.Action[LocalByteMap^[byte(ansichar(LocalInput[CurrentPosition]))]];
  MatchCondition:=NextMatchCondition;

  if ((Condition and sfEmptyAllFlags)=0) or Satisfy(Condition) then begin
   NextIndex:=Condition shr sfIndexShift;
   State:=pointer(@pansichar(Nodes)[StateSize*longint(NextIndex)]);
   NextMatchCondition:=State^.MatchCondition;
  end else begin
   State:=nil;
   NextMatchCondition:=sfImpossible;
  end;

  if (MatchCondition<>sfImpossible) and
     (((Condition and sfMatchWins)<>0) or ((NextMatchCondition and sfEmptyAllFlags)<>0)) and
     (((MatchCondition and sfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   for Counter:=0 to TwoCountOfCaptures-1 do begin
    OnePassNFAMatchCaptures[Counter]:=OnePassNFAWorkCaptures[Counter];
   end;
   if (MatchCondition and sfCapMask)<>0 then begin
    for Counter:=0 to TwoCountOfCaptures-1 do begin
     if (Condition and ((1 shl sfCapShift) shl Counter))<>0 then begin
      OnePassNFAMatchCaptures[Counter]:=CurrentPosition;
     end;
    end;
   end;
   result:=true;
   if ((Condition and sfMatchWins)<>0) and not (rfLONGEST in Flags) then begin
    Done:=true;
    break;
   end;
  end;

  if not assigned(State) then begin
   Done:=true;
   break;
  end;

  if (Condition and sfCapMask)<>0 then begin
   for Counter:=0 to TwoCountOfCaptures-1 do begin
    if (Condition and ((1 shl sfCapShift) shl Counter))<>0 then begin
     OnePassNFAWorkCaptures[Counter]:=CurrentPosition;
    end;
   end;
  end;

  inc(CurrentPosition);
 end;
 
 if assigned(State) and not Done then begin
  MatchCondition:=State^.MatchCondition;
  if (MatchCondition<>sfImpossible) and (((MatchCondition and sfEmptyAllFlags)=0) or Satisfy(MatchCondition)) then begin
   if ((MatchCondition and sfCapMask)<>0) and (TwoCountOfCaptures>0) then begin
    for Counter:=0 to TwoCountOfCaptures-1 do begin
     if (MatchCondition and ((1 shl sfCapShift) shl Counter))<>0 then begin
      OnePassNFAWorkCaptures[Counter]:=CurrentPosition;
     end;
    end;
   end;
   for Counter:=0 to TwoCountOfCaptures-1 do begin
    OnePassNFAMatchCaptures[Counter]:=OnePassNFAWorkCaptures[Counter];
   end;
   result:=true;
  end;
 end;

 if result then begin
  SetLength(Captures,CountParens);
  for Counter:=0 to CountParens-1 do begin
   Captures[Counter].Start:=OnePassNFAMatchCaptures[Counter*2];
   Captures[Counter].Length:=OnePassNFAMatchCaptures[(Counter*2)+1]-OnePassNFAMatchCaptures[Counter*2];
  end;
 end;

end;

function TFLRE.SearchMatchBitStateNFA(var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):longint;
var BasePosition,Len:longint;
 function ShouldVisit(const Instruction:PFLREInstruction;const Position:longint):boolean; {$ifdef caninline}inline;{$endif}
 var i:longword;
 begin
  i:=(ptruint(Instruction^.IndexAndOpcode shr 8)*longword(Len+1))+longword(longint(Position-BasePosition));
  result:=(BitStateNFAVisited[i shr 5] and (1 shl (i and 31)))=0;
  if result then begin
   BitStateNFAVisited[i shr 5]:=BitStateNFAVisited[i shr 5] or (1 shl (i and 31));
  end;
 end;
 procedure Push(const Instruction:PFLREInstruction;const Position,Argument:longint);
 var Job:PFLREBitStateNFAJob;
 begin
  if assigned(Instruction) and not ((Argument=0) and not ShouldVisit(Instruction,Position)) then begin
   if BitStateNFACountJobs>=length(BitStateNFAJobs) then begin
    SetLength(BitStateNFAJobs,(BitStateNFACountJobs+1)*2);
   end;
   Job:=@BitStateNFAJobs[BitStateNFACountJobs];
   inc(BitStateNFACountJobs);
   Job^.Instruction:=Instruction;
   Job^.Position:=Position;
   Job^.Argument:=Argument;
  end;
 end;
 function TrySearch(StartInstruction:PFLREInstruction;var Position:longint):boolean;
 var Job:PFLREBitStateNFAJob;
     Instruction:PFLREInstruction;
     Argument,i,LastPosition:longint;
     CurrentChar,LocalFlags:longword;
     InputIsUTF8:boolean;
  function Satisfy(const NextFlags:longword;const Position:longint):boolean;
  var LocalFlags,LastChar,CurrentChar,NextChar:longword;
      Index,NextPosition:longint;
  begin
   LastChar:=$ffffffff;
   CurrentChar:=$ffffffff;
   NextChar:=$ffffffff;
   if Position>=0 then begin
    if Position>0 then begin
     LastChar:=byte(ansichar(Input[Position-1]));
    end;
    CurrentChar:=byte(ansichar(Input[Position]));
    NextPosition:=Position+1;
    if NextPosition<InputLength then begin
     NextChar:=byte(ansichar(Input[NextPosition]));
    end;
   end;
   LocalFlags:=0;
   if Position<=0 then begin
    LocalFlags:=LocalFlags or (sfEmptyBeginText or sfEmptyBeginLine);
   end else begin
    case LastChar of
     $000a,$00d,$2028,$2029:begin
      LocalFlags:=LocalFlags or sfEmptyBeginLine;
     end;
    end;
   end;
   if Position>(InputLength-1) then begin
    LocalFlags:=LocalFlags or (sfEmptyEndText or sfEmptyEndLine);
   end else begin
    case CurrentChar of
     $000a,$00d,$2028,$2029:begin
      LocalFlags:=LocalFlags or sfEmptyEndLine;
     end;
    end;
   end;
   if InputLength>0 then begin
    if Position=0 then begin
     if IsWordChar(Position) then begin
      LocalFlags:=LocalFlags or sfEmptyWordBoundary;
     end;
    end else if Position>InputLength then begin
     if IsWordChar(Position-1) then begin
      LocalFlags:=LocalFlags or sfEmptyWordBoundary;
     end;
    end else if IsWordChar(Position-1)<>IsWordChar(Position) then begin
     LocalFlags:=LocalFlags or sfEmptyWordBoundary;
    end;
   end;
   if (LocalFlags and sfEmptyWordBoundary)=0 then begin
    LocalFlags:=LocalFlags or sfEmptyNonWordBoundary;
   end;
   if NextChar<>0 then begin
   end;
   result:=((NextFlags and sfEmptyAllFlags) and not LocalFlags)=0;
  end;
 begin
  result:=false;

  LastPosition:=-1;

  BitStateNFACountJobs:=0;
  Push(StartInstruction,Position,0);

  while BitStateNFACountJobs>0 do begin
   dec(BitStateNFACountJobs);
   Job:=@BitStateNFAJobs[BitStateNFACountJobs];

   Instruction:=Job^.Instruction;
   Position:=Job^.Position;
   Argument:=Job^.Argument;

   repeat

    case Instruction^.IndexAndOpcode and $ff of
     opSPLIT:begin
      case Argument of
       0:begin
        Push(Instruction,Position,1);
        Instruction:=Instruction^.Next;
        if ShouldVisit(Instruction,Position) then begin
         continue;
        end;
       end;
       1:begin
        Argument:=0;
        Instruction:=Instruction^.OtherNext;
        if ShouldVisit(Instruction,Position) then begin
         continue;
        end;
       end;
      end;
     end;
     opSINGLECHAR:begin
      if (Position<InputLength) and (byte(ansichar(Input[Position]))=Instruction^.Value) then begin
       inc(Position);
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opCHAR:begin
      if (Position<InputLength) and (Input[Position] in PFLRECharClass(pointer(ptruint(Instruction^.Value)))^) then begin
       inc(Position);
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opANY:begin
      if Position<InputLength then begin
       inc(Position);
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opMATCH:begin
      result:=true;
      if rfLONGEST in Flags then begin
       if LastPosition<Position then begin
        LastPosition:=Position;
        for i:=0 to (CountParens*2)-1 do begin
         BitStateNFAMatchCaptures[i]:=BitStateNFAWorkCaptures[i];
        end;
       end;                                                
      end else begin
       for i:=0 to (CountParens*2)-1 do begin
        BitStateNFAMatchCaptures[i]:=BitStateNFAWorkCaptures[i];
       end;
       exit;
      end;
     end;
     opJMP:begin
      Instruction:=Instruction^.Next;
      if ShouldVisit(Instruction,Position) then begin
       continue;
      end;
     end;
     opSAVE:begin
      case Argument of
       0:begin
        Push(Instruction,BitStateNFAWorkCaptures[Instruction^.Value],1);
        BitStateNFAWorkCaptures[Instruction^.Value]:=Position;
        Instruction:=Instruction^.Next;
        if ShouldVisit(Instruction,Position) then begin
         continue;
        end;
       end;
       1:begin
        BitStateNFAWorkCaptures[Instruction^.Value]:=Position;
       end;
      end;
     end;
     opBOL:begin
      if Satisfy(sfEmptyBeginLine,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opEOL:begin
      if Satisfy(sfEmptyEndLine,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opBOT:begin
      if Satisfy(sfEmptyBeginText,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opEOT:begin
      if Satisfy(sfEmptyEndText,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opBRK:begin
      if Satisfy(sfEmptyWordBoundary,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
     opNBRK:begin
      if Satisfy(sfEmptyNonWordBoundary,Position) then begin
       Instruction:=Instruction^.Next;
       if ShouldVisit(Instruction,Position) then begin
        continue;
       end;
      end;
     end;
    end;

    break;
   until false;

  end;

 end;
var VisitedLength:longword;
    Position,LastPosition,Counter:longint;
    StartInstruction:PFLREInstruction;
begin
 result:=BitStateNFAError;

 Len:=UntilExcludingPosition-StartPosition;
 if Len<1 then begin
  exit;
 end;

 BasePosition:=StartPosition;
 Position:=StartPosition;

 VisitedLength:=longword(qword((qword(Len+1)*longword(CountForwardInstructions))+31) shr 5);
 if longword(VisitedLength)>longword(SizeOf(TFLREBitStateNFAVisited) div SizeOf(longword)) then begin
  // Too big for 32kb visited bitmap
  exit;
 end;
 FillChar(BitStateNFAVisited[0],VisitedLength*SizeOf(longword),#0);

 if UnanchoredStart then begin
  StartInstruction:=UnanchoredStartInstruction;
 end else begin
  StartInstruction:=AnchoredStartInstruction;
 end;

 if TrySearch(StartInstruction,Position) then begin
  SetLength(Captures,CountParens);
  for Counter:=0 to CountParens-1 do begin
   Captures[Counter].Start:=BitStateNFAMatchCaptures[Counter*2];
   Captures[Counter].Length:=BitStateNFAMatchCaptures[(Counter*2)+1]-BitStateNFAMatchCaptures[Counter*2];
  end;
  result:=BitStateNFAMatch;
 end else begin
  result:=BitStateNFAFail;
 end;

end;

function TFLRE.SearchMatchDFA(const StartPosition,UntilExcludingPosition:longint;out MatchEnd:longint;const UnanchoredStart:boolean):longint;
var Position:longint;
    State,LastState:PFLREDFAState;
begin
 result:=DFAFail;
 if UnanchoredStart then begin
  State:=DFAUnanchoredStartState;
 end else begin
  State:=DFAAnchoredStartState;
 end;
 for Position:=StartPosition to UntilExcludingPosition-1 do begin
  LastState:=State;
  State:=State^.NextStates[ByteMap[byte(ansichar(Input[Position]))]];
  if not assigned(State) then begin
   State:=DFAProcessNextState(LastState,Input[Position],false);
   if not assigned(State) then begin
    result:=DFAError;
    exit;
   end;
  end;
  if (State^.Flags and (sfDFAMatchWins or sfDFADead))<>0 then begin
   if (State^.Flags and sfDFADead)<>0 then begin
    if result<>DFAMatch then begin
     result:=DFAFail;
    end;
    exit;
   end;
   if (State^.Flags and sfDFAMatchWins)<>0 then begin
    MatchEnd:=Position;
    result:=DFAMatch;
   end;
  end;
 end;
end;

function TFLRE.SearchMatchReversedDFA(const StartPosition,UntilIncludingPosition:longint;out MatchBegin:longint):longint;
var Position:longint;
    State,LastState:PFLREDFAState;
begin
 result:=DFAFail;
 State:=DFAReversedStartState;
 for Position:=StartPosition downto UntilIncludingPosition do begin
  LastState:=State;
  State:=State^.NextStates[ByteMap[byte(ansichar(Input[Position]))]];
  if not assigned(State) then begin
   State:=DFAProcessNextState(LastState,Input[Position],true);
   if not assigned(State) then begin
    result:=DFAError;
    exit;
   end;
  end;
  if (State^.Flags and (sfDFAMatchWins or sfDFADead))<>0 then begin
   if (State^.Flags and sfDFADead)<>0 then begin
    if result<>DFAMatch then begin
     result:=DFAFail;
    end;
    exit;
   end;
   if (State^.Flags and sfDFAMatchWins)<>0 then begin
    MatchBegin:=Position;
    result:=DFAMatch;
   end;
  end;
 end;
end;

function TFLRE.SearchMatch(var Captures:TFLRECaptures;StartPosition,UntilExcludingPosition:longint;UnanchoredStart:boolean):boolean;
var MatchBegin,MatchEnd:longint;
begin
 case SearchMatchDFA(StartPosition,UntilExcludingPosition,MatchEnd,UnanchoredStart) of
  DFAMatch:begin
   if UnanchoredStart then begin
    // For unanchored searchs, we must do also a "backward" DFA search
    case SearchMatchReversedDFA(MatchEnd,StartPosition,MatchBegin) of
     DFAMatch:begin
      if MatchBegin<StartPosition then begin
       MatchBegin:=StartPosition;
      end;
      if (CountParens=1) and not DFANeedVerification then begin
       // If we have only the root group capture without the need for the verification of the found, then don't execute the slower *NFA algorithms
       SetLength(Captures,1);
       Captures[0].Start:=MatchBegin;
       Captures[0].Length:=(MatchEnd-MatchBegin)+1;
       result:=true;
       exit;
      end else begin
       // Otherwise if we have group captures or if we do need verify the found, set the new stat position *NFA algorithms
       StartPosition:=MatchBegin;
       UnanchoredStart:=true;
      end;
     end;
    end;
   end;
   if (CountParens=1) and not (DFANeedVerification or UnanchoredStart) then begin
    // If we have only the root group capture without the need for the verification of the found, then don't execute the slower *NFA algorithms
    SetLength(Captures,1);
    Captures[0].Start:=StartPosition;
    Captures[0].Length:=(MatchEnd-StartPosition)+1;
    result:=true;
    exit;
   end else begin
    // Otherwise if we have group captures or if we do need verify the found, limit search length for the slower *NFA algorithms
    if UntilExcludingPosition>(MatchEnd+1) then begin
     UntilExcludingPosition:=MatchEnd+1;
    end;
   end;
  end;
  DFAFail:begin
   // No DFA match => stop
   result:=false;
   exit;
  end;
  else {DFAError:}begin
   // Internal error?
{$ifdef debug}
   Assert(false,'Internal error in DFA code');
{$endif}
  end;
 end;
 if OnePassNFAReady and not UnanchoredStart then begin
  result:=SearchMatchOnePassNFA(Captures,StartPosition,UntilExcludingPosition);
 end else begin
  if BitStateNFAReady then begin
   case SearchMatchBitStateNFA(Captures,StartPosition,UntilExcludingPosition,UnanchoredStart) of
    BitStateNFAFail:begin
     result:=false;
     exit;
    end;
    BitStateNFAMatch:begin
     result:=true;
     exit;
    end;
  (*else{BitStateNFAError:}begin
    end;*)
   end;
  end;
  result:=SearchMatchParallelNFA(Captures,StartPosition,UntilExcludingPosition,UnanchoredStart);
 end;
end;

function TFLRE.PtrMatch(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
begin
 Input:=AInput;
 InputLength:=AInputLength;
 result:=SearchMatch(Captures,StartPosition,InputLength,false);
end;

function TFLRE.PtrMatchNext(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
var CurrentPosition:longint;
begin
 result:=false;
 Input:=AInput;
 InputLength:=AInputLength;
 CurrentPosition:=StartPosition;
 if (CurrentPosition>=0) and (CurrentPosition<InputLength) then begin
  repeat
   if FixedStringIsWholeRegExp or not BeginningAnchor then begin
    if (CountPrefixCharClasses<=FixedStringLength) and not (rfCASEINSENSITIVE in Flags) then begin
     case FixedStringLength of
      1:begin
       CurrentPosition:=PtrPosChar(FixedString[1],Input,InputLength,CurrentPosition);
      end;
      2..31:begin
       CurrentPosition:=PtrPosPattern(FixedStringLength,Input,InputLength,FixedStringPatternBitMasks,CurrentPosition);
      end;
      else begin
       CurrentPosition:=PtrPosBoyerMoore(FixedString,Input,InputLength,FixedStringBoyerMooreSkip,FixedStringBoyerMooreNext,CurrentPosition);
      end;
     end;
     if (CurrentPosition<0) or (CurrentPosition>=InputLength) or (BeginningAnchor and (CurrentPosition<>0)) then begin
      exit;
     end;
     if FixedStringIsWholeRegExp and (CountParens=1) and not DFANeedVerification then begin
      SetLength(Captures,1);
      Captures[0].Start:=CurrentPosition;
      Captures[0].Length:=FixedStringLength;
      result:=true;
      exit;
     end;
    end else if CountPrefixCharClasses>0 then begin
     case CountPrefixCharClasses of
      1:begin
       while (CurrentPosition<InputLength) and not (Input[CurrentPosition] in PrefixCharClasses[0]) do begin
        inc(CurrentPosition);
       end;
      end;
      else begin
       CurrentPosition:=PtrPosPattern(CountPrefixCharClasses,Input,InputLength,PrefixPatternBitMasks,CurrentPosition);
      end;
     end;
     if (CurrentPosition<0) or (CurrentPosition>=InputLength) or (BeginningAnchor and (CurrentPosition<>0)) then begin
      break;
     end;
    end;
   end;
   if SearchMatch(Captures,CurrentPosition,InputLength,DoUnanchoredStart) then begin
    result:=true;
    exit;
   end;
   inc(CurrentPosition);
  until (CurrentPosition>=InputLength) or (BeginningWildcardLoop or BeginningAnchor);
 end;
end;

function TFLRE.PtrMatchAll(const AInput:pointer;const AInputLength:longint;var Captures:TFLREMultiCaptures;const StartPosition:longint=0;Limit:longint=-1):boolean;
var CurrentPosition,CountMultiCaptures,Next:longint;
    MatchResult:TFLRECaptures;
begin
 result:=false;
 MatchResult:=nil;
 CountMultiCaptures:=0;
 try
  SetLength(Captures,0);
  CurrentPosition:=StartPosition;
  if CurrentPosition>=0 then begin
   while (CurrentPosition<AInputLength) and (Limit<>0) and PtrMatchNext(AInput,AInputLength,MatchResult,CurrentPosition) do begin
    Next:=CurrentPosition+1;
    CurrentPosition:=MatchResult[0].Start+MatchResult[0].Length;
    if CurrentPosition<Next then begin
     CurrentPosition:=Next;
    end;
    if CountMultiCaptures>=length(Captures) then begin
     SetLength(Captures,(CountMultiCaptures+1)*2);
    end;
    Captures[CountMultiCaptures]:=copy(MatchResult);
    inc(CountMultiCaptures);
    if Limit>0 then begin
     dec(Limit);
    end;
   end;
   result:=CountMultiCaptures>0;
  end;
 finally
  SetLength(MatchResult,0);
  SetLength(Captures,CountMultiCaptures);
 end;
end;

function TFLRE.PtrReplaceAll(const AInput:pointer;const AInputLength:longint;const AReplacement:pointer;const AReplacementLength:longint;const StartPosition:longint=0;Limit:longint=-1):ansistring;
var CurrentPosition,Next,LastPosition,i,j,e:longint;
    Captures:TFLRECaptures;
    SimpleReplacement:boolean;
    c,cc:ansichar;
begin
 result:='';
 Captures:=nil;
 try                       
  SimpleReplacement:=(PtrPosChar('$',AReplacement,AReplacementLength)<0) and (PtrPosChar('\',AReplacement,AReplacementLength)<0);
  CurrentPosition:=StartPosition;
  LastPosition:=CurrentPosition;
  if CurrentPosition>=0 then begin
   while (CurrentPosition<AInputLength) and (Limit<>0) and PtrMatchNext(AInput,AInputLength,Captures,CurrentPosition) do begin
    Next:=CurrentPosition+1;
    if (Captures[0].Start+Captures[0].Length)=LastPosition then begin
     CurrentPosition:=Captures[0].Start+Captures[0].Length;
     if CurrentPosition<Next then begin
      CurrentPosition:=Next;
     end;
    end else begin
     if LastPosition<Captures[0].Start then begin
      result:=result+PtrCopy(PAnsiChar(AInput),LastPosition,Captures[0].Start-LastPosition);
     end;
     CurrentPosition:=Captures[0].Start+Captures[0].Length;
     if CurrentPosition<Next then begin
      CurrentPosition:=Next;
     end;
     LastPosition:=CurrentPosition;
     if SimpleReplacement then begin
      result:=result+PtrCopy(PAnsiChar(AReplacement),0,AReplacementLength);
     end else begin
      i:=0;
      while i<AReplacementLength do begin
       c:=PAnsiChar(AReplacement)[i];
       case c of
        '$','\':begin
         cc:=c;
         inc(i);
         if i<AReplacementLength then begin
          c:=PAnsiChar(AReplacement)[i];
          case c of
           '$':begin
            if cc='$' then begin
             result:=result+c;
             inc(i);
            end else begin
             result:=result+'\$';
             inc(i);
            end;
           end;
           '\':begin
            if cc='\' then begin
             result:=result+c;
             inc(i);
            end else begin
             result:=result+'$\';
             inc(i);
            end;
           end;
           '&':begin
            result:=result+PtrCopy(PAnsiChar(AInput),Captures[0].Start,Captures[0].Length);
            inc(i);
           end;
           '`':begin
            result:=result+PtrCopy(PAnsiChar(AInput),0,Captures[0].Start-1);
            inc(i);
           end;
           '''':begin
            result:=result+PtrCopy(PAnsiChar(AInput),Captures[0].Start+Captures[0].Length,(AInputLength-(Captures[0].Start+Captures[0].Length))+1);
            inc(i);
           end;
           '_':begin
            result:=result+AnsiString(PAnsiChar(AInput));
            inc(i);
           end;
           '-':begin
            if length(Captures)>1 then begin
             result:=result+PtrCopy(PAnsiChar(AInput),Captures[1].Start,Captures[1].Length);
            end;
            inc(i);
           end;
           '+':begin
            if length(Captures)>1 then begin
             e:=length(Captures)-1;
             result:=result+PtrCopy(PAnsiChar(AInput),Captures[e].Start,Captures[e].Length);
            end;
            inc(i);
           end;
           'g':begin
            if cc='\' then begin
             e:=-1;
             inc(i);
             j:=i;
             while i<AReplacementLength do begin
              if PAnsiChar(AReplacement)[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
               inc(i);
              end else begin
               break;
              end;
             end;
             if j<i then begin
              e:=NamedGroupStringIntegerPairHashMap.GetValue(PtrCopy(PAnsiChar(AReplacement),j,i-j));
             end;
             if e<0 then begin
              result:=result+cc+'g';
              i:=j;
             end else begin
              if (e>=0) and (e<length(Captures)) then begin
               result:=result+PtrCopy(PAnsiChar(AInput),Captures[e].Start,Captures[e].Length);
              end;
             end;
            end else begin
             result:=result+cc+'g';
             inc(i);
            end;
           end;
           '{':begin
            e:=-1;
            inc(i);
            j:=i;
            if i<AReplacementLength then begin
             case PAnsiChar(AReplacement)[i] of
              '0'..'9':begin
               e:=0;
               while i<AReplacementLength do begin
                c:=PAnsiChar(AReplacement)[i];
                case c of
                 '0'..'9':begin
                  e:=(e*10)+(ord(c)-ord('0'));
                  inc(i);
                 end;
                 else begin
                  break;
                 end;
                end;
               end;
               if (i<AReplacementLength) and (PAnsiChar(AReplacement)[i]='}') then begin
                inc(i);
               end else begin
                e:=-1;
               end;
              end;
              else begin
               while i<AReplacementLength do begin
                if PAnsiChar(AReplacement)[i] in ['a'..'z','A'..'Z','_','0'..'9'] then begin
                 inc(i);
                end else begin
                 break;
                end;
               end;
               if (j<i) and (PAnsiChar(AReplacement)[i]='}') then begin
                e:=NamedGroupStringIntegerPairHashMap.GetValue(PtrCopy(PAnsiChar(AReplacement),j,i-j));
                inc(i);
               end else begin
                e:=-1;
               end;
              end;
             end;
            end;
            if e<0 then begin
             result:=result+cc+'{';
             i:=j;
            end else begin
             if (e>=0) and (e<length(Captures)) then begin
              result:=result+PtrCopy(PAnsiChar(AInput),Captures[e].Start,Captures[e].Length);
             end;
            end;
           end;
           '0'..'9':begin
            if length(Captures)<10 then begin
             e:=ord(c)-ord('0');
             inc(i);
            end else begin
             e:=0;
             while i<AReplacementLength do begin
              c:=PAnsiChar(AReplacement)[i];
              case c of
               '0'..'9':begin
                e:=(e*10)+(ord(c)-ord('0'));
                inc(i);
               end;
               else begin
                break;
               end;
              end;
             end;
            end;
            if (e>=0) and (e<length(Captures)) then begin
             result:=result+PtrCopy(PAnsiChar(AInput),Captures[e].Start,Captures[e].Length);
            end;
           end;
           else begin
            result:=result+c;
            inc(i);
           end;
          end;
         end;
        end;
        else begin
         result:=result+c;
         inc(i);
        end;
       end;
      end;
     end;
    end;
    if Limit>0 then begin
     dec(Limit);
    end;
   end;
   if LastPosition<AInputLength then begin
    result:=result+PtrCopy(PAnsiChar(AInput),LastPosition,AInputLength-LastPosition);
   end;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function TFLRE.Match(const AInput:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
var Counter:longint;
begin
 result:=PtrMatch(pansichar(@AInput[1]),length(AInput),Captures,StartPosition-1);
 for Counter:=0 to length(Captures)-1 do begin
  if Captures[Counter].Length>0 then begin
   inc(Captures[Counter].Start);
  end;
 end;
end;

function TFLRE.MatchNext(const AInput:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
var Counter:longint;
begin
 result:=PtrMatchNext(pansichar(@AInput[1]),length(AInput),Captures,StartPosition-1);
 for Counter:=0 to length(Captures)-1 do begin
  if Captures[Counter].Length>0 then begin
   inc(Captures[Counter].Start);
  end;
 end;
end;

function TFLRE.MatchAll(const AInput:ansistring;var Captures:TFLREMultiCaptures;const StartPosition:longint=1;Limit:longint=-1):boolean;
var Counter,SubCounter:longint;
begin
 result:=PtrMatchAll(pansichar(@AInput[1]),length(AInput),Captures,StartPosition-1,Limit);
 for Counter:=0 to length(Captures)-1 do begin
  for SubCounter:=0 to length(Captures[Counter])-1 do begin
   if Captures[Counter,SubCounter].Length>0 then begin
    inc(Captures[Counter,SubCounter].Start);
   end;
  end;
 end;
end;

function TFLRE.ReplaceAll(const AInput,AReplacement:ansistring;const StartPosition:longint=1;Limit:longint=-1):ansistring;
begin
 result:=PtrReplaceAll(pansichar(@AInput[1]),length(AInput),pansichar(@AReplacement[1]),length(AReplacement),StartPosition-1,Limit);
end;

procedure InitializeFLRE;
const FLRESignature:ansistring=' FLRE - yet another efficient, principled regular expression library - Version '+FLREVersionStr+' - Copyright (C) 2015, Benjamin ''BeRo'' Rosseaux - benjamin@rosseaux.com - http://www.rosseaux.com ';
 procedure InitializeUTF8DFA;
 type TAnsiCharSet=set of ansichar;
{$ifdef FLREStrictUTF8}
{ c0  8 11000000   | d0  2 11(010000) | e0 10 11100000   | f0 11 11110000
  c1  8 11000001   | d1  2 11(010001) | e1  3 111(00001) | f1  6 111100(01)
  c2  2 11(000010) | d2  2 11(010010) | e2  3 111(00010) | f2  6 111100(10)
  c3  2 11(000011) | d3  2 11(010011) | e3  3 111(00011) | f3  6 111100(11)
  c4  2 11(000100) | d4  2 11(010100) | e4  3 111(00100) | f4  5 11110(100)
  c5  2 11(000101) | d5  2 11(010101) | e5  3 111(00101) | f5  8 11110101
  c6  2 11(000110) | d6  2 11(010110) | e6  3 111(00110) | f6  8 11110110
  c7  2 11(000111) | d7  2 11(010111) | e7  3 111(00111) | f7  8 11110111
  c8  2 11(001000) | d8  2 11(011000) | e8  3 111(01000) | f8  8 11111000
  c9  2 11(001001) | d9  2 11(011001) | e9  3 111(01001) | f9  8 11111001
  ca  2 11(001010) | da  2 11(011010) | ea  3 111(01010) | fa  8 11111010
  cb  2 11(001011) | db  2 11(011011) | eb  3 111(01011) | fb  8 11111011
  cc  2 11(001100) | dc  2 11(011100) | ec  3 111(01100) | fc  8 11111100
  cd  2 11(001101) | dd  2 11(011101) | ed  4 1110(1101) | fd  8 11111101
  ce  2 11(001110) | de  2 11(011110) | ee  3 111(01110) | fe  8 11111110
  cf  2 11(001111) | df  2 11(011111) | ef  3 111(01111) | ff  8 11111111  }
 const cc007F=$0;
       cc808F=$1;
       ccC2DF=$2;
       ccE1ECEEEF=$3;
       ccED=$4;
       ccF4=$5;
       ccF1F3=$6;
       ccA0BF=$7;
       ccC0C1F5FF=$8;
       cc909F=$9;
       ccE0=$a;
       ccF0=$b;
       tsBEGIN=0;
       tsERROR=1;
       tsSINGLETAIL=2;
       tsDOUBLETAIL=3;
       tsDOUBLETAILwithA0BFonly=4;
       tsDOUBLETAILwith809FFonly=5;
       tsTRIPLETAILwith90BFonly=6;
       tsTRIPLETAIL=7;
       tsTRIPLETAILwith808Fonly=8;
{$else}
 const cc007F=$0;
       cc80BF=$1; // Tail
       ccC0DF=$3; // ($ff shr $03)=$1f
       ccE0EF=$4; // ($ff shr $04)=$0f
       ccF0F7=$5; // ($ff shr $05)=$07
       ccF8FB=$6; // ($ff shr $06)=$03
       ccFCFD=$7; // ($ff shr $07)=$01
       ccFEFF=$8; // ($ff shr $08)=$00
       tsBEGIN=0;
       tsERROR=1;
       tsSINGLETAIL=2;
       tsDOUBLETAIL=3;
       tsTRIPLETAIL=4;
       tsQUADTAIL=5;
       tsQUINTAIL=6;
{$endif}
       tsMUL=16;
  procedure AssignCharsetToCharClass(const Charset:TAnsiCharSet;CharClass:byte);
  var c:ansichar;
  begin
   for c:=low(ansichar) to high(ansichar) do begin
    if c in Charset then begin
     UTF8DFACharClasses[c]:=CharClass;
    end;
   end;
  end;
  procedure AddTranslation(FromState,AtCharClass,ToState:byte);
  begin
   UTF8DFATransitions[(FromState*tsMUL)+AtCharClass]:=ToState*tsMUL;
  end;
 var i:longint;
 begin
  FillChar(UTF8DFACharClasses,sizeof(TUTF8Chars),#0);
  FillChar(UTF8DFATransitions,sizeof(TUTF8Bytes),#0);
  begin
{$ifdef FLREStrictUTF8}
   AssignCharsetToCharClass([#$00..#$7f],cc007F);
   AssignCharsetToCharClass([#$80..#$8f],cc808F);
   AssignCharsetToCharClass([#$90..#$9f],cc909F);
   AssignCharsetToCharClass([#$a0..#$bf],ccA0BF);
   AssignCharsetToCharClass([#$c0..#$c1],ccC0C1F5FF);
   AssignCharsetToCharClass([#$c2..#$df],ccC2DF);
   AssignCharsetToCharClass([#$e0],ccE0);
   AssignCharsetToCharClass([#$e1..#$ec,#$ee..#$ef],ccE1ECEEEF);
   AssignCharsetToCharClass([#$ed],ccED);
   AssignCharsetToCharClass([#$f0],ccF0);
   AssignCharsetToCharClass([#$f1..#$f3],ccF1F3);
   AssignCharsetToCharClass([#$f4],ccF4);
   AssignCharsetToCharClass([#$f5..#$ff],ccC0C1F5FF);
{$else}
   AssignCharsetToCharClass([#$00..#$7f],cc007F);
   AssignCharsetToCharClass([#$80..#$bf],cc80BF);
   AssignCharsetToCharClass([#$c0..#$df],ccC0DF);
   AssignCharsetToCharClass([#$e0..#$ef],ccE0EF);
   AssignCharsetToCharClass([#$f0..#$f7],ccF0F7);
   AssignCharsetToCharClass([#$f8..#$fb],ccF8FB);
   AssignCharsetToCharClass([#$fc..#$fd],ccFCFD);
   AssignCharsetToCharClass([#$fe..#$ff],ccFEFF);
{$endif}
  end;
  begin
   for i:=low(TUTF8Bytes) to high(TUTF8Bytes) do begin
    UTF8DFATransitions[i]:=tsERROR*tsMUL;
   end;
{$ifdef FLREStrictUTF8}
   begin
    AddTranslation(tsBEGIN,cc007F,tsBEGIN);
    AddTranslation(tsBEGIN,cc808F,tsERROR);
    AddTranslation(tsBEGIN,cc909F,tsERROR);
    AddTranslation(tsBEGIN,ccA0BF,tsERROR);
    AddTranslation(tsBEGIN,ccC2DF,tsSINGLETAIL);
    AddTranslation(tsBEGIN,ccE0,tsDOUBLETAILwithA0BFonly);
    AddTranslation(tsBEGIN,ccE1ECEEEF,tsDOUBLETAIL);
    AddTranslation(tsBEGIN,ccED,tsDOUBLETAILwith809FFonly);
    AddTranslation(tsBEGIN,ccF0,tsTRIPLETAILwith90BFonly);
    AddTranslation(tsBEGIN,ccF1F3,tsTRIPLETAIL);
    AddTranslation(tsBEGIN,ccF4,tsTRIPLETAILwith808Fonly);
    AddTranslation(tsBEGIN,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsERROR,cc007F,tsERROR);
    AddTranslation(tsERROR,cc808F,tsERROR);
    AddTranslation(tsERROR,cc909F,tsERROR);
    AddTranslation(tsERROR,ccA0BF,tsERROR);
    AddTranslation(tsERROR,ccC2DF,tsERROR);
    AddTranslation(tsERROR,ccE0,tsERROR);
    AddTranslation(tsERROR,ccE1ECEEEF,tsERROR);
    AddTranslation(tsERROR,ccED,tsERROR);
    AddTranslation(tsERROR,ccF0,tsERROR);
    AddTranslation(tsERROR,ccF1F3,tsERROR);
    AddTranslation(tsERROR,ccF4,tsERROR);
    AddTranslation(tsERROR,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
    AddTranslation(tsSINGLETAIL,cc808F,tsBEGIN);
    AddTranslation(tsSINGLETAIL,cc909F,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccA0BF,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE0,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccED,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF0,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF4,tsERROR);
    AddTranslation(tsSINGLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAIL,cc808F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,cc909F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccA0BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccED,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc808F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,cc909F,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccA0BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccED,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAILwithA0BFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAILwith809FFonly,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,cc808F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwith809FFonly,cc909F,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccA0BF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccC2DF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccE0,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccED,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF0,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF1F3,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccF4,tsERROR);
    AddTranslation(tsDOUBLETAILwith809FFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAILwith90BFonly,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,cc808F,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,cc909F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccA0BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccED,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAILwith90BFonly,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAIL,cc808F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,cc909F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccA0BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccED,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccC0C1F5FF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAILwith808Fonly,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,cc808F,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAILwith808Fonly,cc909F,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccA0BF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccC2DF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccE0,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccE1ECEEEF,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccED,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF0,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF1F3,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccF4,tsERROR);
    AddTranslation(tsTRIPLETAILwith808Fonly,ccC0C1F5FF,tsERROR);
   end;
  end;
{$else}
   begin
    AddTranslation(tsBEGIN,cc007F,tsBEGIN);
    AddTranslation(tsBEGIN,cc80BF,tsERROR);
    AddTranslation(tsBEGIN,ccC0DF,tsSINGLETAIL);
    AddTranslation(tsBEGIN,ccE0EF,tsDOUBLETAIL);
    AddTranslation(tsBEGIN,ccF0F7,tsTRIPLETAIL);
    AddTranslation(tsBEGIN,ccF8FB,tsQUADTAIL);
    AddTranslation(tsBEGIN,ccFCFD,tsQUINTAIL);
    AddTranslation(tsBEGIN,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsERROR,cc007F,tsERROR);
    AddTranslation(tsERROR,cc80BF,tsERROR);
    AddTranslation(tsERROR,ccC0DF,tsERROR);
    AddTranslation(tsERROR,ccE0EF,tsERROR);
    AddTranslation(tsERROR,ccF0F7,tsERROR);
    AddTranslation(tsERROR,ccF8FB,tsERROR);
    AddTranslation(tsERROR,ccFCFD,tsERROR);
    AddTranslation(tsERROR,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
    AddTranslation(tsSINGLETAIL,cc80BF,tsBEGIN);
    AddTranslation(tsSINGLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsSINGLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsSINGLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsSINGLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
    AddTranslation(tsDOUBLETAIL,cc80BF,tsSINGLETAIL);
    AddTranslation(tsDOUBLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsDOUBLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
    AddTranslation(tsTRIPLETAIL,cc80BF,tsDOUBLETAIL);
    AddTranslation(tsTRIPLETAIL,ccC0DF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccE0EF,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF0F7,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccF8FB,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccFCFD,tsERROR);
    AddTranslation(tsTRIPLETAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsQUADTAIL,cc007F,tsERROR);
    AddTranslation(tsQUADTAIL,cc80BF,tsTRIPLETAIL);
    AddTranslation(tsQUADTAIL,ccC0DF,tsERROR);
    AddTranslation(tsQUADTAIL,ccE0EF,tsERROR);
    AddTranslation(tsQUADTAIL,ccF0F7,tsERROR);
    AddTranslation(tsQUADTAIL,ccF8FB,tsERROR);
    AddTranslation(tsQUADTAIL,ccFCFD,tsERROR);
    AddTranslation(tsQUADTAIL,ccFEFF,tsERROR);
   end;
   begin
    AddTranslation(tsQUINTAIL,cc007F,tsERROR);
    AddTranslation(tsQUINTAIL,cc80BF,tsQUADTAIL);
    AddTranslation(tsQUINTAIL,ccC0DF,tsERROR);
    AddTranslation(tsQUINTAIL,ccE0EF,tsERROR);
    AddTranslation(tsQUINTAIL,ccF0F7,tsERROR);
    AddTranslation(tsQUINTAIL,ccF8FB,tsERROR);
    AddTranslation(tsQUINTAIL,ccFCFD,tsERROR);
    AddTranslation(tsQUINTAIL,ccFEFF,tsERROR);
   end;
  end;
{$endif}
 end;
 procedure InitializeUnicode;
 var i,l,h:longword;
     Count:longint;
     s:ansistring;
  procedure AddRange(Table,FirstChar,LastChar:longword);
  begin
   if (Count+1)>length(UnicodeCharRangeClasses[Table]) then begin
    SetLength(UnicodeCharRangeClasses[Table],(Count+4097) and not 4095);
   end;
   UnicodeCharRangeClasses[Table,Count,0]:=FirstChar;
   UnicodeCharRangeClasses[Table,Count,1]:=LastChar;
   inc(Count);
  end;
  procedure AddChar(Table,TheChar:longword);
  begin
   AddRange(Table,TheChar,TheChar);
  end;
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
   AddRange(ucrWHITESPACES,$0009,$000d);
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
   AddChar(ucrWHITESPACES,$feff);
{  l:=$ffffffff;
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
   end;}
   SetLength(UnicodeCharRangeClasses[ucrWHITESPACES],Count);
  end;
  begin                                           
   UnicodeClassHashMap:=TFLREStringIntegerPairHashMap.Create;
   for i:=0 to FLREUnicodeCategoryCount-1 do begin
    s:=FLREUnicodeCategoryIDs[i];
    UnicodeClassHashMap.SetValue(UTF8LowerCase(s),1 shl i);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Is'+s),1 shl i);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('In'+s),1 shl i);
   end;
   begin
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Lu'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Uppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('UppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Ll'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Lowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('LowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Lt'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Titlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('TitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Lm'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Modifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('ModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Lo'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Other_Letter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('OtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('L'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Mn'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Non_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('NonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Me'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Enclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('EnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Mc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Spacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('SpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('M'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Mark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Nd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Decimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('DecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Nl'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Letter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('LetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('No'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Other_Number'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('OtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('N'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Number'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Zs'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Space_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('SpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Zl'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Line_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('LineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Zp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Paragraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('ParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Z'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Seperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Cc'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Control'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Cf'),1 shl FLRE_CT_FORMAT);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Format'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Co'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Private_Use'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('PrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Cs'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Surrogate'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Cn'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Unassigned'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('C'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Other'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Pd'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Dash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('DashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Ps'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Open_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('OpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Pe'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Close_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('ClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Pi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Initial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Pf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Final_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('FinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Pc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Connector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('ConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Po'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Other_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('OtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('P'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Punctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Sm'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Math_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('MathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Sc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Currency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('CurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Sk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Modifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('ModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('So'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Other_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('OtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('S'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('Symbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   end;
   begin
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLu'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsUppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsUppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLl'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLt'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsTitlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsTitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLm'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsModifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLo'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOther_Letter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsL'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMn'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNon_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMe'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsEnclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsEnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSpacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsM'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsDecimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsDecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNl'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLetter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNo'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOther_Number'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsN'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsNumber'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsZs'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSpace_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsZl'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLine_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsLineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsZp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsParagraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsZ'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSeperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCc'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsControl'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCf'),1 shl FLRE_CT_FORMAT);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsFormat'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCo'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPrivate_Use'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCs'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSurrogate'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCn'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsUnassigned'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsC'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOther'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPd'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsDash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsDashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPs'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOpen_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPe'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsClose_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsInitial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsInitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsFinal_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsFinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsConnector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPo'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOther_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsP'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsPunctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSm'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMath_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsMathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCurrency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsCurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsModifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSo'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOther_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsOtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsS'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('IsSymbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   end;
   begin
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLu'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InUppercase_Letter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InUppercaseLetter'),1 shl FLRE_CT_UPPERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLl'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLowercase_Letter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLowercaseLetter'),1 shl FLRE_CT_LOWERCASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLt'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InTitlecase_Letter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InTitlecaseLetter'),1 shl FLRE_CT_TITLECASE_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLm'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InModifier_Letter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InModifierLetter'),1 shl FLRE_CT_MODIFIER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLo'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOther_Letter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOtherLetter'),1 shl FLRE_CT_OTHER_LETTER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InL'),(1 shl FLRE_CT_UPPERCASE_LETTER) or (1 shl FLRE_CT_LOWERCASE_LETTER) or (1 shl FLRE_CT_TITLECASE_LETTER) or (1 shl FLRE_CT_MODIFIER_LETTER) or (1 shl FLRE_CT_OTHER_LETTER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMn'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNon_Spacing_Mark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNonSpacingMark'),1 shl FLRE_CT_NON_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMe'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InEnclosing_Mark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InEnclosingMark'),1 shl FLRE_CT_ENCLOSING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMc'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSpacing_Combining_Mark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSpacingCombiningMark'),1 shl FLRE_CT_COMBINING_SPACING_MARK);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InM'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMark'),(1 shl FLRE_CT_NON_SPACING_MARK) or (1 shl FLRE_CT_ENCLOSING_MARK) or (1 shl FLRE_CT_COMBINING_SPACING_MARK));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNd'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InDecimal_Digit_Number'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InDecimalDigitNumber'),1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNl'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLetter_Number'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLetterNumber'),1 shl FLRE_CT_LETTER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNo'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOther_Number'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOtherNumber'),1 shl FLRE_CT_OTHER_NUMBER);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InN'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InNumber'),(1 shl FLRE_CT_DECIMAL_DIGIT_NUMBER) or (1 shl FLRE_CT_LETTER_NUMBER) or (1 shl FLRE_CT_OTHER_NUMBER));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InZs'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSpace_Separator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSpaceSeparator'),1 shl FLRE_CT_SPACE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InZl'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLine_Separator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InLineSeparator'),1 shl FLRE_CT_LINE_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InZp'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InParagraph_Separator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InParagraphSeparator'),1 shl FLRE_CT_PARAGRAPH_SEPARATOR);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InZ'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSeperator'),(1 shl FLRE_CT_SPACE_SEPARATOR) or (1 shl FLRE_CT_LINE_SEPARATOR) or (1 shl FLRE_CT_PARAGRAPH_SEPARATOR));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCc'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InControl'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCf'),1 shl FLRE_CT_FORMAT);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InFormat'),1 shl FLRE_CT_CONTROL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCo'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPrivate_Use'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPrivateUse'),1 shl FLRE_CT_PRIVATE_USE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCs'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSurrogate'),1 shl FLRE_CT_SURROGATE);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCn'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InUnassigned'),1 shl FLRE_CT_UNASSIGNED);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InC'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOther'),(1 shl FLRE_CT_CONTROL) or (1 shl FLRE_CT_FORMAT) or (1 shl FLRE_CT_PRIVATE_USE) or (1 shl FLRE_CT_SURROGATE) or (1 shl FLRE_CT_UNASSIGNED));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPd'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InDash_Punctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InDashPunctuation'),1 shl FLRE_CT_DASH_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPs'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOpen_Punctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOpenPunctuation'),1 shl FLRE_CT_START_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPe'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InClose_Punctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InClosePunctuation'),1 shl FLRE_CT_END_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPi'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InInitial_Punctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InInitialPunctuation'),1 shl FLRE_CT_INITIAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPf'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InFinal_Punctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InFinalPunctuation'),1 shl FLRE_CT_FINAL_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPc'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InConnector_Punctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InConnectorPunctuation'),1 shl FLRE_CT_CONNECTOR_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPo'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOther_Punctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOtherPunctuation'),1 shl FLRE_CT_OTHER_PUNCTUATION);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InP'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InPunctuation'),(1 shl FLRE_CT_DASH_PUNCTUATION) or (1 shl FLRE_CT_START_PUNCTUATION) or (1 shl FLRE_CT_END_PUNCTUATION) or (1 shl FLRE_CT_INITIAL_PUNCTUATION) or (1 shl FLRE_CT_FINAL_PUNCTUATION) or (1 shl FLRE_CT_CONNECTOR_PUNCTUATION) or (1 shl FLRE_CT_OTHER_PUNCTUATION));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSm'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMath_Symbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InMathSymbol'),1 shl FLRE_CT_MATH_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSc'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCurrency_Symbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InCurrencySymbol'),1 shl FLRE_CT_CURRENCY_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSk'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InModifier_Symbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InModifierSymbol'),1 shl FLRE_CT_MODIFIER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSo'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOther_Symbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InOtherSymbol'),1 shl FLRE_CT_OTHER_SYMBOL);
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InS'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
    UnicodeClassHashMap.SetValue(UTF8LowerCase('InSymbol'),(1 shl FLRE_CT_MATH_SYMBOL) or (1 shl FLRE_CT_CURRENCY_SYMBOL) or (1 shl FLRE_CT_MODIFIER_SYMBOL) or (1 shl FLRE_CT_OTHER_SYMBOL));
   end;
  end;
  begin
   UnicodeScriptHashMap:=TFLREStringIntegerPairHashMap.Create;
   for i:=FLREUnicodeScriptCommon to FLREUnicodeScriptCount-1 do begin
    s:=FLREUnicodeScriptIDs[i];
    UnicodeScriptHashMap.SetValue(UTF8LowerCase(s),i);
    UnicodeScriptHashMap.SetValue(UTF8LowerCase('In'+s),i);
    UnicodeScriptHashMap.SetValue(UTF8LowerCase('Is'+s),i);
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    UnicodeScriptHashMap.SetValue(UTF8LowerCase(s),i);
    UnicodeScriptHashMap.SetValue(UTF8LowerCase('In'+s),i);
    UnicodeScriptHashMap.SetValue(UTF8LowerCase('Is'+s),i);
   end;
  end;
  begin
   UnicodeBlockHashMap:=TFLREStringIntegerPairHashMap.Create;
   for i:=FLREUnicodeScriptCommon to FLREUnicodeBlockCount-1 do begin
    s:=AnsiString(StringReplace(String(FLREUnicodeBlocks[i].Name),' ','_',[rfREPLACEALL]));
    UnicodeBlockHashMap.SetValue(UTF8LowerCase(s),i);
    UnicodeBlockHashMap.SetValue(UTF8LowerCase('In'+s),i);
    UnicodeBlockHashMap.SetValue(UTF8LowerCase('Is'+s),i);
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    UnicodeBlockHashMap.SetValue(UTF8LowerCase(s),i);
    UnicodeBlockHashMap.SetValue(UTF8LowerCase('In'+s),i);
    UnicodeBlockHashMap.SetValue(UTF8LowerCase('Is'+s),i);
   end;
  end;
 end;
begin
 if (not FLREInitialized) and (length(FLRESignature)>0) then begin
  FLREInitialized:=true;
  InitializeUTF8DFA;
  InitializeUnicode;
 end;
end;

procedure FinalizeFLRE;
var i:longint;
begin
 if FLREInitialized then begin
  for i:=low(TUnicodeCharRangeClasses) to high(TUnicodeCharRangeClasses) do begin
   SetLength(UnicodeCharRangeClasses[i],0);
  end;
  FreeAndNil(UnicodeClassHashMap);
  FreeAndNil(UnicodeScriptHashMap);
  FreeAndNil(UnicodeBlockHashMap);
  FLREInitialized:=false;
 end;
end;

initialization
 InitializeFLRE;
finalization
 FinalizeFLRE;
end.


