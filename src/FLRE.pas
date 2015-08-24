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

uses SysUtils,Classes,Math,SyncObjs,FLREUnicode;

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

     TFLRE=class;

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
      Next:PFLREInstruction;
      OtherNext:PFLREInstruction;
     end;

     TFLREInstructions=array of TFLREInstruction;

     TPFLREInstructions=array of PFLREInstruction;

     PPFLREInstructionsStatic=^TPFLREInstructionsStatic;
     TPFLREInstructionsStatic=array[0..65535] of PFLREInstruction;

     PFLREParallelNFAStateItem=^TFLREParallelNFAStateItem;
     TFLREParallelNFAStateItem=longint;

     TFLREParallelNFAStateItems=array of TFLREParallelNFAStateItem;

     PFLREParallelNFAState=^TFLREParallelNFAState;
     TFLREParallelNFAState=record
      Next:PFLREParallelNFAState;
      ReferenceCounter:longint;
      Count:longint;
      BitState:longword;
      SubMatches:TFLREParallelNFAStateItems;
     end;

     PFLREThread=^TFLREThread;
     TFLREThread=record
      Instruction:PFLREInstruction;
      State:PFLREParallelNFAState;
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
    
     TFLREStringIntegerPairHashMapData=int64;

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

     TFLREInstructionGenerations=array of int64;

     TFLREThreadLocalStorage=class
      private
       AllNext:TFLREThreadLocalStorage;
       FreeNext:TFLREThreadLocalStorage;
      public
       Instance:TFLRE;

       Input:pansichar;
       InputLength:longint;

       ThreadLists:TFLREThreadLists;

       Generation:int64;
       InstructionGenerations:TFLREInstructionGenerations;

       FreeSubMatches:PFLREParallelNFAState;
       AllSubMatches:TList;

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

       constructor Create(AInstance:TFLRE);
       destructor Destroy; override;

       function GetSatisfyFlags(const Position:longint):longword;

       function ParallelNFAStateAllocate(const Count:longint;const BitState:longword):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}
       function ParallelNFAStateAcquire(const State:PFLREParallelNFAState):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}
       procedure ParallelNFAStateRelease(const State:PFLREParallelNFAState); {$ifdef caninline}inline;{$endif}
       function ParallelNFAStateUpdate(const State:PFLREParallelNFAState;const Index,Position:longint):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}

       procedure ParallelNFAAddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction;State:PFLREParallelNFAState;const Position:longint);

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

     end;

     TFLRE=class
      private

       RegularExpression:ansistring;

       Flags:TFLREFlags;

       AnchoredRootNode:PFLRENode;

       UnanchoredRootNode:PFLRENode;

       Nodes:TList;

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

       DFANeedVerification:longbool;

       BeginningJump:longbool;
       BeginningSplit:longbool;
       BeginningWildCard:longbool;
       BeginningAnchor:longbool;
       BeginningWildcardLoop:longbool;

       DoUnanchoredStart:longbool;

       NamedGroupStringList:TStringList;
       NamedGroupStringIntegerPairHashMap:TFLREStringIntegerPairHashMap;

       ThreadLocalStorageCriticalSection:TCriticalSection;

       ThreadLocalStorages:TFLREThreadLocalStorage;
       FreeThreadLocalStorages:TFLREThreadLocalStorage;

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

       function IsWordChar(const CharValue:longword):boolean; {$ifdef caninline}inline;{$endif}

       function SearchMatchParallelNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):boolean;
       function SearchMatchOnePassNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint):boolean;
       function SearchMatchBitStateNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):longint;
       function SearchMatchDFA(const ThreadLocalStorage:TFLREThreadLocalStorage;const StartPosition,UntilExcludingPosition:longint;out MatchEnd:longint;const UnanchoredStart:boolean):longint;
       function SearchMatchReversedDFA(const ThreadLocalStorage:TFLREThreadLocalStorage;const StartPosition,UntilIncludingPosition:longint;out MatchBegin:longint):longint;
       function SearchMatch(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;StartPosition,UntilExcludingPosition:longint;UnanchoredStart:boolean):boolean;
      public
       constructor Create(const ARegularExpression:ansistring;const AFlags:TFLREFlags=[]);
       destructor Destroy; override;
       function PtrMatch(const Input:pointer;const InputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
       function PtrMatchNext(const Input:pointer;const InputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
       function PtrMatchAll(const Input:pointer;const InputLength:longint;var Captures:TFLREMultiCaptures;const StartPosition:longint=0;Limit:longint=-1):boolean;
       function PtrReplaceAll(const Input:pointer;const InputLength:longint;const AReplacement:pointer;const AReplacementLength:longint;const StartPosition:longint=0;Limit:longint=-1):ansistring;
       function Match(const Input:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
       function MatchNext(const Input:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
       function MatchAll(const Input:ansistring;var Captures:TFLREMultiCaptures;const StartPosition:longint=1;Limit:longint=-1):boolean;
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

procedure UTF8PtrSafeInc(const s:PAnsiChar;var Len,CodeUnit:longint);
var StartCodeUnit,State:longword;
begin
 if CodeUnit>=0 then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<Len do begin
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

function UTF8PtrCodeUnitGetCharFallback(const s:pansichar;Len,CodeUnit:longint):longword;
var Value,CharClass,State:longword;
    StartCodeUnit:longint;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  StartCodeUnit:=CodeUnit;
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
   result:=byte(ansichar(s[StartCodeUnit]));
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
type TString6Chars=array[0..6] of ansichar;
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
    result[0]:=ansichar(byte(1));
    result[1]:=ansichar(byte(CharValue));
   end;
   $00000080..$000007ff:begin
    result[0]:=ansichar(byte(2));
    result[1]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    result[2]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
// {$ifdef PLREStrictUTF8}$00000800..$0000d7ff,$0000e000..$0000ffff{$else}$00000800..$0000ffff{$endif}:begin
   $00000800..$0000ffff:begin
    result[0]:=ansichar(byte(3));
    result[1]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[3]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00010000..$0010ffff:begin
    result[0]:=ansichar(byte(4));
    result[1]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[4]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $00200000..$03ffffff:begin
    result[0]:=ansichar(byte(5));
    result[1]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[5]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   $04000000..$7fffffff:begin
    result[0]:=ansichar(byte(6));
    result[1]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    result[2]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    result[3]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    result[4]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    result[5]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    result[6]:=ansichar(byte($80 or (CharValue and $3f)));
   end;
   else begin
    result[0]:=ansichar(byte(3));
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
     if byte(ansichar(StrLo[0]))=byte(ansichar(StrHi[0])) then begin
      for i:=1 to byte(ansichar(StrLo[0])) do begin
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
       RegExp:TFLRE;
       Previous,Next:TFLREUnicodeCharClass;
       First,Last,Root:TFLREUnicodeCharClassRange;
       CharSet:TFLREUnicodeCharClassCharSet;
       Inverted:longbool;
       Canonicalized:longbool;
       constructor Create(ARegExp:TFLRE);
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

constructor TFLREUnicodeCharClass.Create(ARegExp:TFLRE);
begin
 inherited Create;
 RegExp:=ARegExp;
{if assigned(RegExp.LastCharClass) then begin
  Previous:=RegExp.LastCharClass;
  RegExp.LastCharClass:=self;
  Previous.Next:=self;
  Next:=nil;
 end else begin
  RegExp.FirstCharClass:=self;
  RegExp.LastCharClass:=self;
  Previous:=nil;
  Next:=nil;
 end;{}
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
  First.Destroy;
 end;
{if assigned(Previous) then begin
  Previous.Next:=Next;
 end else if RegExp.FirstCharClass=self then begin
  RegExp.FirstCharClass:=Next;
 end;
 if assigned(Next) then begin
  Next.Previous:=Previous;
 end else if RegExp.LastCharClass=self then begin
  RegExp.LastCharClass:=Previous;
 end;{}
 Previous:=nil;
 Next:=nil;
 inherited Destroy;
end;

procedure TFLREUnicodeCharClass.Clear;
begin
 while assigned(First) do begin
  First.Destroy;
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
   Range.Previous.Destroy;
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
   Range.Next.Destroy;
   if assigned(Range.Previous) then begin
    Range:=Range.Previous;
   end;
  end else begin
   Range:=Range.Next;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.AddRange(Lo,Hi:longword;IgnoreCase:boolean=false);
var Range:TFLREUnicodeCharClassRange;
    c,cl,cu:longword;
    NeedToCanonicalize:boolean;
begin
 if IgnoreCase then begin
  NeedToCanonicalize:=false;
  for c:=Lo to Hi do begin
   cl:=UnicodeToLower(c);
   cu:=UnicodeToUpper(c);
   if (cl<>cu) or (cl<>c) or (cu<>c) then begin
    NeedToCanonicalize:=true;
    break;
   end;
  end;
  if NeedToCanonicalize then begin
   for c:=Lo to Hi do begin
    cl:=UnicodeToLower(c);
    cu:=UnicodeToUpper(c);
    if (cl=cu) and (cl=c) then begin
     AddRange(c,c,false);
    end else begin
     AddRange(cl,cl,false);
     AddRange(cu,cu,false);
     if (cl<>c) and (cu<>c) then begin
      AddRange(c,c,false);
     end;
    end;
   end;
  end else begin
   AddRange(Lo,Hi,false);
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
   First.Destroy;
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
  First.Destroy;
 end else if not assigned(First) then begin
  TFLREUnicodeCharClassRange.Create(self,0,$ffffffff);
 end else begin
  NewList:=TFLREUnicodeCharClass.Create(RegExp);
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
    First.Destroy;
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
   NewList.Destroy;
  end;
 end;
end;

procedure TFLREUnicodeCharClass.Canonicalize;
var NewList:TFLREUnicodeCharClass;
    Range:TFLREUnicodeCharClassRange;
    OldInverted:boolean;
begin
 if not Canonicalized then begin
  NewList:=TFLREUnicodeCharClass.Create(RegExp);
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
    First.Destroy;
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
   NewList.Destroy;
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

constructor TFLREThreadLocalStorage.Create(AInstance:TFLRE);
var Index:longint;
    FLREDFAStateCreateTempDFAState:TFLREDFAState;
begin
 inherited Create;

 AllNext:=nil;
 FreeNext:=nil;

 Instance:=AInstance;

 Input:=nil;
 InputLength:=0;

 Generation:=0;

 InstructionGenerations:=nil;
 SetLength(InstructionGenerations,AInstance.CountForwardInstructions+1);
 for Index:=0 to length(InstructionGenerations)-1 do begin
  InstructionGenerations[Index]:=-1;
 end;

 ThreadLists[0].Threads:=nil;
 ThreadLists[1].Threads:=nil;
 SetLength(ThreadLists[0].Threads,(AInstance.CountForwardInstructions+1)*4);
 SetLength(ThreadLists[1].Threads,(AInstance.CountForwardInstructions+1)*4);

 FreeSubMatches:=nil;

 AllSubMatches:=TList.Create;

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

 begin
  DFACountStatesCached:=0;

  DFAStackInstructions:=nil;
  SetLength(DFAStackInstructions,(AInstance.CountForwardInstructions+1) shl 1);

  DFAStatePoolUsed:=nil;
  DFAStatePoolFree:=nil;

  DFANextStatesSize:=AInstance.ByteMapCount*sizeof(PFLREDFAState);
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
  DFAAddInstructionThread(DFAAnchoredStartState,Instance.AnchoredStartInstruction);
  DFAAnchoredStartState^.Flags:=DFAAnchoredStartState^.Flags or sfDFAStart;
  DFAStateCache.Add(DFAAnchoredStartState);
  inc(DFACountStatesCached);

  inc(Generation);
  GetMem(DFAUnanchoredStartState,DFAStateSize);
  FillChar(DFAUnanchoredStartState^,DFAStateSize,AnsiChar(#0));
  DFAAddInstructionThread(DFAUnanchoredStartState,Instance.UnanchoredStartInstruction);
  DFAUnanchoredStartState^.Flags:=DFAUnanchoredStartState^.Flags or sfDFAStart;
  DFAStateCache.Add(DFAUnanchoredStartState);
  inc(DFACountStatesCached);

  inc(Generation);
  GetMem(DFAReversedStartState,DFAStateSize);
  FillChar(DFAReversedStartState^,DFAStateSize,AnsiChar(#0));
  DFAAddInstructionThread(DFAReversedStartState,Instance.ReversedStartInstruction);
  DFAReversedStartState^.Flags:=DFAReversedStartState^.Flags or sfDFAStart;
  DFAStateCache.Add(DFAReversedStartState);
  inc(DFACountStatesCached);

  inc(Generation);
  GetMem(DFADeadState,DFAStateSize);
  FillChar(DFADeadState^,DFAStateSize,AnsiChar(#0));
  DFAAddInstructionThread(DFADeadState,Instance.AnchoredStartInstruction);
  DFADeadState^.Flags:=DFADeadState^.Flags or sfDFADead;
  DFAStateCache.Add(DFADeadState);
  inc(DFACountStatesCached);
 end;
end;

destructor TFLREThreadLocalStorage.Destroy;
var State:PFLREParallelNFAState;
begin

 while assigned(FreeSubMatches) do begin
  State:=FreeSubMatches;
  FreeSubMatches:=FreeSubMatches^.Next;
  SetLength(State^.SubMatches,0);
  Finalize(State^);
  FreeMem(State);
 end;
 FreeSubMatches:=nil;

 FreeAndNil(AllSubMatches);

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

 SetLength(InstructionGenerations,0);

 SetLength(ThreadLists[0].Threads,0);
 SetLength(ThreadLists[1].Threads,0);

 inherited Destroy;
end;

function TFLREThreadLocalStorage.GetSatisfyFlags(const Position:longint):longword;
var PreviousPosition:longint;
    PreviousChar,CurrentChar:longword;
begin
 result:=0;
 if rfUTF8 in Instance.Flags then begin
  PreviousPosition:=Position;
  UTF8PtrDec(Input,InputLength,PreviousPosition);
  if (PreviousPosition>=0) and (PreviousPosition<=InputLength) then begin
   PreviousChar:=UTF8PtrCodeUnitGetCharFallback(Input,InputLength,PreviousPosition);
  end else begin
   PreviousChar:=0;
  end;
  if (Position>=0) and (Position<InputLength) then begin
   CurrentChar:=UTF8PtrCodeUnitGetCharFallback(Input,InputLength,Position);
  end else begin
   CurrentChar:=0;
  end;
 end else begin
  PreviousPosition:=Position-1;
  if (PreviousPosition>=0) and (PreviousPosition<=InputLength) then begin
   PreviousChar:=byte(ansichar(Input[PreviousPosition]));
  end else begin
   PreviousChar:=0;
  end;
  if (Position>=0) and (Position<InputLength) then begin
   CurrentChar:=byte(ansichar(Input[Position]));
  end else begin
   CurrentChar:=0;
  end;
 end;
 if Position<=0 then begin
  result:=result or (sfEmptyBeginText or sfEmptyBeginLine);
 end else begin
  case PreviousChar of
   $0a,$0d,$85,$2028,$2029:begin
    result:=result or sfEmptyBeginLine;
   end;
  end;
 end;
 if Position>=(InputLength-1) then begin
  result:=result or (sfEmptyEndText or sfEmptyEndLine);
 end else begin
  case CurrentChar of
   $0a,$0d,$85,$2028,$2029:begin
    result:=result or sfEmptyEndLine;
   end;
  end;
 end;
 if InputLength>0 then begin
  if Position=0 then begin
   if Instance.IsWordChar(CurrentChar) then begin
    result:=result or sfEmptyWordBoundary;
   end;
  end else if Position>=InputLength then begin
   if Instance.IsWordChar(PreviousChar) then begin
    result:=result or sfEmptyWordBoundary;
   end;
  end else if Instance.IsWordChar(PreviousChar)<>Instance.IsWordChar(CurrentChar) then begin
   result:=result or sfEmptyWordBoundary;
  end;
 end;
 if (result and sfEmptyWordBoundary)=0 then begin
  result:=result or sfEmptyNonWordBoundary;
 end;
end;

function TFLREThreadLocalStorage.ParallelNFAStateAllocate(const Count:longint;const BitState:longword):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}
begin
 if assigned(FreeSubMatches) then begin
  result:=FreeSubMatches;
  FreeSubMatches:=result^.Next;
 end else begin
  GetMem(result,SizeOf(TFLREParallelNFAState));
  FillChar(result^,SizeOf(TFLREParallelNFAState),#0);
  SetLength(result^.SubMatches,Instance.CountSubMatches);
  AllSubMatches.Add(result);
 end;
 result^.ReferenceCounter:=1;
 result^.Count:=Count;
 result^.BitState:=BitState;
end;

function TFLREThreadLocalStorage.ParallelNFAStateAcquire(const State:PFLREParallelNFAState):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}
begin
 inc(State^.ReferenceCounter);
 result:=State;
end;

procedure TFLREThreadLocalStorage.ParallelNFAStateRelease(const State:PFLREParallelNFAState); {$ifdef caninline}inline;{$endif}
begin
 dec(State^.ReferenceCounter);
 if State^.ReferenceCounter=0 then begin
  State^.Next:=FreeSubMatches;
  FreeSubMatches:=State;
 end;
end;

function TFLREThreadLocalStorage.ParallelNFAStateUpdate(const State:PFLREParallelNFAState;const Index,Position:longint):PFLREParallelNFAState; {$ifdef caninline}inline;{$endif}
var Counter:longint;
    BitState:longword;
begin
 result:=State;
 if result^.ReferenceCounter>1 then begin
  result:=ParallelNFAStateAllocate(State^.Count,State^.BitState);
{$ifdef cpu386}
  asm
   push ebx
   push esi
   push edi
    mov ebx,dword ptr result
    mov ecx,dword ptr [ebx+TFLREParallelNFAState.BitState]
    test ecx,ecx // or test ecx,$80000000; jnz @CopyAll 
    js @CopyAll
    jecxz @Done
     mov esi,dword ptr State
     mov esi,dword ptr [esi+TFLREParallelNFAState.SubMatches]
     mov edi,dword ptr [ebx+TFLREParallelNFAState.SubMatches]
     @CopySelectedLoop:
       bsf eax,ecx
       mov edx,dword ptr [esi+eax*4]
       mov dword ptr [edi+eax*4],edx
       lea edx,[ecx-1]
       and ecx,edx
      jnz @CopySelectedLoop
     jmp @Done
     @CopyAll:
      mov esi,dword ptr State
      mov ecx,dword ptr [esi+TFLREParallelNFAState.Count]
      mov esi,dword ptr [esi+TFLREParallelNFAState.SubMatches]
      mov edi,dword ptr [ebx+TFLREParallelNFAState.SubMatches]
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
    result^.SubMatches[Counter]:=State^.SubMatches[Counter];
   end;
  end else begin
   Move(State^.SubMatches[0],result^.SubMatches[0],State^.Count*SizeOf(TFLREParallelNFAStateItem));
  end;
{$endif}
  dec(State^.ReferenceCounter);
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

procedure TFLREThreadLocalStorage.ParallelNFAAddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction;State:PFLREParallelNFAState;const Position:longint);
 function Satisfy(const NextFlags:longword;const Position:longint):boolean;
 begin
  result:=((NextFlags and sfEmptyAllFlags) and not GetSatisfyFlags(Position))=0;
 end;
var Thread:PFLREThread;
begin
 while assigned(Instruction) do begin
  if InstructionGenerations[Instruction^.IndexAndOpcode shr 8]=Generation then begin
   ParallelNFAStateRelease(State);
   break;
  end else begin
   InstructionGenerations[Instruction^.IndexAndOpcode shr 8]:=Generation;
   case Instruction^.IndexAndOpcode and $ff of
    opJMP:begin
     Instruction:=Instruction^.Next;
     continue;
    end;
    opSPLIT:begin
     ParallelNFAAddThread(ThreadList,Instruction^.Next,ParallelNFAStateAcquire(State),Position);
     Instruction:=Instruction^.OtherNext;
     continue;
    end;
    opSAVE:begin
     State:=ParallelNFAStateUpdate(State,Instruction^.Value,Position);
     Instruction:=Instruction^.Next;
     continue;
    end;
    opBOL:begin
     if Satisfy(sfEmptyBeginLine,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    opEOL:begin
     if Satisfy(sfEmptyEndLine,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    opBOT:begin
     if Satisfy(sfEmptyBeginText,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    opEOT:begin
     if Satisfy(sfEmptyEndText,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    opBRK:begin
     if Satisfy(sfEmptyWordBoundary,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    opNBRK:begin
     if Satisfy(sfEmptyNonWordBoundary,Position) then begin
      Instruction:=Instruction^.Next;
      continue;
     end else begin
      ParallelNFAStateRelease(State);
      break;
     end;
    end;
    else begin
     Thread:=@ThreadList^.Threads[ThreadList^.Count];
     inc(ThreadList^.Count);
     Thread^.Instruction:=Instruction;
     Thread^.State:=State;
     break;
    end;
   end;
  end;
 end;
end;

function TFLREThreadLocalStorage.DFACacheState(const State:PFLREDFAState):PFLREDFAState; {$ifdef caninline}inline;{$endif}
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

procedure TFLREThreadLocalStorage.DFAAddInstructionThread(const State:PFLREDFAState;Instruction:PFLREInstruction);
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
  while assigned(Instruction) and (InstructionGenerations[Instruction^.IndexAndOpcode shr 8]<>Generation) do begin
   InstructionGenerations[Instruction^.IndexAndOpcode shr 8]:=Generation;
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
                                           
function TFLREThreadLocalStorage.DFAProcessNextState(State:PFLREDFAState;const CurrentChar:ansichar;const Reversed:boolean):PFLREDFAState;
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
    if not ((rfLONGEST in Instance.Flags) or Reversed) then begin
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
 State.NextStates[Instance.ByteMap[byte(ansichar(CurrentChar))]]:=result;
end;

procedure TFLREThreadLocalStorage.DFADestroyStatePool(StatePool:PFLREDFAStatePool);
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

procedure TFLREThreadLocalStorage.DFAFreeUsedStatePool;
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

function TFLREThreadLocalStorage.DFAAllocateNewStatePool:PFLREDFAStatePool;
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

function TFLREThreadLocalStorage.DFAGetState:PFLREDFAState;
begin
 if DFAStatePoolUsed^.NextState=DFAStatePoolUsed^.EndState then begin
  DFAAllocateNewStatePool;
 end;
 result:=DFAStatePoolUsed^.NextState;
 inc(ptruint(DFAStatePoolUsed^.NextState),DFAStateSize);
end;

function TFLREThreadLocalStorage.DFATakeOverState(TakeOverFrom:PFLREDFAState):PFLREDFAState;
begin
 result:=DFAGetState;
 result^.Instructions:=TakeOverFrom^.Instructions;
 result^.CountInstructions:=TakeOverFrom^.CountInstructions;
 result^.Flags:=TakeOverFrom^.Flags;
 TakeOverFrom^.Instructions:=nil;
 TakeOverFrom^.CountInstructions:=0;
 TakeOverFrom^.Flags:=0;
end;

procedure TFLREThreadLocalStorage.DFAFreeState(State:PFLREDFAState);
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

procedure TFLREThreadLocalStorage.DFAReset;
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

constructor TFLRE.Create(const ARegularExpression:ansistring;const AFlags:TFLREFlags=[]);
begin
 inherited Create;

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

 DFANeedVerification:=false;

 BeginningJump:=false;
 BeginningSplit:=false;
 BeginningWildCard:=false;
 BeginningAnchor:=false;

 NamedGroupStringList:=TStringList.Create;
 NamedGroupStringIntegerPairHashMap:=TFLREStringIntegerPairHashMap.Create;

 ThreadLocalStorageCriticalSection:=TCriticalSection.Create;

 ThreadLocalStorages:=nil;
 FreeThreadLocalStorages:=nil;

 try

  try

   Parse;

   Compile;

  finally
   SetLength(CharClasses,CountCharClasses);
  end;

  CountSubMatches:=(CountParens+1)*2;

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
    NextCharClassAction:PFLREOnePassNFAStateCharClassAction;
    Flags:longword;
    ThreadLocalStorage,NextThreadLocalStorage:TFLREThreadLocalStorage;
begin

 ThreadLocalStorage:=ThreadLocalStorages;
 ThreadLocalStorages:=nil;
 while assigned(ThreadLocalStorage) do begin
  NextThreadLocalStorage:=ThreadLocalStorage.AllNext;
  ThreadLocalStorage.Free;
  ThreadLocalStorage:=NextThreadLocalStorage;
 end;

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

 NamedGroupStringList.Free;
 NamedGroupStringIntegerPairHashMap.Free;

 ThreadLocalStorageCriticalSection.Free;

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
 procedure GetCharClassPerName(const Name:ansistring;const UnicodeCharClass:TFLREUnicodeCharClass;const IgnoreCase:boolean);
 var i:longint;
     f:int64;
 begin
  begin
   f:=UnicodeClassHashMap[Name];
   if f>0 then begin
    UnicodeCharClass.AddUnicodeCategory(f,IgnoreCase);
    UnicodeCharClass.Canonicalized:=true;
    exit;
   end;
  end;
  begin
   f:=UnicodeScriptHashMap[Name];
   if f>0 then begin
    UnicodeCharClass.AddUnicodeScript(f,IgnoreCase);
    UnicodeCharClass.Canonicalized:=true;
    exit;
   end;
  end;
  begin
   f:=UnicodeBlockHashMap[Name];
   if f>0 then begin
    UnicodeCharClass.AddUnicodeBlock(f,IgnoreCase);
    UnicodeCharClass.Canonicalized:=true;
    exit;
   end;
  end;
  if Name='alnum' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory((1 shl FLREUnicodeCategoryLu) or (1 shl FLREUnicodeCategoryLl) or (1 shl FLREUnicodeCategoryLt) or (1 shl FLREUnicodeCategoryNd),IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange(ord('a'),ord('z'),IgnoreCase);
    UnicodeCharClass.AddRange(ord('A'),ord('Z'),IgnoreCase);
    UnicodeCharClass.AddRange(ord('0'),ord('9'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='alpha' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory((1 shl FLREUnicodeCategoryLu) or (1 shl FLREUnicodeCategoryLl) or (1 shl FLREUnicodeCategoryLt),IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange(ord('a'),ord('z'),IgnoreCase);
    UnicodeCharClass.AddRange(ord('A'),ord('Z'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='ascii' then begin
   UnicodeCharClass.AddRange($00,$7f,IgnoreCase);
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='blank' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory(1 shl FLREUnicodeCategoryZs,IgnoreCase);
   end else begin
    UnicodeCharClass.AddChar(9,IgnoreCase);
    UnicodeCharClass.AddChar(32,IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='cntrl' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory(1 shl FLREUnicodeCategoryCc,IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange($00,$1f,IgnoreCase);
    UnicodeCharClass.AddChar($7f,IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='digits' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory(1 shl FLREUnicodeCategoryNd,IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange(ord('0'),ord('9'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='graph' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory((1 shl FLREUnicodeCategoryZs) or (1 shl FLREUnicodeCategoryZl) or (1 shl FLREUnicodeCategoryZp) or (1 shl FLREUnicodeCategoryCc) or (1 shl FLREUnicodeCategoryCf) or (1 shl FLREUnicodeCategoryCo) or (1 shl FLREUnicodeCategoryCs) or (1 shl FLREUnicodeCategoryCn),IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange($21,$7e,IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='inbasiclatin' then begin
   UnicodeCharClass.AddRange($00,$7f,IgnoreCase);
   UnicodeCharClass.Canonicalized:=true;
  end else if (Name='inno_block') or (Name='isno_block') or (Name='innoblock') or (Name='isnoblock') then begin
   for i:=0 to FLREUnicodeBlockCount-1 do begin
    UnicodeCharClass.AddUnicodeBlock(i,false);
   end;
   UnicodeCharClass.Invert;
   UnicodeCharClass.Inverted:=false;
   if IgnoreCase then begin
    UnicodeCharClass.Canonicalize;
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='lower' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory(1 shl FLREUnicodeCategoryLl,IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange(ord('a'),ord('z'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='print' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory((1 shl FLREUnicodeCategoryCc) or (1 shl FLREUnicodeCategoryCf) or (1 shl FLREUnicodeCategoryCo) or (1 shl FLREUnicodeCategoryCs) or (1 shl FLREUnicodeCategoryCn),IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange($20,$7e,IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='punct' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory((1 shl FLREUnicodeCategoryPd) or (1 shl FLREUnicodeCategoryPs) or (1 shl FLREUnicodeCategoryPe) or (1 shl FLREUnicodeCategoryPc) or (1 shl FLREUnicodeCategoryPo),IgnoreCase);
   end else begin
    UnicodeCharClass.AddChar(ord('!'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('"'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('#'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('$'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('%'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('&'),IgnoreCase);
    UnicodeCharClass.AddChar(ord(''''),IgnoreCase);
    UnicodeCharClass.AddChar(ord('('),IgnoreCase);
    UnicodeCharClass.AddChar(ord('!'),IgnoreCase);
    UnicodeCharClass.AddChar(ord(')'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('*'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('+'),IgnoreCase);
    UnicodeCharClass.AddChar(ord(','),IgnoreCase);
    UnicodeCharClass.AddChar(ord('\'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('-'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('.'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('/'),IgnoreCase);
    UnicodeCharClass.AddChar(ord(':'),IgnoreCase);
    UnicodeCharClass.AddChar(ord(';'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('<'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('='),IgnoreCase);
    UnicodeCharClass.AddChar(ord('>'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('?'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('@'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('['),IgnoreCase);
    UnicodeCharClass.AddChar(ord('^'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('_'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('`'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('{'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('|'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('}'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('~'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='space' then begin
   if rfUTF8 in Flags then begin
    for i:=0 to length(UnicodeCharRangeClasses[ucrWHITESPACES])-1 do begin
     UnicodeCharClass.AddRange(UnicodeCharRangeClasses[ucrWHITESPACES,i,0],UnicodeCharRangeClasses[ucrWHITESPACES,i,1],IgnoreCase);
    end;
   end else begin
    UnicodeCharClass.AddRange(9,13,IgnoreCase);
    UnicodeCharClass.AddChar(32,IgnoreCase);
   end;
  end else if Name='upper' then begin
   if rfUTF8 in Flags then begin
    UnicodeCharClass.AddUnicodeCategory(1 shl FLREUnicodeCategoryLu,IgnoreCase);
   end else begin
    UnicodeCharClass.AddRange(ord('A'),ord('Z'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='word' then begin
   if rfUTF8 in Flags then begin
    for i:=0 to length(UnicodeCharRangeClasses[ucrWORDS])-1 do begin
     UnicodeCharClass.AddRange(UnicodeCharRangeClasses[ucrWORDS,i,0],UnicodeCharRangeClasses[ucrWORDS,i,1],IgnoreCase);
    end;
   end else begin
    UnicodeCharClass.AddRange(ord('a'),ord('z'),IgnoreCase);
    UnicodeCharClass.AddRange(ord('A'),ord('Z'),IgnoreCase);
    UnicodeCharClass.AddRange(ord('0'),ord('9'),IgnoreCase);
    UnicodeCharClass.AddChar(ord('_'),IgnoreCase);
   end;
   UnicodeCharClass.Canonicalized:=true;
  end else if Name='xdigit' then begin
   UnicodeCharClass.AddRange(ord('a'),ord('f'),IgnoreCase);
   UnicodeCharClass.AddRange(ord('A'),ord('f'),IgnoreCase);
   UnicodeCharClass.AddRange(ord('0'),ord('9'),IgnoreCase);
   UnicodeCharClass.Canonicalized:=true;
  end else begin
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
 function NewUnicodeChar(UnicodeChar:longword):PFLRENode;
 var Index:longint;
     TemporaryString:ansistring;
     TemporaryNode:PFLRENode;
 begin
  result:=nil;
  TemporaryString:=UTF32CharToUTF8(UnicodeChar);
  try
   for Index:=1 to length(TemporaryString) do begin
    TemporaryNode:=NewNode(ntCHAR,nil,nil,nil,0);
    TemporaryNode^.CharClass:=[TemporaryString[Index]];
    if assigned(result) then begin
     result:=Concat(result,TemporaryNode);
    end else begin
     result:=TemporaryNode;
    end;
   end;
  finally
   TemporaryString:='';
  end;
 end;
 function NewCharEx(UnicodeChar:longword):PFLRENode;
 begin           
  if (rfUTF8 in Flags) and (UnicodeChar>=$80) then begin
   result:=NewUnicodeChar(UnicodeChar);
  end else if UnicodeChar<=$ff then begin
   result:=NewNode(ntCHAR,nil,nil,nil,0);
   result^.CharClass:=[ansichar(byte(UnicodeChar))];
  end else begin
   raise EFLRE.Create('Syntax error');
  end;
 end;
 function NewChar(UnicodeChar:longword):PFLRENode;
 var LowerCaseUnicodeChar,UpperCaseUnicodeChar:longword;
 begin
  LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
  UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
  if (rfCASEINSENSITIVE in Flags) and (LowerCaseUnicodeChar<>UpperCaseUnicodeChar) then begin
   if (rfUTF8 in Flags) and ((LowerCaseUnicodeChar>=$80) or (UpperCaseUnicodeChar>=$80)) then begin
    result:=NewAlt(NewCharEx(LowerCaseUnicodeChar),NewCharEx(UpperCaseUnicodeChar));
   end else begin
    result:=NewNode(ntCHAR,nil,nil,nil,0);
    result^.CharClass:=[ansichar(byte(LowerCaseUnicodeChar)),ansichar(byte(UpperCaseUnicodeChar))];
   end;
  end else begin
   if (rfUTF8 in Flags) and (UnicodeChar>=$80) then begin
    result:=NewCharEx(UnicodeChar);
   end else begin
    result:=NewNode(ntCHAR,nil,nil,nil,0);
    result^.CharClass:=[ansichar(byte(UnicodeChar))];
   end;
  end;
 end;
 function CompileUTF8Range(Lo,Hi:longword):PFLRENode;
 type TString6Chars=array[0..6] of ansichar;
 const Seq0010ffff:array[0..6,0..4,0..1] of longint=((($00,$7f),(-1,-1),(-1,-1),(-1,-1),(-1,-1)),        // 00-7F
                                                     (($c2,$df),($80,$bf),(-1,-1),(-1,-1),(-1,-1)),      // C2-DF 80-BF
                                                     (($e0,$e0),($a0,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E0-E0 A0-BF 80-BF
                                                     (($e1,$ef),($80,$bf),($80,$bf),(-1,-1),(-1,-1)),    // E1-EF 80-BF 80-BF
                                                     (($f0,$f0),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F0-F0 90-BF 80-BF 80-BF
                                                     (($f1,$f3),($80,$bf),($80,$bf),($80,$bf),(-1,-1)),  // F1-F3 80-BF 80-BF 80-BF
                                                     (($f4,$f4),($80,$bf),($80,$bf),($80,$bf),(-1,-1))); // F4-F4 80-8F 80-BF 80-BF
 var OutputNode,NodeChain:PFLRENode;
  procedure Add(const NewNode:PFLRENode);
  begin
   if assigned(NewNode) then begin
    if assigned(NodeChain) then begin
     NodeChain:=Concat(OutputNode,NewNode);
    end else begin
     NodeChain:=NewNode;
    end;
   end;
  end;
  procedure AddSuffix;
  begin
   if assigned(NodeChain) then begin
    if assigned(OutputNode) then begin
     OutputNode:=NewAlt(OutputNode,NodeChain);
    end else begin
     OutputNode:=NodeChain;
    end;
    NodeChain:=nil;
   end;
  end;
  function ToString(CharValue:longword):TString6Chars;
  begin
   case CharValue of
    $00000000..$0000007f:begin
     result[0]:=ansichar(byte(1));
     result[1]:=ansichar(byte(CharValue));
    end;
    $00000080..$000007ff:begin
     result[0]:=ansichar(byte(2));
     result[1]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
     result[2]:=ansichar(byte($80 or (CharValue and $3f)));
    end;
 // {$ifdef PLREStrictUTF8}$00000800..$0000d7ff,$0000e000..$0000ffff{$else}$00000800..$0000ffff{$endif}:begin
    $00000800..$0000ffff:begin
     result[0]:=ansichar(byte(3));
     result[1]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
     result[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
     result[3]:=ansichar(byte($80 or (CharValue and $3f)));
    end;
    $00010000..$0010ffff:begin
     result[0]:=ansichar(byte(4));
     result[1]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
     result[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
     result[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
     result[4]:=ansichar(byte($80 or (CharValue and $3f)));
    end;
    $00200000..$03ffffff:begin
     result[0]:=ansichar(byte(5));
     result[1]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
     result[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
     result[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
     result[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
     result[5]:=ansichar(byte($80 or (CharValue and $3f)));
    end;
    $04000000..$7fffffff:begin
     result[0]:=ansichar(byte(6));
     result[1]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
     result[2]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
     result[3]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
     result[4]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
     result[5]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
     result[6]:=ansichar(byte($80 or (CharValue and $3f)));
    end;
    else begin
     result[0]:=ansichar(byte(3));
     result[1]:=#$ef;
     result[2]:=#$bf;
     result[3]:=#$bd;
    end;
   end;
  end;
  procedure AddRange(const Lo,Hi:byte);
  var Node:PFLRENode;
  begin
   Node:=NewNode(ntCHAR,nil,nil,nil,0);
   Node^.CharClass:=[ansichar(byte(Lo))..ansichar(byte(Hi))];
   Add(Node);
  end;
  procedure ProcessRange(Lo,Hi:longword);
  var i,m:longword;
      StrLo,StrHi:TString6Chars;
      CurrentNode:PFLRENode;
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
      AddSuffix;
     end;
    end else if (Lo=$00000080) and (Hi=$0010ffff) then begin
     for m:=1 to high(Seq0010ffff) do begin
      for i:=low(Seq0010ffff[m]) to high(Seq0010ffff[m]) do begin
       if Seq0010ffff[m,i,0]<0 then begin
        break;
       end;
       AddRange(byte(Seq0010ffff[m,i,0]),byte(Seq0010ffff[m,i,1]));
      end;
      AddSuffix;
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
      AddSuffix;
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
      if byte(ansichar(StrLo[0]))=byte(ansichar(StrHi[0])) then begin
       for i:=1 to byte(ansichar(StrLo[0])) do begin
        AddRange(byte(ansichar(StrLo[i])),byte(ansichar(StrHi[i])));
       end;
       AddSuffix;
      end;
     end;
    end;
   end;
  end;
 begin
  OutputNode:=nil;
  NodeChain:=nil;
  ProcessRange(Lo,Hi);
  result:=OutputNode;
 end;
 function NewUnicodeCharClass(UnicodeCharClass:TFLREUnicodeCharClass):PFLRENode;
 var Range:TFLREUnicodeCharClassRange;
     Node:PFLRENode;
 begin
  UnicodeCharClass.Optimize;
  if UnicodeCharClass.IsSingle then begin
   result:=NewChar(UnicodeCharClass.First.Lo);
  end else begin
   if (rfUTF8 in Flags) and assigned(UnicodeCharClass.Last) and (UnicodeCharClass.Last.Hi>=128) then begin
    result:=nil;
    Range:=UnicodeCharClass.First;
    while assigned(Range) do begin
     if (Range.Lo<=$10ffff) and (Range.Lo=Range.Hi) then begin
      Node:=NewCharEx(Range.Lo);
     end else begin
      Node:=CompileUTF8Range(Range.Lo,Range.Hi);
     end;
     if assigned(result) then begin
      result:=NewAlt(result,Node);
     end else begin
      result:=Node;
     end;
     Range:=Range.Next;
    end;
   end else begin
    result:=NewNode(ntCHAR,nil,nil,nil,0);
    result^.CharClass:=[];
    Range:=UnicodeCharClass.First;
    while assigned(Range) and (Range.Lo<256) do begin
     if Range.Lo=Range.Hi then begin
      Include(result^.CharClass,ansichar(byte(Range.Lo)));
     end else begin
      if Range.Hi<256 then begin
       result^.CharClass:=result^.CharClass+[ansichar(byte(Range.Lo))..ansichar(byte(Range.Hi))];
      end else begin
       result^.CharClass:=result^.CharClass+[ansichar(byte(Range.Lo))..#$ff];
      end;
     end;
     Range:=Range.Next;
    end;
   end;
  end;
 end;
 function TestClassPOSIXCharacterClass:boolean;
 var LocalSourcePosition,i:longint;
 begin
  result:=false;
  LocalSourcePosition:=SourcePosition;

  if (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition]='[') then begin
   inc(LocalSourcePosition);
  end else begin
   exit;
  end;

  if (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition]=':') then begin
   inc(LocalSourcePosition);
  end else begin
   exit;
  end;

  if (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition]='^') then begin
   inc(LocalSourcePosition);
  end else begin
   exit;
  end;

  i:=0;
  while (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition] in ['a'..'z','A'..'Z','0'..'9','_','-']) do begin
   inc(LocalSourcePosition);
  end;
  if i=0 then begin
   exit;
  end;

  if (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition]=':') then begin
   inc(LocalSourcePosition);
  end else begin
   exit;
  end;

  if (LocalSourcePosition<=SourceLength) and (Source[LocalSourcePosition]=']') then begin
   inc(LocalSourcePosition);
  end else begin
   exit;
  end;

  result:=true;
 end;
 function ParseClassPOSIXCharacterClass(const UnicodeCharClass:TFLREUnicodeCharClass;const CanBeAlreadyCanonicalized:boolean):boolean;
 var Name:ansistring;
     Negate,IgnoreCase:boolean;
 begin
  result:=false;

  IgnoreCase:=CanBeAlreadyCanonicalized and (rfCASEINSENSITIVE in Flags);

  if (SourcePosition<=SourceLength) and (Source[SourcePosition]='[') then begin
   inc(SourcePosition);
  end else begin
   raise EFLRE.Create('Syntax error');
  end;

  if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
   inc(SourcePosition);
  end else begin
   raise EFLRE.Create('Syntax error');
  end;

  if (SourcePosition<=SourceLength) and (Source[SourcePosition]='^') then begin
   inc(SourcePosition);
   Negate:=true;
  end else begin
   Negate:=false;
  end;

  Name:='';
  while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['a'..'z','A'..'Z','0'..'9','_','-']) do begin
   Name:=Name+Source[SourcePosition];
   inc(SourcePosition);
  end;

  if (SourcePosition<=SourceLength) and (Source[SourcePosition]=':') then begin
   inc(SourcePosition);
  end else begin
   raise EFLRE.Create('Syntax error');
  end;

  if (SourcePosition<=SourceLength) and (Source[SourcePosition]=']') then begin
   inc(SourcePosition);
  end else begin
   raise EFLRE.Create('Syntax error');
  end;

  GetCharClassPerName(Name,UnicodeCharClass,IgnoreCase);
  if Negate then begin
   UnicodeCharClass.Invert;
   UnicodeCharClass.Inverted:=false;
  end;

  result:=true;
 end;
 function ParseClassEscapeUnicodeProperty(const UnicodeCharClass:TFLREUnicodeCharClass;const CanBeAlreadyCanonicalized:boolean):boolean;
 var Identifier:ansistring;
     i:longword;
     IgnoreCase,IsNegative:boolean;
     f,LastSourcePos,UntilSourcePos:longint;
 begin
  result:=false;
  if SourcePosition<=SourceLength then begin
   IgnoreCase:=CanBeAlreadyCanonicalized and (rfCASEINSENSITIVE in Flags);
   case Source[SourcePosition] of
    'a'..'z','A'..'Z':begin
     f:=UnicodeClassHashMap.GetValue(UTF32CharToUTF8(UnicodeToLower(byte(ansichar(Source[SourcePosition])))));
     if f>=0 then begin
      inc(SourcePosition);
      UnicodeCharClass.AddUnicodeCategory(f,IgnoreCase);
      result:=true;
     end;
    end;
    '{':begin
     LastSourcePos:=SourcePosition;
     inc(SourcePosition);
     if (SourcePosition<=SourceLength) and (Source[SourcePosition]='^') then begin
      LastSourcePos:=SourcePosition;
      inc(SourcePosition);
      IsNegative:=true;
      IgnoreCase:=false;
     end else begin
      IsNegative:=false;
     end;
     UntilSourcePos:=LastSourcePos;
     while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['a'..'z','A'..'Z','0'..'9','_','-']) do begin
      UntilSourcePos:=SourcePosition;
      inc(SourcePosition);
     end;
     if (LastSourcePos<UntilSourcePos) and ((SourcePosition<=SourceLength) and (Source[SourcePosition]='}')) then begin
      Identifier:=UTF8LowerCase(copy(Source,LastSourcePos,UntilSourcePos-LastSourcePos));
      inc(SourcePosition);
      GetCharClassPerName(Identifier,UnicodeCharClass,IgnoreCase);
      if IsNegative and assigned(UnicodeCharClass.First) then begin
       UnicodeCharClass.Invert;
       UnicodeCharClass.Inverted:=false;
       if CanBeAlreadyCanonicalized and (rfCASEINSENSITIVE in Flags) then begin
        UnicodeCharClass.Canonicalize;
       end;
      end;
      result:=true;
     end;
    end;
   end;
  end;
 end;
 function ParseClassEscape(const CanBeAlreadyCanonicalized:boolean):TFLREUnicodeCharClass;
 var i:longint;
     IgnoreCase:boolean;
     UnicodeChar:longword;
 begin
  result:=nil;
  try
   IgnoreCase:=CanBeAlreadyCanonicalized and (rfCASEINSENSITIVE in Flags);
   result:=TFLREUnicodeCharClass.Create(self);
   if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) then begin
    i:=0;
    while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9']) do begin
     i:=(i*10)+longint(byte(ansichar(Source[SourcePosition]))-byte(ansichar('0')));
     inc(SourcePosition);
    end;
    if i<>0 then begin
     raise EFLRE.Create('Syntax error');
    end;
    result.AddChar(i,IgnoreCase);
    result.Canonicalized:=IgnoreCase;
    exit;
   end;
   case Source[SourcePosition] of
    'a':begin
     inc(SourcePosition);
     result.AddChar($0007,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'b':begin
     inc(SourcePosition);
     result.AddChar($0008,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'B':begin
     inc(SourcePosition);
     result.AddChar(ord('\'),IgnoreCase);
     result.Canonicalized:=true;
    end;
    't':begin
     inc(SourcePosition);
     result.AddChar(9,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'n':begin
     inc(SourcePosition);
     result.AddChar(10,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'v':begin
     inc(SourcePosition);
     result.AddChar(11,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'f':begin
     inc(SourcePosition);
     result.AddChar(12,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'r':begin
     inc(SourcePosition);
     result.AddChar(13,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'e':begin
     inc(SourcePosition);
     result.AddChar($1b,IgnoreCase);
     result.Canonicalized:=true;
    end;
    'w','W':begin
     if rfUTF8 in Flags then begin
      for i:=0 to length(UnicodeCharRangeClasses[ucrWORDS])-1 do begin
       result.AddRange(UnicodeCharRangeClasses[ucrWORDS,i,0],UnicodeCharRangeClasses[ucrWORDS,i,1],IgnoreCase);
      end;
     end else begin
      result.AddRange(ord('a'),ord('z'),IgnoreCase);
      result.AddRange(ord('A'),ord('Z'),IgnoreCase);
      result.AddRange(ord('0'),ord('9'),IgnoreCase);
      result.AddChar(ord('_'),IgnoreCase);
     end;
     if Source[SourcePosition]='W' then begin
      result.Invert;
      result.Inverted:=false;
     end;
     result.Canonicalized:=true;
     inc(SourcePosition);
    end;
    's','S':begin
     if rfUTF8 in Flags then begin
      for i:=0 to length(UnicodeCharRangeClasses[ucrWHITESPACES])-1 do begin
       result.AddRange(UnicodeCharRangeClasses[ucrWHITESPACES,i,0],UnicodeCharRangeClasses[ucrWHITESPACES,i,1],IgnoreCase);
      end;
     end else begin
      result.AddRange(9,13,IgnoreCase);
      result.AddChar(32,IgnoreCase);
     end;
     if Source[SourcePosition]='S' then begin
      result.Invert;
      result.Inverted:=false;
     end;
     result.Canonicalized:=true;
     inc(SourcePosition);
    end;
    'd','D':begin
     if rfUTF8 in Flags then begin
      for i:=0 to length(UnicodeCharRangeClasses[ucrDIGITS])-1 do begin
       result.AddRange(UnicodeCharRangeClasses[ucrDIGITS,i,0],UnicodeCharRangeClasses[ucrDIGITS,i,1],IgnoreCase);
      end;
     end else begin
      result.AddRange(ord('0'),ord('9'),IgnoreCase);
     end;
     if Source[SourcePosition]='D' then begin
      result.Invert;
      result.Inverted:=false;
     end;
     result.Canonicalized:=true;
     inc(SourcePosition);
    end;
    'p':begin
     inc(SourcePosition);
     if ParseClassEscapeUnicodeProperty(result,CanBeAlreadyCanonicalized) then begin
      result.Canonicalized:=true;
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
    end;
    'P':begin
     inc(SourcePosition);
     if ParseClassEscapeUnicodeProperty(result,CanBeAlreadyCanonicalized) then begin
      result.Canonicalized:=true;
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
     result.Invert;
     result.Inverted:=false;
     result.Canonicalized:=true;
    end;
    'c':begin
     inc(SourcePosition);
     if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['a'..'z','A'..'Z']) then begin
      case Source[SourcePosition] of
       'a'..'z':begin
        result.AddChar(byte(ansichar(Source[SourcePosition]))-byte(ansichar('a')),IgnoreCase);
       end;
       'A'..'Z':begin
        result.AddChar(byte(ansichar(Source[SourcePosition]))-byte(ansichar('A')),IgnoreCase);
       end;
      end;
      result.Canonicalized:=IgnoreCase;
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
    end;
    'x':begin
     inc(SourcePosition);
     if ((SourcePosition+1)<=SourceLength) and
        (Source[SourcePosition+0] in ['0'..'9','a'..'f','A'..'F']) and
        (Source[SourcePosition+1] in ['0'..'9','a'..'f','A'..'F']) then begin
      result.AddChar((Hex2Value(Source[SourcePosition+0]) shl 8) or Hex2Value(Source[SourcePosition+1]),IgnoreCase);
      inc(SourcePosition,2);
     end else if (SourcePosition<=SourceLength) and (Source[SourcePosition]='{') then begin
      inc(SourcePosition);
      UnicodeChar:=0;
      while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) do begin
       UnicodeChar:=(UnicodeChar shl 4) or Hex2Value(Source[SourcePosition]);
       inc(SourcePosition);
      end;
      if (SourcePosition<=SourceLength) and (Source[SourcePosition]='}') then begin
       inc(SourcePosition);
       result.AddChar(UnicodeChar,IgnoreCase);
      end else begin
       raise EFLRE.Create('Syntax error');
      end;
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
    end;
    'u':begin
     inc(SourcePosition);
     if ((SourcePosition+3)<=SourceLength) and
        (Source[SourcePosition+0] in ['0'..'9','a'..'f','A'..'F']) and
        (Source[SourcePosition+1] in ['0'..'9','a'..'f','A'..'F']) and
        (Source[SourcePosition+2] in ['0'..'9','a'..'f','A'..'F']) and
        (Source[SourcePosition+3] in ['0'..'9','a'..'f','A'..'F']) then begin
      UnicodeChar:=(Hex2Value(Source[SourcePosition+0]) shl 24) or (Hex2Value(Source[SourcePosition+1]) shl 16) or (Hex2Value(Source[SourcePosition+2]) shl 8) or Hex2Value(Source[SourcePosition+3]);
      inc(SourcePosition,4);
      result.AddChar(UnicodeChar,IgnoreCase);
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
    end;
    'U':begin
     inc(SourcePosition);
     if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) then begin
      UnicodeChar:=0;
      while (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['0'..'9','a'..'f','A'..'F']) do begin
       UnicodeChar:=(UnicodeChar shl 4) or Hex2Value(Source[SourcePosition]);
       inc(SourcePosition);
      end;
      result.AddChar(UnicodeChar,IgnoreCase);
     end else begin
      raise EFLRE.Create('Syntax error');
     end;
    end;
    'Q':begin
     inc(SourcePosition);
     while SourcePosition<=SourceLength do begin
      if Source[SourcePosition]='\' then begin
       inc(SourcePosition);
       if (SourcePosition<=SourceLength) and (Source[SourcePosition]='E') then begin
        inc(SourcePosition);
        break;
       end else begin
        result.AddChar(ord('\'),IgnoreCase);
       end;
      end else begin
       if (rfUTF8 in Flags) and (byte(ansichar(Source[SourcePosition]))>=$80) then begin
        UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
        result.AddChar(UnicodeChar,IgnoreCase);
       end else begin
        result.AddChar(byte(ansichar(Source[SourcePosition])),IgnoreCase);
        inc(SourcePosition);
       end;
      end;
     end;
     result.Canonicalized:=IgnoreCase;
    end;
    else begin
     if (rfUTF8 in Flags) and (byte(ansichar(Source[SourcePosition]))>=$80) then begin
      UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
      result.AddChar(UnicodeChar,IgnoreCase);
     end else begin
      result.AddChar(byte(ansichar(Source[SourcePosition])),IgnoreCase);
      inc(SourcePosition);
     end;
    end;
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ParseClassAtom:TFLREUnicodeCharClass;
 begin
  result:=nil;
  try
   if SourcePosition<=SourceLength then begin
    if Source[SourcePosition]='\' then begin
     inc(SourcePosition);
     result:=ParseClassEscape(false);
    end else if TestClassPOSIXCharacterClass then begin
     result:=TFLREUnicodeCharClass.Create(self);
     if not ParseClassPOSIXCharacterClass(result,false) then begin
      raise EFLRE.Create('Syntax error');
     end;
    end else begin
     result:=TFLREUnicodeCharClass.Create(self);
     if (rfUTF8 in Flags) and (byte(ansichar(Source[SourcePosition]))>=$80) then begin
      result.AddChar(UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition));
     end else begin
      result.AddChar(byte(ansichar(Source[SourcePosition])));
      inc(SourcePosition);
     end;
    end;
   end else begin
    raise EFLRE.Create('Syntax error');
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ParseCharacterClass:TFLREUnicodeCharClass;
 var InvertFlag:boolean;
     a,b,c:TFLREUnicodeCharClass;
 begin
  result:=nil;
  try
   a:=nil;
   b:=nil;
   try
    result:=TFLREUnicodeCharClass.Create(self);
    if (SourcePosition<=SourceLength) and (Source[SourcePosition]='[') then begin
     inc(SourcePosition);
    end else begin
     raise EFLRE.Create('Syntax error');
    end;
    if (SourcePosition<=SourceLength) and (Source[SourcePosition]='^') then begin
     inc(SourcePosition);
     InvertFlag:=true;
    end else begin
     InvertFlag:=false;
    end;
    while (SourcePosition<=SourceLength) and (Source[SourcePosition]<>']') do begin
     if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]='-') and (Source[SourcePosition+1] in ['[',']'])) then begin
      inc(SourcePosition);
      if Source[SourcePosition]=']' then begin
       a:=TFLREUnicodeCharClass.Create(self);
       a.AddChar(ord('-'));
      end else begin
       a:=ParseCharacterClass;
       b:=TFLREUnicodeCharClass.Create(self);
       if b.Subtraction(result,a) then begin
        FreeAndNil(result);
        result:=b;
        b:=nil;
       end else begin
        FreeAndNil(b);
       end;
       FreeAndNil(a);
       if not ((SourcePosition<=SourceLength) and (Source[SourcePosition]=']')) then begin
        raise EFLRE.Create('Syntax error');
       end;
       continue;
      end;
     end else begin
      a:=ParseClassAtom;
     end;
     if ((SourcePosition+1)<=SourceLength) and ((Source[SourcePosition]='&') and (Source[SourcePosition+1]='&')) then begin
      inc(SourcePosition,2);
      if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['[',']']) then begin
       if Source[SourcePosition]='[' then begin
        if assigned(a) then begin
         result.TakeoverCombine(a);
         FreeAndNil(a);
        end;
        a:=ParseCharacterClass;
        b:=TFLREUnicodeCharClass.Create(self);
        if b.Intersection(result,a) then begin
         FreeAndNil(result);
         result:=b;
         b:=nil;
        end else begin
         FreeAndNil(b);
        end;
        FreeAndNil(a);
        if not ((SourcePosition<=SourceLength) and (Source[SourcePosition]=']')) then begin
         raise EFLRE.Create('Syntax error');
        end;
        continue;
       end else begin
        raise EFLRE.Create('Syntax error');
       end;
      end else begin
       raise EFLRE.Create('Syntax error');
      end;
     end else if (SourcePosition<=SourceLength) and (Source[SourcePosition]='-') then begin
      inc(SourcePosition);
      if (SourcePosition<=SourceLength) and (Source[SourcePosition] in ['[',']']) then begin
       if Source[SourcePosition]='[' then begin
        if assigned(a) then begin
         result.TakeoverCombine(a);
         FreeAndNil(a);
        end;
        a:=ParseCharacterClass;
        b:=TFLREUnicodeCharClass.Create(self);
        if b.Subtraction(result,a) then begin
         FreeAndNil(result);
         result:=b;
         b:=nil;
        end else begin
         FreeAndNil(b);
        end;
        FreeAndNil(a);
        if not ((SourcePosition<=SourceLength) and (Source[SourcePosition]=']')) then begin
         raise EFLRE.Create('Syntax error');
        end;
        continue;
       end else begin
        a.AddChar(ord('-'));
       end;
      end else if assigned(a) then begin
       if not a.IsSingle then begin
        if assigned(a.Last) and (a.Last.Lo<=a.Last.Hi) then begin
         c:=TFLREUnicodeCharClass.Create(self);
         c.AddChar(a.Last.Hi);
         c.Canonicalized:=a.Canonicalized;
         c.Inverted:=a.Inverted;
         result.TakeoverCombine(a);
         FreeAndNil(a);
         a:=c;
        end else begin
         FreeAndNil(a);
         raise EFLRE.Create('Syntax error');
        end;
       end;
       b:=ParseClassAtom;
       if (not b.IsSingle) and (assigned(b.First) and (a.First.Lo<=a.First.Hi)) then begin
        c:=TFLREUnicodeCharClass.Create(self);
        c.AddChar(b.First.Lo);
        c.Canonicalized:=b.Canonicalized;
        c.Inverted:=b.Inverted;
        result.TakeoverCombine(b);
        FreeAndNil(b);
        b:=c;
       end;
       if (not b.IsSingle) or ((assigned(a.Last) and assigned(b.Last)) and not (a.Last.Lo<=b.Last.Hi)) then begin
        FreeAndNil(a);
        FreeAndNil(b);
        raise EFLRE.Create('Syntax error');
       end;
       if assigned(a.Last) and assigned(b.Last) then begin
        a.Last.Hi:=b.Last.Hi;
       end else begin
        a.TakeoverCombine(b);
       end;
       FreeAndNil(b);
      end;
     end;
     if assigned(a) then begin
      result.TakeoverCombine(a);
      FreeAndNil(a);
     end;
    end;
    if (SourcePosition<=SourceLength) and (Source[SourcePosition]=']') then begin
     inc(SourcePosition);
    end else begin
     raise EFLRE.Create('Syntax error');
    end;
    if (rfCASEINSENSITIVE in Flags) and not result.Canonicalized then begin
     result.Canonicalize;
    end;
    if InvertFlag then begin
     result.Invert;
    end;
   finally
    FreeAndNil(a);
    FreeAndNil(b);
   end;
  except
   FreeAndNil(result);
   raise;
  end;
 end;
 function ParseAtom:PFLRENode;
 var Value,Index:longint;
     Negate,IsSingle,Done,IsNegative,First,Num:boolean;
     StartChar,EndChar,UnicodeChar,LowerCaseUnicodeChar,UpperCaseUnicodeChar:longword;
     UnicodeCharClass:TFLREUnicodeCharClass;
     OldFlags:TFLREFlags;
     Name:ansistring;
     TemporaryNode:PFLRENode;
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
         'b','y':begin
          result:=NewNode(ntBRK,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'B','Y':begin
          result:=NewNode(ntNBRK,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'A':begin
          if rfMULTILINE in Flags then begin
           result:=NewNode(ntBOL,nil,nil,nil,0);
          end else begin
           result:=NewNode(ntBOT,nil,nil,nil,0);
          end;
          inc(SourcePosition);
         end;
         'Z':begin
          if rfMULTILINE in Flags then begin
           result:=NewNode(ntEOL,nil,nil,nil,0);
          end else begin
           result:=NewNode(ntEOT,nil,nil,nil,0);
          end;
          inc(SourcePosition);
         end;
         'k':begin
          result:=NewNode(ntBOT,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'z':begin
          result:=NewNode(ntEOT,nil,nil,nil,0);
          inc(SourcePosition);
         end;
         'p':begin
          inc(SourcePosition);
          UnicodeCharClass:=nil;
          try
           UnicodeCharClass:=TFLREUnicodeCharClass.Create(self);
           if not ParseClassEscapeUnicodeProperty(UnicodeCharClass,true) then begin
            raise EFLRE.Create('Syntax error');
           end;
           UnicodeCharClass.Canonicalized:=true;
           if assigned(UnicodeCharClass) then begin
            result:=NewUnicodeCharClass(UnicodeCharClass);
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
          finally
           FreeAndNil(UnicodeCharClass);
          end;
         end;
         'P':begin
          inc(SourcePosition);
          UnicodeCharClass:=nil;
          try
           UnicodeCharClass:=TFLREUnicodeCharClass.Create(self);
           if not ParseClassEscapeUnicodeProperty(UnicodeCharClass,true) then begin
            raise EFLRE.Create('Syntax error');
           end;
           UnicodeCharClass.Invert;
           UnicodeCharClass.Inverted:=false;
           UnicodeCharClass.Canonicalized:=true;
           if assigned(UnicodeCharClass) then begin
            result:=NewUnicodeCharClass(UnicodeCharClass);
           end else begin
            raise EFLRE.Create('Syntax error');
           end;
          finally
           FreeAndNil(UnicodeCharClass);
          end;
         end;
         else begin
          if (rfUTF8 in Flags) and (byte(ansichar(Source[SourcePosition]))>=$80) then begin
           UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
           result:=NewUnicodeChar(UnicodeChar);
          end else begin
           UnicodeCharClass:=nil;
           try
            UnicodeCharClass:=ParseClassEscape(true);
            if assigned(UnicodeCharClass) then begin
             result:=NewUnicodeCharClass(UnicodeCharClass);
            end else begin
             raise EFLRE.Create('Syntax error');
            end;
           finally
            FreeAndNil(UnicodeCharClass);
           end;
          end;
         end;
        end;
       end else begin
        raise EFLRE.Create('Syntax error');
       end;
      end;
      'Q':begin
       inc(SourcePosition);
       while SourcePosition<=SourceLength do begin
        TemporaryNode:=nil;
        case Source[SourcePosition] of
         '\':begin
          inc(SourcePosition);
          if (SourcePosition<=SourceLength) and (Source[SourcePosition]='E') then begin
           inc(SourcePosition);
           break;
          end else begin
           TemporaryNode:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNode^.CharClass:=['\'];
          end;
         end;
         #128..#255:begin
          if rfUTF8 in Flags then begin
           UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
           LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
           UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
           if (rfCASEINSENSITIVE in Flags) and (LowerCaseUnicodeChar<>UpperCaseUnicodeChar) then begin
            TemporaryNode:=NewAlt(NewUnicodeChar(LowerCaseUnicodeChar),NewUnicodeChar(UpperCaseUnicodeChar));
           end else begin
            TemporaryNode:=NewUnicodeChar(UnicodeChar);
           end;
          end else begin
           TemporaryNode:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNode^.CharClass:=[Source[SourcePosition]];
           inc(SourcePosition);
          end;
         end;
         else begin
          if (rfCASEINSENSITIVE in Flags) and (Source[SourcePosition] in ['a'..'z','A'..'Z']) then begin
           UnicodeChar:=byte(ansichar(Source[SourcePosition]));
           LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
           UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
           TemporaryNode:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNode^.CharClass:=[ansichar(byte(LowerCaseUnicodeChar)),ansichar(byte(UpperCaseUnicodeChar))];
          end else begin
           TemporaryNode:=NewNode(ntCHAR,nil,nil,nil,0);
           TemporaryNode^.CharClass:=[Source[SourcePosition]];
           inc(SourcePosition);
          end;
         end;
        end;
        if assigned(TemporaryNode) then begin
         if assigned(result) then begin
          result:=Concat(result,TemporaryNode);
         end else begin
          result:=TemporaryNode;
         end;
        end else begin
         raise EFLRE.Create('Syntax error');
        end;
       end;
      end;
      '[':begin
       UnicodeCharClass:=nil;
       try
        if TestClassPOSIXCharacterClass then begin
         UnicodeCharClass:=TFLREUnicodeCharClass.Create(self);
         if not ParseClassPOSIXCharacterClass(UnicodeCharClass,true) then begin
          raise EFLRE.Create('Syntax error');
         end;
        end else begin
         UnicodeCharClass:=ParseCharacterClass;
        end;
        if assigned(UnicodeCharClass) then begin
         result:=NewUnicodeCharClass(UnicodeCharClass);
        end else begin
         raise EFLRE.Create('Syntax error');
        end;
       finally
        FreeAndNil(UnicodeCharClass);
       end;
      end;
      '.':begin
       inc(SourcePosition);
       if rfUTF8 in Flags then begin
        UnicodeCharClass:=nil;
        try
         UnicodeCharClass:=TFLREUnicodeCharClass.Create(self);
         if rfSINGLELINE in Flags then begin
          UnicodeCharClass.AddRange($00000000,$ffffffff);
         end else begin
          UnicodeCharClass.AddChar(10);
          UnicodeCharClass.AddChar(13);
          UnicodeCharClass.Invert;
          UnicodeCharClass.Inverted:=false;
         end;
         result:=NewUnicodeCharClass(UnicodeCharClass);
        finally
         FreeAndNil(UnicodeCharClass);
        end;
       end else begin
        if rfSINGLELINE in Flags then begin
         result:=NewNode(ntANY,nil,nil,nil,0);
        end else begin
         result:=NewNode(ntCHAR,nil,nil,nil,0);
         result^.CharClass:=AllCharClass-[#10,#13];
        end;
       end;
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
      #128..#255:begin
       if rfUTF8 in Flags then begin
        UnicodeChar:=UTF8CodeUnitGetCharAndIncFallback(Source,SourcePosition);
        LowerCaseUnicodeChar:=UnicodeToLower(UnicodeChar);
        UpperCaseUnicodeChar:=UnicodeToUpper(UnicodeChar);
        if (rfCASEINSENSITIVE in Flags) and (LowerCaseUnicodeChar<>UpperCaseUnicodeChar) then begin
         result:=NewAlt(NewUnicodeChar(LowerCaseUnicodeChar),NewUnicodeChar(UpperCaseUnicodeChar));
        end else begin
         result:=NewUnicodeChar(UnicodeChar);
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
    Generation:int64;
    InstructionGenerations:TFLREInstructionGenerations;
 procedure AddThread(const ThreadList:PFLREThreadList;Instruction:PFLREInstruction);
 begin
  while assigned(Instruction) do begin
   if InstructionGenerations[Instruction^.IndexAndOpcode shr 8]=Generation then begin
    break;
   end else begin
    InstructionGenerations[Instruction^.IndexAndOpcode shr 8]:=Generation;
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
var ThreadIndex,Count,Index:longint;
    CurrentThreadList,NewThreadList,TemporaryThreadList:PFLREThreadList;
    CurrentThread:PFLREThread;
    Instruction:PFLREInstruction;
    CurrentChar:ansichar;
    ThreadLists:TFLREThreadLists;
begin

 Generation:=0;

 InstructionGenerations:=nil;
 try

  SetLength(InstructionGenerations,CountForwardInstructions+1);
  for Index:=0 to length(InstructionGenerations)-1 do begin
   InstructionGenerations[Index]:=-1;
  end;

  for CurrentPosition:=0 to MaxPrefixCharClasses-1 do begin
   PrefixCharClasses[CurrentPosition]:=[];
  end;

  CountPrefixCharClasses:=0;

  ThreadLists[0].Threads:=nil;
  ThreadLists[1].Threads:=nil;
  try
   SetLength(ThreadLists[0].Threads,(CountForwardInstructions+1)*4);
   SetLength(ThreadLists[1].Threads,(CountForwardInstructions+1)*4);

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

  finally
   SetLength(ThreadLists[0].Threads,0);
   SetLength(ThreadLists[1].Threads,0);
  end;

 finally
  SetLength(InstructionGenerations,0);
 end;
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

function TFLRE.IsWordChar(const CharValue:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 if CharValue=$ffffffff then begin
  result:=false;
 end else begin
  if rfUTF8 in Flags then begin
   result:=UnicodeIsWord(CharValue);
  end else begin
   case CharValue of
    ord('a')..ord('z'),ord('A')..ord('Z'),ord('0')..ord('9'),ord('_'):begin
     result:=true;
    end;
    else begin
     result:=false;
    end;
   end;
  end;
 end;
end;

function TFLRE.SearchMatchParallelNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):boolean;
var InputLength,CurrentPosition,Counter,ThreadIndex,CurrentLength,LastPosition:longint;
    CurrentThreadList,NewThreadList,TemporaryThreadList:PFLREThreadList;
    State,Matched,BestState:PFLREParallelNFAState;
    CurrentThread:PFLREThread;
    Thread:TFLREThread;
    Instruction:PFLREInstruction;
    CurrentChar:ansichar;
    Capture:PFLRECapture;
    BitState:longword;
    Input:pansichar;
begin
 result:=false;

 Input:=ThreadLocalStorage.Input;
 InputLength:=ThreadLocalStorage.InputLength;

 CurrentThreadList:=@ThreadLocalStorage.ThreadLists[0];
 NewThreadList:=@ThreadLocalStorage.ThreadLists[1];

 CurrentThreadList^.Count:=0;
 NewThreadList^.Count:=0;

 State:=ThreadLocalStorage.ParallelNFAStateAllocate(CountSubMatches,0);

 inc(ThreadLocalStorage.Generation);
 if UnanchoredStart then begin
  ThreadLocalStorage.ParallelNFAAddThread(CurrentThreadList,UnanchoredStartInstruction,State,StartPosition);
 end else begin
  ThreadLocalStorage.ParallelNFAAddThread(CurrentThreadList,AnchoredStartInstruction,State,StartPosition);
 end;

 Matched:=nil;

 BestState:=nil;

 LastPosition:=-1;

 for CurrentPosition:=StartPosition to UntilExcludingPosition-1 do begin
  if CurrentThreadList^.Count=0 then begin
   break;
  end;
  CurrentChar:=Input[CurrentPosition];
  inc(ThreadLocalStorage.Generation);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   State:=CurrentThread^.State;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR:begin
     if (CurrentPosition>=InputLength) or (byte(ansichar(CurrentChar))<>Instruction^.Value) then begin
      ThreadLocalStorage.ParallelNFAStateRelease(State);
     end else begin
      ThreadLocalStorage.ParallelNFAAddThread(NewThreadList,Instruction^.Next,State,CurrentPosition+1);
     end;
    end;
    opCHAR:begin
     if (CurrentPosition>=InputLength) or not (CurrentChar in PFLRECharClass(pointer(ptruint(Instruction^.Value)))^) then begin
      ThreadLocalStorage.ParallelNFAStateRelease(State);
     end else begin
      ThreadLocalStorage.ParallelNFAAddThread(NewThreadList,Instruction^.Next,State,CurrentPosition+1);
     end;
    end;
    opANY:begin
     if CurrentPosition>=InputLength then begin
      ThreadLocalStorage.ParallelNFAStateRelease(State);
     end else begin
      ThreadLocalStorage.ParallelNFAAddThread(NewThreadList,Instruction^.Next,State,CurrentPosition+1);
     end;
    end;
    opMATCH:begin
     if rfLONGEST in Flags then begin
      if not assigned(BestState) then begin
       BestState:=ThreadLocalStorage.ParallelNFAStateAllocate(CountSubMatches,State^.BitState);
      end;
      if State^.BitState<>0 then begin
       if LastPosition<CurrentPosition then begin
        LastPosition:=CurrentPosition;
        BestState^.BitState:=State^.BitState;
        Move(State^.SubMatches[0],BestState^.SubMatches[0],State^.Count*SizeOf(TFLREParallelNFAStateItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       ThreadLocalStorage.ParallelNFAStateRelease(Matched);
      end;
      Matched:=State;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       ThreadLocalStorage.ParallelNFAStateRelease(CurrentThreadList^.Threads[Counter].State);
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
  inc(ThreadLocalStorage.Generation);
  for ThreadIndex:=0 to CurrentThreadList^.Count-1 do begin
   CurrentThread:=@CurrentThreadList^.Threads[ThreadIndex];
   Instruction:=CurrentThread^.Instruction;
   State:=CurrentThread^.State;
   case Instruction^.IndexAndOpcode and $ff of
    opSINGLECHAR,opCHAR,opANY:begin
     ThreadLocalStorage.ParallelNFAStateRelease(State);
    end;
    opMATCH:begin
     if rfLONGEST in Flags then begin
      if not assigned(BestState) then begin
       BestState:=ThreadLocalStorage.ParallelNFAStateAllocate(CountSubMatches,State^.BitState);
      end;
      if State^.BitState<>0 then begin
       if LastPosition<UntilExcludingPosition then begin
        LastPosition:=UntilExcludingPosition;
        BestState^.BitState:=State^.BitState;
        Move(State^.SubMatches[0],BestState^.SubMatches[0],State^.Count*SizeOf(TFLREParallelNFAStateItem));
       end;
      end;
     end else begin
      if assigned(Matched) then begin
       ThreadLocalStorage.ParallelNFAStateRelease(Matched);
      end;
      Matched:=State;
      for Counter:=ThreadIndex+1 to CurrentThreadList^.Count-1 do begin
       ThreadLocalStorage.ParallelNFAStateRelease(CurrentThreadList^.Threads[Counter].State);
      end;
      break;
     end;
    end;
   end;
  end;
 end;

 if assigned(BestState) then begin
  if assigned(Matched) then begin
   ThreadLocalStorage.ParallelNFAStateRelease(Matched);
  end;
  Matched:=BestState;
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
  ThreadLocalStorage.ParallelNFAStateRelease(Matched);
  result:=true;
 end;

end;

function TFLRE.SearchMatchOnePassNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint):boolean;
var State,Nodes:PFLREOnePassNFAState;
    InputLength,CurrentPosition,StateSize,TwoCountOfCaptures,Counter:longint;
    LocalByteMap:PFLREByteMap;
    Done:boolean;
    NextMatchCondition,MatchCondition,Condition,NextIndex:longword;
    Input:pansichar;
 function Satisfy(Condition:longword):boolean;
 begin
  result:=((Condition and sfEmptyAllFlags) and not ThreadLocalStorage.GetSatisfyFlags(CurrentPosition))=0;
 end;
begin
 TwoCountOfCaptures:=CountParens*2;

 Input:=ThreadLocalStorage.Input;
 InputLength:=ThreadLocalStorage.InputLength;
 
 State:=OnePassNFAStart;
 Nodes:=OnePassNFANodes;
 StateSize:=OnePassNFAStateSize;
 LocalByteMap:=@ByteMap;

 result:=false;
 Done:=false;

 NextMatchCondition:=State^.MatchCondition;
 Condition:=0;
 CurrentPosition:=StartPosition;

 while CurrentPosition<UntilExcludingPosition do begin
 
  Condition:=State^.Action[LocalByteMap^[byte(ansichar(Input[CurrentPosition]))]];
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

function TFLRE.SearchMatchBitStateNFA(const ThreadLocalStorage:TFLREThreadLocalStorage;var Captures:TFLRECaptures;const StartPosition,UntilExcludingPosition:longint;const UnanchoredStart:boolean):longint;
var InputLength,BasePosition,Len:longint;
    Input:PAnsiChar;
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
  begin
   result:=((NextFlags and sfEmptyAllFlags) and not ThreadLocalStorage.GetSatisfyFlags(Position))=0;
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

 Input:=ThreadLocalStorage.Input;
 InputLength:=ThreadLocalStorage.InputLength;

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

function TFLRE.SearchMatchDFA(const ThreadLocalStorage:TFLREThreadLocalStorage;const StartPosition,UntilExcludingPosition:longint;out MatchEnd:longint;const UnanchoredStart:boolean):longint;
var InputLength,Position:longint;
    State,LastState:PFLREDFAState;
    Input:pansichar;
begin
 result:=DFAFail;
 Input:=ThreadLocalStorage.Input;
 InputLength:=ThreadLocalStorage.InputLength;
 if UnanchoredStart then begin
  State:=ThreadLocalStorage.DFAUnanchoredStartState;
 end else begin
  State:=ThreadLocalStorage.DFAAnchoredStartState;
 end;
 for Position:=StartPosition to UntilExcludingPosition-1 do begin
  LastState:=State;
  State:=State^.NextStates[ByteMap[byte(ansichar(Input[Position]))]];
  if not assigned(State) then begin
   State:=ThreadLocalStorage.DFAProcessNextState(LastState,Input[Position],false);
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

function TFLRE.SearchMatchReversedDFA(const ThreadLocalStorage:TFLREThreadLocalStorage;const StartPosition,UntilIncludingPosition:longint;out MatchBegin:longint):longint;
var InputLength,Position:longint;
    State,LastState:PFLREDFAState;
    Input:pansichar;
begin
 result:=DFAFail;
 Input:=ThreadLocalStorage.Input;
 InputLength:=ThreadLocalStorage.InputLength;
 State:=ThreadLocalStorage.DFAReversedStartState;
 for Position:=StartPosition downto UntilIncludingPosition do begin
  LastState:=State;
  State:=State^.NextStates[ByteMap[byte(ansichar(Input[Position]))]];
  if not assigned(State) then begin
   State:=ThreadLocalStorage.DFAProcessNextState(LastState,Input[Position],true);
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

function TFLRE.SearchMatch(const AInput:pointer;const AInputLength:longint;var Captures:TFLRECaptures;StartPosition,UntilExcludingPosition:longint;UnanchoredStart:boolean):boolean;
var MatchBegin,MatchEnd:longint;
    HaveResult:boolean;
    ThreadLocalStorage:TFLREThreadLocalStorage;
begin
 result:=false;
 ThreadLocalStorageCriticalSection.Enter;
 try
  ThreadLocalStorage:=FreeThreadLocalStorages;
  if assigned(ThreadLocalStorage) then begin
   FreeThreadLocalStorages:=ThreadLocalStorage.FreeNext;
  end else begin
   ThreadLocalStorage:=TFLREThreadLocalStorage.Create(self);
   ThreadLocalStorage.AllNext:=ThreadLocalStorages;
   ThreadLocalStorages:=ThreadLocalStorage;
  end;
 finally
  ThreadLocalStorageCriticalSection.Leave;
 end;
 try
  ThreadLocalStorage.Input:=AInput;
  ThreadLocalStorage.InputLength:=AInputLength;
  repeat
   case SearchMatchDFA(ThreadLocalStorage,StartPosition,UntilExcludingPosition,MatchEnd,UnanchoredStart) of
    DFAMatch:begin
     if UnanchoredStart then begin
      // For unanchored searchs, we must do also a "backward" DFA search
      case SearchMatchReversedDFA(ThreadLocalStorage,MatchEnd,StartPosition,MatchBegin) of
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
         break;
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
      break;
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
     break;
    end;
    else {DFAError:}begin
     // Internal error?
{$ifdef debug}
     Assert(false,'Internal error in DFA code');
{$endif}
    end;
   end;
   if OnePassNFAReady and not UnanchoredStart then begin
    result:=SearchMatchOnePassNFA(ThreadLocalStorage,Captures,StartPosition,UntilExcludingPosition);
   end else begin
    if BitStateNFAReady then begin
     case SearchMatchBitStateNFA(ThreadLocalStorage,Captures,StartPosition,UntilExcludingPosition,UnanchoredStart) of
      BitStateNFAFail:begin
       result:=false;
       break;
      end;
      BitStateNFAMatch:begin
       result:=true;
       break;
      end;
(*    else{BitStateNFAError:}begin
      end;*)
     end;
    end;
    result:=SearchMatchParallelNFA(ThreadLocalStorage,Captures,StartPosition,UntilExcludingPosition,UnanchoredStart);
   end;
   break;
  until true;
 finally
  ThreadLocalStorageCriticalSection.Enter;
  try
   ThreadLocalStorage.FreeNext:=FreeThreadLocalStorages;
   FreeThreadLocalStorages:=ThreadLocalStorage;
  finally
   ThreadLocalStorageCriticalSection.Leave;
  end;
 end;
end;

function TFLRE.PtrMatch(const Input:pointer;const InputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
begin
 result:=SearchMatch(Input,InputLength,Captures,StartPosition,InputLength,false);
end;

function TFLRE.PtrMatchNext(const Input:pointer;const InputLength:longint;var Captures:TFLRECaptures;const StartPosition:longint=0):boolean;
var CurrentPosition:longint;
begin
 result:=false;
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
       while (CurrentPosition<InputLength) and not (PAnsiChar(Input)[CurrentPosition] in PrefixCharClasses[0]) do begin
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
   if SearchMatch(Input,InputLength,Captures,CurrentPosition,InputLength,DoUnanchoredStart) then begin
    result:=true;
    exit;
   end;
   inc(CurrentPosition);
  until (CurrentPosition>=InputLength) or (BeginningWildcardLoop or BeginningAnchor);
 end;
end;

function TFLRE.PtrMatchAll(const Input:pointer;const InputLength:longint;var Captures:TFLREMultiCaptures;const StartPosition:longint=0;Limit:longint=-1):boolean;
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
   while (CurrentPosition<InputLength) and (Limit<>0) and PtrMatchNext(Input,InputLength,MatchResult,CurrentPosition) do begin
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

function TFLRE.PtrReplaceAll(const Input:pointer;const InputLength:longint;const AReplacement:pointer;const AReplacementLength:longint;const StartPosition:longint=0;Limit:longint=-1):ansistring;
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
   while (CurrentPosition<InputLength) and (Limit<>0) and PtrMatchNext(Input,InputLength,Captures,CurrentPosition) do begin
    Next:=CurrentPosition+1;
    if (Captures[0].Start+Captures[0].Length)=LastPosition then begin
     CurrentPosition:=Captures[0].Start+Captures[0].Length;
     if CurrentPosition<Next then begin
      CurrentPosition:=Next;
     end;
    end else begin
     if LastPosition<Captures[0].Start then begin
      result:=result+PtrCopy(PAnsiChar(Input),LastPosition,Captures[0].Start-LastPosition);
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
            result:=result+PtrCopy(PAnsiChar(Input),Captures[0].Start,Captures[0].Length);
            inc(i);
           end;
           '`':begin
            result:=result+PtrCopy(PAnsiChar(Input),0,Captures[0].Start-1);
            inc(i);
           end;
           '''':begin
            result:=result+PtrCopy(PAnsiChar(Input),Captures[0].Start+Captures[0].Length,(InputLength-(Captures[0].Start+Captures[0].Length))+1);
            inc(i);
           end;
           '_':begin
            result:=result+AnsiString(PAnsiChar(Input));
            inc(i);
           end;
           '-':begin
            if length(Captures)>1 then begin
             result:=result+PtrCopy(PAnsiChar(Input),Captures[1].Start,Captures[1].Length);
            end;
            inc(i);
           end;
           '+':begin
            if length(Captures)>1 then begin
             e:=length(Captures)-1;
             result:=result+PtrCopy(PAnsiChar(Input),Captures[e].Start,Captures[e].Length);
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
               result:=result+PtrCopy(PAnsiChar(Input),Captures[e].Start,Captures[e].Length);
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
              result:=result+PtrCopy(PAnsiChar(Input),Captures[e].Start,Captures[e].Length);
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
             result:=result+PtrCopy(PAnsiChar(Input),Captures[e].Start,Captures[e].Length);
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
   if LastPosition<InputLength then begin
    result:=result+PtrCopy(PAnsiChar(Input),LastPosition,InputLength-LastPosition);
   end;
  end;
 finally
  SetLength(Captures,0);
 end;
end;

function TFLRE.Match(const Input:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
var Counter:longint;
begin
 result:=PtrMatch(pansichar(@Input[1]),length(Input),Captures,StartPosition-1);
 for Counter:=0 to length(Captures)-1 do begin
  if Captures[Counter].Length>0 then begin
   inc(Captures[Counter].Start);
  end;
 end;
end;

function TFLRE.MatchNext(const Input:ansistring;var Captures:TFLRECaptures;const StartPosition:longint=1):boolean;
var Counter:longint;
begin
 result:=PtrMatchNext(pansichar(@Input[1]),length(Input),Captures,StartPosition-1);
 for Counter:=0 to length(Captures)-1 do begin
  if Captures[Counter].Length>0 then begin
   inc(Captures[Counter].Start);
  end;
 end;
end;

function TFLRE.MatchAll(const Input:ansistring;var Captures:TFLREMultiCaptures;const StartPosition:longint=1;Limit:longint=-1):boolean;
var Counter,SubCounter:longint;
begin
 result:=PtrMatchAll(pansichar(@Input[1]),length(Input),Captures,StartPosition-1,Limit);
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
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    UnicodeScriptHashMap.SetValue(UTF8Correct(s),i);
    UnicodeScriptHashMap.SetValue(UTF8Correct('In'+s),i);
    UnicodeScriptHashMap.SetValue(UTF8Correct('Is'+s),i);
   end;
  end;
  begin
   UnicodeBlockHashMap:=TFLREStringIntegerPairHashMap.Create;
   for i:=FLREUnicodeScriptCommon to FLREUnicodeBlockCount-1 do begin
    s:=AnsiString(StringReplace(String(FLREUnicodeBlocks[i].Name),' ','_',[rfREPLACEALL]));
    UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
    UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
    UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
    s:=AnsiString(StringReplace(String(s),'_','',[rfREPLACEALL]));
    UnicodeBlockHashMap.SetValue(UTF8Correct(s),i);
    UnicodeBlockHashMap.SetValue(UTF8Correct('In'+s),i);
    UnicodeBlockHashMap.SetValue(UTF8Correct('Is'+s),i);
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


