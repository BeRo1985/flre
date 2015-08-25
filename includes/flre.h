/**********************************************************
** FLRE Regular Expression Library                        *
***********************************************************
**
** This file is part of the FLRE Regular Expression Library.
** Copyright (C) 2011-2012 by Benjamin Rosseaux
**
** See the file COPYING.FLRE, included in this distribution,
** for details about the copyright.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*/
#ifndef FLRE_H
#define FLRE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#ifdef __i386__
#define flreCALLCONV __stdcall
#define flreLIBIMPORT __declspec(dllimport)
#else
#define flreCALLCONV
#endif

#define flrefIGNORECASE (1 << 0)
#define flrefSINGLELINE (1 << 1)
#define flrefMULTILINE (1 << 2)
#define flrefFREESPACING (1 << 3)
#define flrefNAMED (1 << 4)
#define flrefUNGREEDY (1 << 5)
#define flrefLONGEST (1 << 6)
#define flrefUTF8 (1 << 7)
#define flrefDELIMITERS (1 << 8)
#define flrefSAFE (1 << 9)
#define flrefFAST (1 << 10)

//#pragma pack(push)
//#pragma pack(1)

typedef void* TFLRE;

//#pragma pack(pop)

flreLIBIMPORT int64_t flreCALLCONV FLREGetVersion();
flreLIBIMPORT char* flreCALLCONV FLREGetVersionString();
flreLIBIMPORT int32_t flreCALLCONV FLRECreate(TFLRE Instance, char* RegExp, int32_t RegExpLen, uint32_t Flags, char** Error);
flreLIBIMPORT void flreCALLCONV FLREDestroy(TFLRE Instance);
flreLIBIMPORT void flreCALLCONV FLREFree(TFLRE Instance, void* p);
flreLIBIMPORT int32_t flreCALLCONV FLREGetCountCaptures(TFLRE Instance);
flreLIBIMPORT int32_t flreCALLCONV FLREGetNamedGroupIndex(TFLRE Instance, char* GroupName);
flreLIBIMPORT int32_t flreCALLCONV FLRESDumpRegularExpression(TFLRE Instance, char** RegularExpression, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREGetPrefilterExpression(TFLRE Instance, char** Expression, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREGetPrefilterShortExpression(TFLRE Instance, char** ShortExpression, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREGetPrefilterSQLBooleanFullTextExpression(TFLRE Instance, char** SQLBooleanFullTextExpression,char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREGetPrefilterSQLExpression(TFLRE Instance, char* Field, char** SQLExpression, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREGetRange(TFLRE Instance, char** RangeLow, char** RangeHigh, int32_t* RangeLowLength, int32_t* RangeHighLength, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREMatch(TFLRE Instance, char* Input, int32_t InputLength, void** Captures, int32_t MaxCaptures, int32_t* CountCaptures, int32_t StartPosition, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREMatchNext(TFLRE Instance, char* Input, int32_t InputLength, void** Captures, int32_t MaxCaptures, int32_t* CountCaptures, int32_t StartPosition, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREMatchAll(TFLRE Instance, char* Input, int32_t InputLength, void** MultiCaptures, int32_t MaxMultiCaptures, int32_t* CountMultiCaptures, int32_t StartPosition, int32_t Limit, char** Error);
flreLIBIMPORT int32_t flreCALLCONV FLREReplaceAll(TFLRE Instance, char* Input, int32_t InputLength, char* Replacement, int32_t ReplacementLength, char** ResultString, int32_t* ResultStringLength, int32_t StartPosition, int32_t Limit, char** Error);

#ifdef __cplusplus
}
#endif

#endif
