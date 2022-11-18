#ifndef LIBFLRE_H
#define LIBFLRE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdio.h>

#if defined(_WIN32) || defined(_WIN64)
  #define WINDOWS
#endif
#ifdef WINDOWS
  #include <windows.h>
#else
  #include <dlfcn.h>
  #include <sys/types.h>
#endif

typedef char TFLREChar;

typedef TFLREChar* PFLREChar;

typedef void* TFLREInstance;

#if defined(__GNUC__) && (defined(WIN32) || defined(_WIN32)) && (defined(i386) || defined(__i386) || defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__) || defined(_M_IX86) || defined(__X86__) || defined(_X86_) || defined(__386)) 
#define FLRE_CALLCONV __attribute__((stdcall))
#elif defined(_MSC_VER) && (defined(WIN32) || defined(_WIN32)) && (defined(i386) || defined(__i386) || defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__) || defined(_M_IX86) || defined(__X86__) || defined(_X86_) || defined(__386))
#define FLRE_CALLCONV __stdcall
#else
// Just use the default target system call convention in this case  
#define FLRE_CALLCONV
#endif

static const uint32_t FLRE_carfIGNORECASE = 1u << 0;
static const uint32_t FLRE_carfSINGLELINE = 1u << 1;
static const uint32_t FLRE_carfMULTILINE = 1u << 2;
static const uint32_t FLRE_carfFREESPACING = 1u << 3;
static const uint32_t FLRE_carfNAMED = 1u << 4;
static const uint32_t FLRE_carfNOCAPTURES = 1u << 5;
static const uint32_t FLRE_carfUNGREEDY = 1u << 6;
static const uint32_t FLRE_carfLONGEST = 1u << 7;
static const uint32_t FLRE_carfMULTIMATCH = 1u << 8;
static const uint32_t FLRE_carfUTF8 = 1u << 9;
static const uint32_t FLRE_carfONLYFASTOPTIMIZATIONS = 1u << 10;
static const uint32_t FLRE_carfDELIMITERS = 1u << 11;
 
typedef FLRE_CALLCONV uint32_t (*_FLREGetVersion)();
typedef FLRE_CALLCONV PFLREChar (*_FLREGetVersionString)();
typedef FLRE_CALLCONV TFLREInstance (*_FLRECreate)(PFLREChar RegularExpression, int32_t RegularExpressionLength, uint32_t Flags, PFLREChar* Error);
typedef FLRE_CALLCONV TFLREInstance (*_FLREDestroy)(TFLREInstance Instance);
typedef FLRE_CALLCONV void (*_FLREFree)(void* Data);
typedef FLRE_CALLCONV int32_t (*_FLREGetCountCaptures)(TFLREInstance Instance);
typedef FLRE_CALLCONV int32_t (*_FLREGetNamedGroupIndex)(TFLREInstance Instance, PFLREChar GroupName);
typedef FLRE_CALLCONV int32_t (*_FLREDumpRegularExpression)(TFLREInstance Instance, PFLREChar* RegularExpression, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREGetPrefilterExpression)(TFLREInstance Instance, PFLREChar* Expression, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREGetPrefilterShortExpression)(TFLREInstance Instance, PFLREChar* ShortExpression, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREGetPrefilterSQLBooleanFullTextExpression)(TFLREInstance Instance, PFLREChar* SQLBooleanFullTextExpression, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREGetPrefilterSQLExpression)(TFLREInstance Instance, PFLREChar* SQLExpression, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREGetRange)(TFLREInstance Instance, PFLREChar* LowRange, PFLREChar* HighRange, int32_t* LowRangeLength, int32_t* HighRangeLength, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREMatch)(TFLREInstance Instance, void* Input, int32_t InputLength, void** Captures, int32_t MaxCaptures, int32_t* CountCaptures, int32_t StartPosition, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREMatchNext)(TFLREInstance Instance, void* Input, int32_t InputLength, void** Captures, int32_t MaxCaptures, int32_t* CountCaptures, int32_t StartPosition, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREMatchAll)(TFLREInstance Instance, void* Input, int32_t InputLength, void** MultiCaptures, int32_t MaxMultiCaptures, int32_t* CountMultiCaptures, int32_t* CountCaptures, int32_t StartPosition, int32_t Limit, PFLREChar* Error);
typedef FLRE_CALLCONV int32_t (*_FLREReplaceAll)(TFLREInstance Instance, void* Input, int32_t InputLength, void* Replacement, int32_t ReplacementLength, void** ResultString, int32_t* ResultStringLength, int32_t StartPosition, int32_t Limit, PFLREChar* Error);

extern _FLREGetVersion FLREGetVersion;
extern _FLREGetVersionString FLREGetVersionString;
extern _FLRECreate FLRECreate;
extern _FLREDestroy FLREDestroy;
extern _FLREFree FLREFree;
extern _FLREGetCountCaptures FLREGetCountCaptures;
extern _FLREGetNamedGroupIndex FLREGetNamedGroupIndex;
extern _FLREDumpRegularExpression FLREDumpRegularExpression;
extern _FLREGetPrefilterExpression FLREGetPrefilterExpression;
extern _FLREGetPrefilterShortExpression FLREGetPrefilterShortExpression;
extern _FLREGetPrefilterSQLBooleanFullTextExpression FLREGetPrefilterSQLBooleanFullTextExpression;
extern _FLREGetPrefilterSQLExpression FLREGetPrefilterSQLExpression;
extern _FLREGetRange FLREGetRange;
extern _FLREMatch FLREMatch;
extern _FLREMatchNext FLREMatchNext;
extern _FLREMatchAll FLREMatchAll;
extern _FLREReplaceAll FLREReplaceAll;

extern int32_t FLRELoaded;

#ifdef WINDOWS
extern HINSTANCE FLRELibraryHandle;
#else
extern void* FLRELibraryHandle;
#endif

extern int32_t FLRELoad();
extern int32_t FLREUnload();

#ifdef LIBFLRE_IMPL
int32_t FLRELoaded = 0;

#ifdef WINDOWS
HINSTANCE FLRELibraryHandle = NULL;
#else
void* FLRELibraryHandle = NULL;
#endif

_FLREGetVersion FLREGetVersion = NULL;
_FLREGetVersionString FLREGetVersionString = NULL;
_FLRECreate FLRECreate = NULL;
_FLREDestroy FLREDestroy = NULL;
_FLREFree FLREFree = NULL;
_FLREGetCountCaptures FLREGetCountCaptures = NULL;
_FLREGetNamedGroupIndex FLREGetNamedGroupIndex = NULL;
_FLREDumpRegularExpression FLREDumpRegularExpression = NULL;
_FLREGetPrefilterExpression FLREGetPrefilterExpression = NULL;
_FLREGetPrefilterShortExpression FLREGetPrefilterShortExpression = NULL;
_FLREGetPrefilterSQLBooleanFullTextExpression FLREGetPrefilterSQLBooleanFullTextExpression = NULL;
_FLREGetPrefilterSQLExpression FLREGetPrefilterSQLExpression = NULL;
_FLREGetRange FLREGetRange = NULL;
_FLREMatch FLREMatch = NULL;
_FLREMatchNext FLREMatchNext = NULL;
_FLREMatchAll FLREMatchAll = NULL;
_FLREReplaceAll FLREReplaceAll = NULL;

int32_t FLRELoad(){
  if(FLRELoaded){
    return 1;
  }else{ 
#ifdef WINDOWS
#if defined(i386) || defined(__i386) || defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__) || defined(_M_IX86) || defined(__X86__) || defined(_X86_) || defined(__386))
    FLRELibraryHandle = dlopen("libFLRE_i386.dll");
#elif defined(__amd64) || defined(__amd64__) || defined(__x86_64) || defined(__x86_64__) || defined(_M_AMD64) || defined(_M_X64)
    FLRELibraryHandle = LoadLibrary("libFLRE_x86_64.dll");
#elif defined(__aarch64) || defined(__aarch64__)
    FLRELibraryHandle = LoadLibrary("libFLRE_aarch64.dll");
#elif defined(__arm__)
    FLRELibraryHandle = LoadLibrary("libFLRE_arm32.dll");
#else
    #error "Unsupported CPU/OS target combination"
#endif
    if(FLRELibraryHandle == NULL){
      FLRELibraryHandle = LoadLibrary("libFLRE.dll");
    }
#else
#if defined(i386) || defined(__i386) || defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__) || defined(_M_IX86) || defined(__X86__) || defined(_X86_) || defined(__386))
    FLRELibraryHandle = dlopen("libFLRE_i386.so");
#elif defined(__amd64) || defined(__amd64__) || defined(__x86_64) || defined(__x86_64__) || defined(_M_AMD64) || defined(_M_X64)
    FLRELibraryHandle = dlopen("libFLRE_x86_64.so");
#elif defined(__aarch64) || defined(__aarch64__)
    FLRELibraryHandle = dlopen("libFLRE_aarch64.so");
#elif defined(__arm__)
    FLRELibraryHandle = dlopen("libFLRE_arm32.so");
#else
    #error "Unsupported CPU/OS target combination"
#endif    
    if(FLRELibraryHandle == NULL){
      FLRELibraryHandle = dlopen("libFLRE.so");
    }
#endif          
    if(FLRELibraryHandle != NULL){
#ifdef WINDOWS
#define FLREGetProcAddress GetProcAddress
#else
#define FLREGetProcAddress dlsym
#endif
      FLREGetVersion = (_FLREGetVersion)FLREGetProcAddress(FLRELibraryHandle, "FLREGetVersion");
      FLREGetVersionString = (_FLREGetVersionString)FLREGetProcAddress(FLRELibraryHandle, "FLREGetVersionString");
      FLRECreate = (_FLRECreate)FLREGetProcAddress(FLRELibraryHandle, "FLRECreate");
      FLREDestroy = (_FLREDestroy)FLREGetProcAddress(FLRELibraryHandle, "FLREDestroy");
      FLREFree = (_FLREFree)FLREGetProcAddress(FLRELibraryHandle, "FLREFree");
      FLREGetCountCaptures = (_FLREGetCountCaptures)FLREGetProcAddress(FLRELibraryHandle, "FLREGetCountCaptures");
      FLREGetNamedGroupIndex = (_FLREGetNamedGroupIndex)FLREGetProcAddress(FLRELibraryHandle, "FLREGetNamedGroupIndex");
      FLREDumpRegularExpression = (_FLREDumpRegularExpression)FLREGetProcAddress(FLRELibraryHandle, "FLREDumpRegularExpression");
      FLREGetPrefilterExpression = (_FLREGetPrefilterExpression)FLREGetProcAddress(FLRELibraryHandle, "FLREGetPrefilterExpression");
      FLREGetPrefilterShortExpression = (_FLREGetPrefilterShortExpression)FLREGetProcAddress(FLRELibraryHandle, "FLREGetPrefilterShortExpression");
      FLREGetPrefilterSQLBooleanFullTextExpression = (_FLREGetPrefilterSQLBooleanFullTextExpression)FLREGetProcAddress(FLRELibraryHandle, "FLREGetPrefilterSQLBooleanFullTextExpression");
      FLREGetPrefilterSQLExpression = (_FLREGetPrefilterSQLExpression)FLREGetProcAddress(FLRELibraryHandle, "FLREGetPrefilterSQLExpression");
      FLREGetRange = (_FLREGetRange)FLREGetProcAddress(FLRELibraryHandle, "FLREGetRange");
      FLREMatch = (_FLREMatch)FLREGetProcAddress(FLRELibraryHandle, "FLREMatch");
      FLREMatchNext = (_FLREMatchNext)FLREGetProcAddress(FLRELibraryHandle, "FLREMatchNext");
      FLREMatchAll = (_FLREMatchAll)FLREGetProcAddress(FLRELibraryHandle, "FLREMatchAll");
      FLREReplaceAll = (_FLREReplaceAll)FLREGetProcAddress(FLRELibraryHandle, "FLREReplaceAll");
#undef FLREGetProcAddress
      if(FLREGetVersion &&
         FLREGetVersionString &&
         FLRECreate &&
         FLREDestroy &&
         FLREFree &&
         FLREGetCountCaptures &&
         FLREGetNamedGroupIndex &&
         FLREDumpRegularExpression &&
         FLREGetPrefilterExpression &&
         FLREGetPrefilterShortExpression &&
         FLREGetPrefilterSQLBooleanFullTextExpression &&
         FLREGetPrefilterSQLExpression &&
         FLREGetRange &&
         FLREMatch &&
         FLREMatchNext &&
         FLREMatchAll &&
         FLREReplaceAll){
        FLRELoaded = 1;
        return 1;
      }     
    }
    return 0;
  } 
}

int32_t FLREUnload(){
  if(FLRELoaded){
#ifdef WINDOWS
    FreeLibrary(FLRELibraryHandle);
#else
    dlclose(FLRELibraryHandle);
#endif    
    FLRELoaded = 0;
  }
  return 0;
}

#endif

#ifdef __cplusplus
}
#endif

#endif
