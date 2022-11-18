@echo off
fpc -FUunits -Twin64 -Px86_64 -olibFLRE_x86_64.dll -B libFLRE.dpr
fpc -FUunits -Tlinux -Pi386 -olibFLRE_i386.so -B libFLRE.dpr
fpc -FUunits -Tlinux -Px86_64 -olibFLRE_x86_64.so -B libFLRE.dpr
fpc -FUunits -Tlinux -Paarch64 -olibFLRE_aarch64.so -B libFLRE.dpr
