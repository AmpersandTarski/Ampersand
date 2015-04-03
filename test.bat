@echo off
((ghc -fhpc -with-rtsopts="-M1536m -H256m -K128m" -o runtest.exe -Wall ^
	-isrc/exec:src/lib:dist/build/autogen:src:src/Database/Design/Ampersand/Test:/MinGW/bin ^
	src/Database/Design/Ampersand/Test/Main )^
&& runtest.exe && echo Returned 0 && hpc markup runtest.exe.tix --destdir=hpc) ^
|| echo Failure!
