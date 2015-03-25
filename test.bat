@echo off
((ghc -with-rtsopts="-M1536m -H256m -K128m" -o runtest.exe -Wall ^
	-isrc/exec:src/lib:dist/build/autogen:src:src/Database/Design/Ampersand/Test:/MinGW/bin ^
	src/Database/Design/Ampersand/Test/Main )^
&& runtest.exe && echo Returned 0) || echo Failure!
