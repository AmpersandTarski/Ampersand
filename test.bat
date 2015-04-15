@echo off
del runtest.exe.*
((ghc -fhpc -with-rtsopts="-M1536m -H256m -K128m" -o runtest.exe -Wall ^
	-isrc/exec:src/lib:dist/build/autogen:src:/MinGW/bin ^
	src/Test.hs )^
&& runtest.exe && echo Success!  See test.adl for warnings. && hpc markup runtest.exe.tix --destdir=hpc) 2> test.adl ^
|| echo Failure! See test.adl for details.
