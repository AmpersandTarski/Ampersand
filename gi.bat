@echo off

if "%~1" == "" (
	echo Please give either 'main' or 'test' as parameter
) else if "%~1" == "main" (
	ghci -Wall -isrc/exec:src/lib:dist/build/autogen:src Main
) else if "%~1" == "test" (
	ghci -Wall -isrc/exec:src/lib:dist/build/autogen:src:src/Database/Design/Ampersand/Test src\Main
) else (
	echo "Module '%1' not configured in %0"
)
