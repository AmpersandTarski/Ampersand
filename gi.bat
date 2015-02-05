rem ghci -Wall -package-db .cabal-sandbox/x86_64-windows-ghc-7.8.3-packages.conf.d -isrc/exec:src/lib:dist/build/autogen:src:/MinGW/bin Main
ghci -Wall ^
	-isrc/exec:src/lib:dist/build/autogen:src:src/Database/Design/Ampersand/Test:/MinGW/bin ^
	src/Database/Design/Ampersand/Test/Parser/ParseScripts