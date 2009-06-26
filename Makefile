.PHONY: ghci
default: adl

# On Windows machines (under cygwin), ghc automatically puts the .exe suffix after
# all executables it generates. This gives some problems when we have to call this
# executable later on. Hence, we first test whether we are on a windows machine or 
# not (check whether Win32 is listed under ghc's installed packages).
#
# on Windows: EXE = .exe
# otherwise:  EXE = 

WINDOWS = $(shell ghc-pkg list | grep -q Win32 && echo yes || echo no)

SOURCES = $(wildcard src/*.lhs) $(wildcard src/*.hs) $(wildcard src/*/*.hs)

ifeq ($(WINDOWS), yes)
EXE  = .exe
GHCI = ghcii.sh
else
EXE  = 
GHCI = ghci
endif

FLAGS = -Wall --make -O -isrc -odir out -hidir out

ghci:
	$(GHCI) -Wall -isrc -odir out -hidir out

adl: bin/adl$(EXE)

bin/adl$(EXE): $(SOURCES) out bin
	ghc $(FLAGS) -o $@ src/Main.hs
	cp src/Rendering/customheader.tex bin/customheader.tex
	strip $@

out:
	mkdir out
	
bin:
	mkdir bin


