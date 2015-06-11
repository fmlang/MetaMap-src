#!/bin/env make
include ../../Makefile.include

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  LDFLAGS=-G
  LIBS =  -lsocket -lnsl -lrt
endif

SPLIBDIR=../lib
LEXICONSRC=../lexicon
MISCUTIL=../miscutil
QUERYSRC=../query
INC=../include 
DEBUGSRC=../../debug
DEBUGINC=$(DEBUGSRC)/include
FUNCTIONS=../functions
MORPH=../morph

BDBLIB=$(BERKELEY)/lib

# Include paths
INCLUDES = -I$(INC) -I$(DEBUGINC) -I$(FUNCTIONS)

# Libraries
LIBRARIES = \
	$(FUNCTIONS)/lexicon.a \
	$(MORPH)/liblm.a \
	$(MISCUTIL)/miscutil.a \
	$(DEBUGSRC)/debug.a 
#	$(LEXICONSRC)/qp_lexicon.s.o

# Libs on compile line
LLIBS = $(LIBRARIES)  -lm $(LIBS)

# compiler flags
CFLAGS=-g $(INCLUDES) $(DEFAULT_CFLAGS)

# Source files
SRC = lvar.c

# Object modules
OBJ = lvar.o 

all: lvar

clean:
	rm -f *.o lvar

lvar.o : lvar.c

lvar : lvar.o
	$(CC) $(CFLAGS) -o $@ $< $(LLIBS) 


# fin
