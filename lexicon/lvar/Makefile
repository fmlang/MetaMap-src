O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  LDFLAGS=-G
  LIBS =  -lsocket -lnsl -lrt
else
  ifeq '$(O_SYSTEM)' "Linux"
    LD = gcc
    LDFLAGS=-m32 -shared
    LIBS = -lpthread
  endif
endif


SPLIBDIR=../lib
LEXICONSRC=../lexicon
MISCUTIL=../miscutil
QUERYSRC=../query
INC=../include 
DEBUGSRC=../../debug
DEBUGINC=$(DEBUGSRC)/include
FUNCTIONS=../functions

BDBLIB=$(BERKELEY)/lib

# Include paths
INCLUDES = -I$(INC) -I$(DEBUGINC) -I$(FUNCTIONS)

# Libraries
LIBRARIES = \
	$(LEXICONSRC)/qp_lexicon.so \
	$(FUNCTIONS)/lexicon.a \
	$(SPLIBDIR)/liblm.a \
	$(MISCUTIL)/miscutil.a \
	$(DEBUGSRC)/debug.a

# Libs on compile line
LLIBS = $(LIBRARIES)  -lm $(LIBS)

# compiler flags
CFLAGS=-g -m32 $(INCLUDES)
LDFLAGS=-m32

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
