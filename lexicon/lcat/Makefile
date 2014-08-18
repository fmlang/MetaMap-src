include ../../Makefile.include
CC=gcc
PIC=-fpic

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  LDFLAGS=-G
  LIBS = -lrt -lnsl -lsocket -ldl
else
  ifeq '$(O_SYSTEM)' "Linux"
    ARCH=Linux-i686
  endif
endif

INCLUDES=-I../include -I../../debug/include
CFLAGS=-g $(INCLUDES) $(DEFAULT_CFLAGS)

LIBLEXBTREE=../query/lex_btree.a
LIBDEBUG=../../debug/debug.a
LIBMISCUTIL=../miscutil/miscutil.a
LIBFUNCS=../functions/lexicon.a
LIBLM=../morph/liblm.a	

LEXLIBS=$(LIBLEXBTREE) $(LIBDEBUG) $(LIBMISCUTIL) $(LIBFUNCS) $(LIBLM) \
     $(BERKELEY_LIB)/libdb.a -lm 

lcat : lcat.o
	$(CC) $(LDFLAGS) -o lcat lcat.o $(LEXLIBS) $(LIBS)

lcat.o : lcat.c

