
#############################################################################
#
#                          PUBLIC DOMAIN NOTICE                         
#         Lister Hill National Center for Biomedical Communications
#                      National Library of Medicine
#                      National Institues of Health
#           United States Department of Health and Human Services
#                                                                         
#  This software is a United States Government Work under the terms of the
#  United States Copyright Act. It was written as part of the authors'
#  official duties as United States Government employees and contractors
#  and thus cannot be copyrighted. This software is freely available
#  to the public for use. The National Library of Medicine and the
#  United States Government have not placed any restriction on its
#  use or reproduction.
#                                                                        
#  Although all reasonable efforts have been taken to ensure the accuracy 
#  and reliability of the software and data, the National Library of Medicine
#  and the United States Government do not and cannot warrant the performance
#  or results that may be obtained by using this software or data.
#  The National Library of Medicine and the U.S. Government disclaim all
#  warranties, expressed or implied, including warranties of performance,
#  merchantability or fitness for any particular purpose.
#                                                                         
#  For full details, please see the MetaMap Terms & Conditions, available at
#  http://metamap.nlm.nih.gov/MMTnCs.shtml.
#
############################################################################

# Makefile for lexicon related files.
#
# SCCS:	@(#)Makefile.lexicon	1.11 12/07/06

include ../../Makefile.include

PURIFY=
RM = /bin/rm -f
INSTALL = cp

CFLAGS = $(DEFAULT_CFLAGS) $(ALL_INCLUDES)

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  LDFLAGS=-G -shared
  LIBS = # -lnsl  -lsocket -lrt -llm  -lm
else
  ifeq '$(O_SYSTEM)' "Linux"
    LIBS = -lpthread -llm  -lm
  endif
endif

LIBDIR = ../lib
INCDIR= ../include

SPLIBS= -f lexicon:../lexicon		\
	-f metamap:$(SKR_METAMAP)			\
	-f mmi:$(SKR_MMI)				\
	-f morph:$(SKR_MORPH)				\
	-f skr:$(SKR_SKR)				\
	-f skr_db:$(SKR_DB)				\
	-f skr_lib:$(SKR_LIB)				\
	-f skr_lib:$(SKR_LEX_LIB)			\
	-f tagger:$(SKR_TAGGER)				\
	-f text:$(SKR_TEXT)				\
	-f wsd:$(SKR_WSD)/WSD

DEBUG_LIB = ../../debug/debug.a
SPLD = spld

INCLUDES = -I../include -I../../debug/include

SPLFR_BASIC_FLAGS =  --vv --keep $(SPCONFIG)
SPLFR_DYNAMIC_FLAGS = $(SPLFR_BASIC_FLAGS) --moveable
SPLFR_STATIC_FLAGS = $(SPLFR_BASIC_FLAGS) --static
SPLFR = splfr --conf CC="$(CC)" --cflag="$(INCLUDES)" $(addprefix --cflag=, $(CFLAGS))

STATIC_LINK_FLAGS = -- ../functions/linfl.o		\
		       ../functions/tokenize.o	\
		       ../morph/lm.o		\
		       ../morph/util.o		\
		       ../query/btree_query.o	\
		       ../query/tokenize.o		\
		       ../query/lex_btree.a		\
		       ../query/wl.o		\
		       ../miscutil/miscutil.o	\
		       ../../debug/get_val.o

DYNAMIC_LINK_FLAGS = -- $(SOLDFLAGS)			        \
			$(BERKELEY_LIB_SO)			\
			../functions/linfl.o	\
		        ../morph/lm.o		\
		        ../morph/util.o		\
			../functions/lexicon.a	\
			../query/lex_btree.a	\
			../miscutil/miscutil.a	\
			../../debug/debug.o			\
			../../debug/get_val.o


all:		splfr_STATIC splfr_DYNAMIC

splfr_STATIC:	functions query miscutil morph qp_lexicon.pl
		$(SPLFR) $(SPLFR_STATIC_FLAGS) qp_lexicon.pl c_lexicon.c c_linfl.c $(STATIC_LINK_FLAGS)

splfr_DYNAMIC:	functions query miscutil morph 
		$(SPLFR) $(SPLFR_DYNAMIC_FLAGS) qp_lexicon.pl  c_lexicon.c c_linfl.c $(BTREE_QLIB_O) $(DYNAMIC_LINK_FLAGS)


functions:
		cd ../functions && $(MAKE)

query:
		cd ../query && $(MAKE)

miscutil:
		cd ../miscutil && $(MAKE)

morph:
		cd ../morph && $(MAKE)

clean:
		-$(RM) *.o *.a *.s.o *.${SOEXT} *.s.o *glue* lcheck lcheckp lterm core
		cd ../functions && $(MAKE) clean
		cd ../query     && $(MAKE) clean
		cd ../miscutil  && $(MAKE) clean
		cd ../morph     && $(MAKE) clean 
