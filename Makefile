#!/usr/bin/make -f 

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

# Top-level makefile for tools directory.
#
# To use set environment variable "SKR" to location of SKR directory,
# usually ${HOME}/specialist/SKR.
#
#   $ make SKR=${HOME}/specialist/SKR
#
# In this case the main Makefile and the sub-directory Makefiles
# include the file "Makefile.include" (Makefile is used implicitly).
#
include Makefile.include

SICSTUSARGS=

APPNAME=metamap
SAVED_STATE=$(APPNAME).sav
BINEXEC=$(APPNAME).BINARY.$(ARCH)

all : build_debug build_lib build_db build_functions \
      build_miscutil build_morph build_query build_lexicon \
      build_runtime $(BINEXEC) $(MSBINEXEC) build_mmserver 

build_debug :
	cd debug && $(MAKE)

build_lib :
	cd lib && $(MAKE)

build_db :
	cd db && $(MAKE)

build_miscutil :
	cd lexicon/miscutil && $(MAKE)

build_functions :
	cd lexicon/functions && $(MAKE)

build_morph :
	cd lexicon/morph && $(MAKE)

build_query :
	cd lexicon/query && $(MAKE)

build_lexicon :
	cd lexicon/lexicon && $(MAKE)

build_mmserver :
	cd mmserver && $(MAKE)

# MetaMap targets
$(SAVED_STATE) :
	sicstus $(SICSTUSARGS) --goal "save_program('$(SAVED_STATE)'), halt."

$(BINEXEC) : $(SAVED_STATE)
	spld -vv $(CONF) --moveable --respath=$(RESPATH) $(PREFIX)/$(SAVED_STATE) --output=$(BINEXEC) $(LINK_FILES) $(LDFLAGS)

RT_DIR=$(SKR_SRC_HOME)/sp-4.2.0
RT_BIN=$(RT_DIR)/sicstus-4.2.0/bin
RT_LIB=$(RT_DIR)/sicstus-4.2.0/library
RT_NATIVE_LIB=$(RT_LIB)/x86-linux-glibc2.5

RT_PO_FILES = $(RT_LIB)/bdb.po        \
	$(RT_LIB)/between.po          \
	$(RT_LIB)/codesio.po          \
	$(RT_LIB)/fastrw.po           \
	$(RT_LIB)/file_systems.po     \
	$(RT_LIB)/lists.po            \
	$(RT_LIB)/ordsets.po          \
	$(RT_LIB)/prologbeans.po      \
	$(RT_LIB)/prologbeanserver.po \
	$(RT_LIB)/random.po           \
	$(RT_LIB)/sockets.po          \
	$(RT_LIB)/system.po           \
	$(RT_LIB)/terms.po            \
	$(RT_LIB)/timeout.po          \
	$(RT_LIB)/typesio.po          

RT_DLLS = $(RT_NATIVE_LIB)/fastrw.so $(RT_NATIVE_LIB)/jasper.so \
	$(RT_NATIVE_LIB)/random.so $(RT_NATIVE_LIB)/timeout.so \
	$(RT_NATIVE_LIB)/bdb.so $(RT_NATIVE_LIB)/clpfd.so  \
	$(RT_NATIVE_LIB)/codesio.so $(RT_NATIVE_LIB)/timeout.s.o \
	$(RT_NATIVE_LIB)/bdb.s.o $(RT_NATIVE_LIB)/clpfd.s.o \
	$(RT_NATIVE_LIB)/codesio.s.o $(RT_NATIVE_LIB)/fastrw.s.o \
	$(RT_NATIVE_LIB)/jasper.s.o $(RT_NATIVE_LIB)/jasper.s.o \
	$(SKR_SRC_HOME)/c_nls_db.so $(SKR_SRC_HOME)/db_access.so \
	$(SKR_SRC_HOME)/debug.so $(SKR_SRC_HOME)/nls_signal.so \
	$(SKR_SRC_HOME)/qp_morph.so $(SKR_SRC_HOME)/qp_lexicon.so 

build_runtime: make_rtdirs copy_dlls
	$(CP) $(SICSTUS_LIB)/libspnative.so           $(RT_DIR)
	$(CP) $(SICSTUS_LIB)/libsprt4-2-0*.so         $(RT_DIR)
	$(CP) $(SICSTUS_OBJECTS)/sprt.sav             $(RT_BIN)
	$(CP) $(SICSTUS_LIBRARY)/avl.po               $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/bdb.po               $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/between.po           $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/codesio.po           $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/fastrw.po            $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/file_systems.po      $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/lists.po             $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/ordsets.po           $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/prologbeans.po       $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/prologbeansserver.po $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/random.po            $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/sockets.po           $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/system.po            $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/terms.po             $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/timeout.po           $(RT_LIB)
	$(CP) $(SICSTUS_LIBRARY)/types.po             $(RT_LIB)

copy_dlls : $(RT_DLLS) 

$(RT_NATIVE_LIB)/random.so : $(SICSTUS_NATIVE_LIB)/random.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/random.so  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/timeout.so : $(SICSTUS_NATIVE_LIB)/timeout.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/timeout.so $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/bdb.so : $(SICSTUS_NATIVE_LIB)/bdb.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/bdb.so     $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/clpfd.so : $(SICSTUS_NATIVE_LIB)/clpfd.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/clpfd.so   $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/codesio.so : $(SICSTUS_NATIVE_LIB)/codesio.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/codesio.so $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/fastrw.so : $(SICSTUS_NATIVE_LIB)/fastrw.so  $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/fastrw.so  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/jasper.so : $(SICSTUS_NATIVE_LIB)/jasper.so $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/jasper.so  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/random.s.o : $(SICSTUS_NATIVE_LIB)/random.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/random.s.o  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/timeout.s.o : $(SICSTUS_NATIVE_LIB)/timeout.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/timeout.s.o $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/bdb.s.o : $(SICSTUS_NATIVE_LIB)/bdb.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/bdb.s.o     $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/clpfd.s.o : $(SICSTUS_NATIVE_LIB)/clpfd.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/clpfd.s.o   $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/codesio.s.o : $(SICSTUS_NATIVE_LIB)/codesio.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/codesio.s.o $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/fastrw.s.o : $(SICSTUS_NATIVE_LIB)/fastrw.s.o  $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/fastrw.s.o  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/jasper.s.o : $(SICSTUS_NATIVE_LIB)/jasper.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/jasper.s.o  $(RT_NATIVE_LIB)

$(RT_NATIVE_LIB)/jasper.s.o : $(SICSTUS_NATIVE_LIB)/jasper.s.o $(RT_NATIVE_LIB)
	$(CP) $(SICSTUS_NATIVE_LIB)/jasper.s.o  $(RT_NATIVE_LIB)


$(SKR_SRC_HOME)/c_nls_db.so : $(SKR_DB)/c_nls_db.so
	$(CP) $(SKR_DB)/c_nls_db.so $(SKR_SRC_HOME)/c_nls_db.so

$(SKR_SRC_HOME)/db_access.so : $(SKR_DB)/db_access.so
	$(CP) $(SKR_DB)/db_access.so $(SKR_SRC_HOME)/db_access.so

$(SKR_SRC_HOME)/debug.so : $(SKR_DEBUG)/debug.so
	$(CP) $(SKR_DEBUG)/debug.so $(SKR_SRC_HOME)/debug.so

$(SKR_SRC_HOME)/nls_signal.so : $(SKR_LIB)/nls_signal.so
	$(CP) $(SKR_LIB)/nls_signal.so $(SKR_SRC_HOME)/nls_signal.so 

$(SKR_SRC_HOME)/qp_morph.so : $(SKR_MORPH)/qp_morph.so
	$(CP) $(SKR_MORPH)/qp_morph.so 	$(SKR_SRC_HOME)/qp_morph.so 

$(SKR_SRC_HOME)/qp_lexicon.so  : $(SKR_LEXICON)/lexicon/qp_lexicon.so
	$(CP) $(SKR_LEXICON)/lexicon/qp_lexicon.so $(SKR_SRC_HOME)/qp_lexicon.so 

RT_DIRS=$(RT_BIN) $(RT_LIB) $(RT_NATIVE_LIB)

make_rtdirs: $(RT_DIRS)

$(RT_BIN) : 
	$(MKDIR) -p $(RT_BIN)

$(RT_LIB) :
	$(MKDIR) -p $(RT_LIB)

$(RT_NATIVE_LIB) :
	$(MKDIR) -p $(RT_NATIVE_LIB)

clean : clean_debug clean_lib clean_db clean_lexicon clean_morph \
clean_functions clean_query clean_miscutil clean_rtdir
	$(RM) -f $(BINEXEC) $(SAVED_STATE) $(MSBINEXEC) $(MSSAVED_STATE) *.so 

clean_debug :
	cd debug && $(MAKE) clean

clean_lib :
	cd lib && $(MAKE) clean

clean_db :
	cd db && $(MAKE) clean

clean_lexicon :
	cd lexicon/lexicon && $(MAKE) clean

clean_morph :
	cd lexicon/morph && $(MAKE) clean

clean_functions :
	cd lexicon/functions && $(MAKE) clean

clean_query :
	cd lexicon/query && $(MAKE) clean

clean_miscutil :
	cd lexicon/miscutil && $(MAKE) clean

clean_rtdir :
	$(RM) -rf $(RT_DIR) 

clean_mmserver :
	cd mmserver && $(MAKE) clean