
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

#/*==========================================================
#
#%SOURCE FILE
#	Makefile.lib
#
#%DESCRIPTION OF FILE
#	Make file
#
#%REVISED
#	30Aug2006 FML
#	based on Makefile.lexical and Makefile.nls_signal
#	
#%SCCS:	@(#)Makefile.lib	1.1 09/15/06
#
#==========================================================*/

include ../Makefile.include

O_SYSTEM = $(shell uname)

ifeq '$(O_SYSTEM)' "SunOS"
  LD = ld
  LDFLAGS=-G -shared
  LIBS =
endif

CFLAGS=${DEFAULT_CFLAGS}

SPLFR_BASIC_FLAGS = --verbose --keep
SPLFR_DYNAMIC_FLAGS = ${SPLFR_BASIC_FLAGS} --moveable
SPLFR_STATIC_FLAGS = ${SPLFR_BASIC_FLAGS} --static

SPLFR = splfr --conf CC=${CC} ${addprefix --cflag=, ${CFLAGS}}

# all:		nls_signal.o nls_signal.so nls_signal.a
# 
# nls_signal.a:	nls_signal.o 
# 		${AR} ${ARFLAGS} $@ $<
# 
# nls_signal.so:	nls_signal.o
# 		${LD} ${LDFLAGS} $< -o nls_signal.so ${LIBS}
# 
# nls_signal.o:	nls_signal.c 
# 		${CC} ${CFLAGS} -c $< -I${SPINCDIR}

all:		splfr_DYNAMIC splfr_STATIC

splfr_DYNAMIC:
		${SPLFR} ${SPLFR_DYNAMIC_FLAGS} nls_signal.pl nls_signal.c

splfr_STATIC:
		${SPLFR} ${SPLFR_STATIC_FLAGS} nls_signal.pl nls_signal.c

clean:
		-${RM} -f *.o *.a *.so *glue*
