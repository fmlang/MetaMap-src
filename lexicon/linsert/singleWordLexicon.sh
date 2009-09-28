#!/bin/sh

# --------------------------
# Single word lexicon
# --------------------------

if [ $# = 0 ]; then
    echo "usage: $0 <four-digit-year>"
    exit 0
fi

#TOP=/net/nls2/export/home/divita/nls/specialist/lexicon1999
#TOP=/nfsvol/nls/specialist/lexicon1999_Current
TOP=$SKR_SRC_HOME/lexicon

# path of nlp java classes
# CURRENT=/net/lexlx2/media/usbdisk/testbed/v2.4.B/nls/nlp
#CURRENT=/net/indlx1/export/home/wrogers/Projects/mmtxdev/rel-2-4-C-git/nls/nlp
CURRENT=/nfsvol/nlsaux15/mmtxdev/nls_projects/nls/nlp

# Berkeley DB library path for java
#DB_TOP=/nfsvol/nls/tools/berkeley_db/db_3
DB_TOP=/nfsvol/nls/tools/berkeley_db/Linux-i686/db-4.1.25
# should be: DB_TOP=$BERKELEY
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$DB_TOP/lib
DB_CLASSES=$DB_TOP/classes
export LD_LIBRARY_PATH

# ---------------------------
# index the files 
# ---------------------------
LEXICON_VERSION="$1"
DEFAULT_LEXICON_FILE=$DATABASE_HOME/lexiconStatic${LEXICON_VERSION}
export DEFAULT_LEXICON_FILE
DEFAULT_LEXICON_INDEX_FILE=${DEFAULT_LEXICON_FILE}Ind
export DEFAULT_LEXICON_INDEX_FILE

../linsert/linsert -v ${LEXICON_VERSION} -l ../data/lexicon${LEXICON_VERSION} -i ../data/singleWordLexiconStatic${LEXICON_VERSION}Ind -r ../data/LEXICON${LEXICON_VERSION} >./buildLexicon.msg 2>&1

echo "done" >>./buildLexicon${LEXICON_VERSION}.msg
# ---------------------------
# read in the indexed files, 
# find out the orphan terms
# and tag them
# ---------------------------

echo "About to alter the infl Table and create a new infl table"
java -mx900m -ms300m -cp "$DB_CLASSES:$CURRENT/lib/nlpProject.jar:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon $* >orphanWords 2>./inflMsgs.txt 
echo "done" >>./inflMsgs.txt

echo "About to alter the eui Table and create a new eui table"
java -mx900m -ms300m -cp "$DB_CLASSES:$CURRENT/lib/nlpProject.jar:$TOP/classes:$TOP/config:" gov.nih.nlm.nls.singleWordLexicon.SingleWordLexicon --euiTable $* >orphanWords2 2>./euiMsgs.txt 
echo "done" >>./euiMsgs.txt 

# ----------------------------------------------
# The programs require that the endings of the data files
# end with IndByEui.dbx and IndByInfl.dbx.
#
# Rename the files to make them so:
# ----------------------------------------------
# mv ../data/singleWordLexiconStatic${LEXICON_VERSION} ../data/lexiconStatic${LEXICON_VERSION}
mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByEui.dbx ../data/lexiconStatic${LEXICON_VERSION}IndByEui.dbx
mv ../data/singleWordLexiconStatic${LEXICON_VERSION}IndByInfl.dbx ../data/lexiconStatic${LEXICON_VERSION}IndByInfl.dbx
