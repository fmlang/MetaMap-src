#!/bin/sh
# --------------------------
# use lf <year> <term>
# --------------------------

if [ $# -lt 2 ]; then 
  echo "usage: $0 year term"
  exit 1
fi 

TOP=$SKR_SRC_HOME/lexicon
DATABASE_HOME=${TOP}/data
export DATABASE_HOME

LM_TRANSLATED_RULES_FILE=${TOP}/lexicon/morph/lm_translated_rules
export LM_TRANSLATED_RULES_FILE

if [ $1 == "test" ]; then
  LEXICON_VERSION="Test"
  ./lf -l ../data/lexiconStatic${LEXICON_VERSION} -i ../data/LexiconStatic${LEXICON_VERSION}Ind -f $2
else
  LEXICON_VERSION="$1"
  DEFAULT_LEXICON_FILE=$DATABASE_HOME/singleWordLexiconStatic${LEXICON_VERSION}
  export DEFAULT_LEXICON_FILE
  DEFAULT_LEXICON_INDEX_FILE=${DEFAULT_LEXICON_FILE}Ind
  export DEFAULT_LEXICON_INDEX_FILE
  echo  ./lf -l ../data/lexiconStatic${LEXICON_VERSION} -i ../data/lexiconStatic${LEXICON_VERSION}Ind -f $2
  ./lf -l ../data/lexiconStatic${LEXICON_VERSION} -i ../data/lexiconStatic${LEXICON_VERSION}Ind -f $2
fi



