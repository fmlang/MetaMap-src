/*==========================================================

%SOURCE FILE
	lvg.h

%DESCRIPTION OF FILE
	C include file.
	Header for lexical variant generation.

%REVISED
	23Dec94 divita -- Modified Version

==========================================================*/


/*----------------------------
Some platform depenant things
----------------------------*/

#define LVG_DEFAULT_ALLOCATION 256

#define FS  '|'
#define LVG_FIELD_SEPARATOR '|'
#define EOS '\0'		/* end-of-string */
#define NEWLINE '\n'
#define newLine "\n"
#define SPACE ' '
#define COMMA ','
#define QUOTE '\''
#define TAB '\t'
#define EOL '\n'

/* for handling a list of words */
typedef struct _wordList {
    char **words;		/* pointer to words */
    int	n;			/* number of words */
} WordList;

/* ptr to n'th word of a wordlist */
#define WLWN(wl,n)	    (*(((wl)->words)+(n)))

#define STRIP_WHITE_SPACE        1
#define KEEP_EVERYTHING          2
#define DONT_BREAK_ON_HYPHENS    3
#define ORIG_WORD_DEF            4
#define DONT_BREAK_ON_QUESTION   5
#define BREAK_ON_SPACE           6
#define RUSSELLS_RULES           7
#define REPLACE_PUNCT_WITH_SPACE 8

#define CASE_INSENSITIVE        1
#define CASE_SENSITIVE          2
