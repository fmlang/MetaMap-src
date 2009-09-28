/* loc.c - input file locator stack functions
*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>

int push_loc(
	     char *includeFile,
	     int *linenoP,
	     char **curFileP
		);
int pop_loc(
	    int *curLineNumP,
	    char **curFileNameP
	    );

int int_loc(
	int *linenoP,
	char **curFileP
	);

typedef struct _locNode {
    int lineno;
    char *filename;
    struct _locNode *next;
} LocNode;

typedef LocNode *LocStack;
static LocNode *locStack = (LocNode *) NULL;

/* pushes old location on stack */
int
push_loc(
	 char *inputLine,
	 int *curLineNumP,
	 char **curFileNameP
	 )
{
    LocNode *loc;
    char *incFile;
    char *sp, *ep;

    if ((loc = (LocNode *) malloc((unsigned)(sizeof(LocNode)))) == (LocNode *)NULL)
	return(0);
    loc->lineno = *curLineNumP;
    loc->filename = *curFileNameP;
    loc->next = locStack;
    locStack = loc;

    sp = &inputLine[10];
    if ((ep = strchr(sp, '"')) == (char *)NULL)
	return(0);
    if ((incFile = malloc((unsigned)(ep-sp+1))) == (char *)NULL)
	return(0);
    strncpy(incFile, sp, (size_t)(ep-sp));
    *(incFile + (int)(ep-sp)) = '\0';
    *curFileNameP = incFile;
    *curLineNumP = 0;
    return(1);
}

/* pops stack */
int
pop_loc(
	int *curLineNumP,
	char **curFileNameP
	)
{
    LocNode *node = locStack;

    if (node == (LocNode *)NULL)
	return(0);
    locStack = node->next;

    if (*curFileNameP != (char *)NULL)
	(void) free(*curFileNameP);

    *curLineNumP = node->lineno;
    *curFileNameP = node->filename;
    (void) free((char *)node);
    return(1);
}
