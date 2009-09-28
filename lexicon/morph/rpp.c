/* rpp.c - rule pre-processor for handling #includes
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "im.h"

static void output( FILE *fp);

int main(void)
{
    output(stdin);
    return(0);
}

/* recursively outputs any included files */
static void
output(
       FILE *fp
       )
{
    char line[512];

    while (fgets(line, sizeof(line), fp) != (char *)NULL)
    {
	fputs(line, stdout);
	if (line[0] == IM_COMMENT_CHAR)
	{
		if ((strncmp(line, "#include", 8) == 0) && (isspace((int)line[8])))
	    {
		int i;

		i = 8;
		while (isspace((int)line[i]))  i++;
		if (line[i] == '"')
		{
		    FILE *incFp;
		    char filename[64];
		    int j;

		    for (++i, j=0; line[i] != EOS && line[i] != '"' && j<64; i++, j++)
			filename[j] = line[i];
		    filename[j] = EOS;
		    if ((incFp = fopen(filename, "r")) == (FILE *)NULL)
		    {
			fprintf(stderr, "Warning: include file: \"%s\" does not exist.\n", filename);
		    }
		    else
		    {
			output(incFp);
			printf("#endinclude\n");
		    }
		}
	    }
	}
    }
    (void) fclose(fp);
    return;
}
