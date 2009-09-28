/* general utility functions
*/

#include <ctype.h>
#include <malloc.h>

#define MUTIL_DEFAULT_ALLOCATION 64
#define EOS '\0'

/* prototypes */
int *obsolete_get_fields(char *line, int sep, int *numF);
void *realloc_buf(void *buf, int unit, int size, int *alloc, int n, int incr);
void llip(register unsigned char *s);

/*  breaks a line into fields and returns an integer array
    such that a[2*n]=start offset and a[2*n+1] = length of
    the n'th field.
*/
int
*obsolete_get_fields(
		     char *line,		/* line to be broken up */
		     int sep,			/* separator char */
		     int *numF			/* number of fields */
		     )
{
    static int n_fieldBuf = 0;
    static int a_fieldBuf = 0;
    static int *fieldBuf = (int *)NULL;

    int start;
    int length;
    char *lp;

    for (start=0, n_fieldBuf=0, length=0, lp=line, *numF = 0; *lp != EOS; lp++) {
	if (*lp == sep)	{
	    if (n_fieldBuf >= a_fieldBuf)
		if ((fieldBuf = (int *) realloc_buf((void *)fieldBuf,
			sizeof(int), n_fieldBuf,
			&a_fieldBuf, (int)1, MUTIL_DEFAULT_ALLOCATION)) ==
			(int *)NULL)
		    return((int *)NULL);

	    *(fieldBuf + 2*n_fieldBuf) = start;
	    *(fieldBuf + 2*n_fieldBuf + 1) = length;
	    start = lp-line+1;
	    length = 0;
	    n_fieldBuf++;
	}
	else
	    length++;
    }
    
    if (n_fieldBuf >= a_fieldBuf)
	if ((fieldBuf = (int *) realloc_buf((void *)fieldBuf, sizeof(int),
			n_fieldBuf, &a_fieldBuf, (int)1,
			MUTIL_DEFAULT_ALLOCATION)) == (int *)NULL)
		    return((int *)NULL);

	    *(fieldBuf + 2*n_fieldBuf) = start;
	    *(fieldBuf + 2*n_fieldBuf + 1) = length;
    n_fieldBuf++;
    *numF = n_fieldBuf;
    return(fieldBuf);
}

/*  Increases the allocation of a buffer, buf using malloc/realloc.
    buf has alloc bytes currently allocated and size bytes are in use.
    n additional bytes are needed.  Allocation jumps in steps of incr.
*/
void *
realloc_buf(
	    void *buf,		/* pointer to buffer */
	    int unit,		/* size of one of what buf points to in bytes */
	    int size,		/* current size of buffer in units */
	    int *alloc,		/* current allocation of buffer in units */
	    int n,		/* units needed */
	    int incr		/* increment size in units */
	    )
{
    if ((size + n) > *alloc) {
	while ((size+n) > *alloc)
	    *alloc += incr;
	if (size == 0) {
	    if ((buf = (void *) malloc((unsigned)(unit*(*alloc)))) ==
		    (void *)NULL)
		return((void *)NULL);
	}
	else {
	    if ((buf = (void *) realloc((char *)buf,
		    (unsigned)(unit*(*alloc)))) == (void *)NULL)
		return((void *)NULL);
	}
    }
    return(buf);
}

/* lowercases uppercase letters of in s in place. */
void
llip(
     register unsigned char *s
     )
{
    for (; *s != EOS; s++)
	*s = (isupper(*s) ? tolower(*s) : *s);
    return;
}
