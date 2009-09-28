/*
   im.h - header file for inflectional morphology module.
*/


typedef unsigned int im_t;

#define IM_CAT_ADJ	((im_t)(0x1 << 0))
#define IM_CAT_ADV	((im_t)(0x1 << 1))
#define IM_CAT_NOUN	((im_t)(0x1 << 7))
#define IM_CAT_VERB	((im_t)(0x1 << 10))

/* other macros */
#define IM_COMMENT_CHAR		'#'

#define EOS			'\0'
#define NEWLINE			'\n'

