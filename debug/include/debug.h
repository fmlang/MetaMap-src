/*==========================================================

%SOURCE FILE
	debug.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	9May94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <malloc.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/*----------------------------
%CONSTANTS
----------------------------*/
#define DT 		      1
#define DF 		      2
#define MAX_MSG_SIZE 	  10000
#define D_TRUE  	      1
#define D_FALSE 	      0
#define MAX_FLAGS 	   4500

#define SUCCESS 	      1
#define MESSAGE 	      5
#define WARNING 	      4 
#define ERROR   	      2
#define FATAL   	      3

#define D_S_SUCCESS           1
#define D_E_ERROR             0
#define D_F_MALLOC            2      /* Call to Malloc Failed */
#define D_E_FILE              3      /* Error trying to open or close a file*/
#define D_E_FSEEK             4
#define D_E_FTELL             5
#define D_E_FPUTS             6
#define D_F_ERROR             7
#define D_S_NUMBER_EXISTS     8
#define D_S_EOF               9
#define D_S_NOT_FOUND        10
#define D_E_ERRLOG           11
#define D_E_DBGLOG           12
#define D_F_VALNODENEW       13
#define D_W_UNEXPECTED_VALUE 14
#define D_E_UNEXPECTED_VALUE 15
#define D_W_DESIGN_DEFECT    16
#define D_F_ASNLOAD          17
#define D_F_ASNIOOPEN        18 

#define D_NUMBER_OF_MESSAGES 19

#define EOS             '\0'
#define TAB             '\t'
#define SPACE           ' '
#define PIPE            '|'

#define BIG_LINE_SIZE   9000
#define MAXLINE         BIG_LINE_SIZE

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int dfname(char *function_name);
int denter(int flag);
int dexit(int flag);

/*----------------------------
%MACROS
----------------------------*/

#define DFNAME(x) \
          char *msg = NULL; \
          dfname(x);

#define DENTER(x) \
          denter(x); \
          msg = (char *) malloc ( BIG_LINE_SIZE );

#define DEXIT(x) \
        CHAR_FREE(msg); \
        dexit(x);

#define CHAR_FREE(x)  if (x != NULL ) free (x); x = (char *)NULL;  

/*----------------------------
%TYPEDEFS
----------------------------*/
typedef struct{
     int    key;
     int    message_type;
     char  *neumonic;
     char  *message;
   } Derror;


#ifdef NT
#define strcasecmp(x,y) stricmp(x,y)
#endif

/*end of constants ---------*/
