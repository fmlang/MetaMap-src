/*==========================================================

%SOURCE FILE
	get_value.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	17Oct94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include "debug.h"

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
/*end of static functions --*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int get_value_from_field(int field_position, char *value, char   *umls_record);
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT410  410          /* DT for get_value_from_field() */
#define DF411  411          /* DF for get_value_from_field() */

/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_value_from_field
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_value_from_field(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT410
	FULL  DF411
%HEADER END
==========================================================*/
int get_value_from_field(
			  int field_position, /* Input                */
			  char   *value,       /* Output -space should */
                                               /* already be malloced  */
			  char   *umls_record  /* Input                */
			  )

{
   int return_code = D_S_SUCCESS;
   int current_field  = 1;
   int i,char_len,j=0;
   char   tmpvalue[MAXLINE];

   
   DFNAME("get_value_from_field");
   DENTER(DT410);

   
  /* ----------------------------------------------
     Some yutes, like me have taken files off the
     cd, and not stripped off the cr's on them 
     this bit of code is an attempt to stip that off
   if (( umls_record != NULL ) && 
      ( strlen(umls_record) > 2 ) &&
      ( umls_record[ strlen(umls_record) -2] < ' ')
   {
     char_len = strlen(umls_record) -2 ;
   }
   else
     ---------------------------------------------- */
   {
     char_len = strlen(umls_record);
   }
                                  
   
   strcpy(tmpvalue,"");
   strcpy(value,"");


   for (i =0;i < char_len; i++ )
   {
      if ( umls_record[i] == '|' )
      {
	 current_field ++;
	 continue;
      }


      if ( current_field > field_position )
	 break;

      if (field_position == current_field )
      {
	if ( umls_record[i] != '\n' )
	{
	  tmpvalue[j] = umls_record[i];
	  j++;
	}
      }

   }

   if (j == 0 )
   {

     value[0] = EOS;
   }
   else
   {
     tmpvalue[j]=EOS;
     strcpy(value,tmpvalue);
   }
   

   DEXIT(DT410);
   return ( return_code );

} /*** End get_value_from_field */
