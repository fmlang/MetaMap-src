/*==========================================================

%SOURCE FILE
	linsert.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	2Aug94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <unistd.h>

/*----------------------------
%SCCS Version Flag
----------------------------*/
/*----------------------------
%STATIC FUNCTIONS
----------------------------*/

/*----------------------------
%External FUNCTIONS
----------------------------*/
extern int getopt();
extern int lex_insert();
extern int DPR(int flag, char *msg);
/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int main(
	 int	argc,
	 char *argv[] 
	 );
     
int usage();
/*--------PROTOTYPES--------*/



/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT118  118          /* DT for main() */
#define DF119  119          /* DF for main() */
#define DT124  124          /* DT for usage() */
#define DF125  125          /* DF for usage() */
/*-------DEBUG_FLAGS--------*/
/**/
/*==========================================================
%FUNCTION NAME
	main
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = main(arg);
%RETURNS
	?
%METHOD

%FILES
	Lexicon
	Lexicon Index
	Input file
	Temporary File
%TABLES
	None
%FLAGS  
	TRACE DT118
	FULL  DF119
%SYNTAX ARGS
==========================================================*/
int main(
	 int	argc,
	 char *argv[] 
	 )
     
{
  int   return_code = 1; 
  char  msg[1024];
  char lexicon_file_name[MAXLINE]; 
  char btree_index_files[MAXLINE];
  char recFile[MAXLINE];
  FILE *recFp = NULL;
  char cfg_name[40];
  char version[20];
  int done = D_FALSE;
  char c;
  char record[MAXLINE];

  debug_init(__FILE__);
  
  dfname("main");
  DENTER(DT118);
  

  strcpy(version,"2008");

  /* ----------------------------------------------------------- 
     Get Program Options
     -----------------------------------------------------------  */
  DPR(DF119,"Get Program Options");
  while ((c=getopt(argc, argv, "l:i:r:v:h:")) != (-1)) {
    switch (c) {
    case 'l': strcpy(lexicon_file_name, optarg);   break;
      
    case 'i': strcpy(btree_index_files, optarg); break;
      
    case 'r': strcpy(recFile, optarg);   break;

    case 'v': strcpy(version, optarg);   break;
      
    case 'h': usage(); goto bottom;      break; 
    default:
      usage();
      return_code = -1; 
      goto bottom;
    }
  }

  
  /* --------------------------
     read in the config file
     -------------------------- */
  // sprintf( cfg_name , "lexicon%s.cfg", version);

  // nls_cfg_read( cfg_name ); 

  
  if ( strlen( lexicon_file_name ) <= 0 ) {
    strcpy(lexicon_file_name, getenv("DEFAULT_LEXICON_FILE" )); 
  }
  
  if ( strlen( btree_index_files ) <= 0 ) {
    strcpy(btree_index_files, getenv("DEFAULT_LEXICON_INDEX_FILE" )); 
  } 
  
  if (recFile == (char *)NULL) {
    usage();
    return_code = -1; 
    goto bottom;
  }
  
  if ( strcmp( lexicon_file_name, recFile ) == 0 ) {
    sprintf(msg,"The record file and the lexicon file are the same");
    DPE(msg);
    goto bottom ;
  }

   /* ----------------------------------------------------------- 
        Open Input file for reading
      -----------------------------------------------------------  */
   DPR(DF119,"Open Index for reading and updating");
   if ((recFp = fopen(recFile, "r+")) == (FILE *)NULL) {
     fprintf(stderr, "Could not open the input file: %s\n", recFile);
     return_code = -1;
     goto bottom;
   }
   



  done = get_next_record_from_input( recFp, record );
  
  /* ----------------------------------------------------------- 
     Cycle through the records 
     ----------------------------------------------------------- */
  while ( done == D_FALSE ) 
  {
    
   if ( lex_insert( 
		   btree_index_files,   
		   lexicon_file_name,  
		   record              
		   ) != D_S_SUCCESS ) 
     
     {
       /* done = D_TRUE; */
       /* goto bottom;   */
       
     } 
    
    memset(record, 0, sizeof( char )*MAXLINE);
    
    done = get_next_record_from_input(recFp,  record );
  }
  
   /* ----------------------------------------------------------- 
        close all open files 
      ----------------------------------------------------------- */
  DPR(DF119,"Close all open files");
  fclose(recFp);


  close_lex_btree();

  close_lexicon_record_file();
  
  
 bottom:

   DEXIT(DT118);
   debug_term();

   return ( return_code );

} /*** End main */

/**/
/*==========================================================
%FUNCTION NAME
	usage
%SCOPE
	public
%PURPOSE
	Prints out the usage when the command line options dictate, or
	when no command line options are given, or when unrecognizable
	command line options are given.
%SYNTAX INCLUDES
	#include ""
%EXAMPLE CALL
	ret_val = usage();
%RETURNS
	NLS_S_SUCCESS
%METHOD
	Lots of fprintf's to the stderr
%FILES
	stderr
%TABLES
	None
%FLAGS  
	TRACE DT124
	FULL  DF125
%SYNTAX ARGS
==========================================================*/
int usage()

{
   int return_code = D_S_SUCCESS; 
   char default_lexicon[255];
   char default_index[255];

   DFNAME("usage");
   DENTER(DT124);

   /* nls_cfg_read( "lexicon2004.cfg" ); */
   strcpy(default_lexicon, getenv("DEFAULT_LEXICON_FILE" )); 
   strcpy(default_index, getenv("DEFAULT_LEXICON_INDEX_FILE" )); 
 
   fprintf(stderr,"Usage: linsert [-l <lexicon>] [-i <index>] -r <records>\n\n");
   fprintf(stderr,"The default lexicon is: %s\n", default_lexicon);
   fprintf(stderr,"The default index is: %s\n", default_index);
   fprintf(stderr,"The -r option is required.\n");

   DEXIT(DT124);
   return ( return_code );

} /*** End usage */
