/*
% File:	    btreeQlib.c
% Module:   Berkeley DB
% Author:   Jim
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>
#include "embed.h"

#define TRUE  1
#define FALSE 0


int NUM_TABLES;
int first_flag = TRUE;
struct config_struct **config_info;

void btree_query(char *query, char ***q_results, int *numberOfResults, int pos);
void init_dbs(char *db_path);
void GetConfigInfo(char *database_home);
void open_dbs(int dbptr);
void destroy_dbs(void);

static char *database_home;
static DB **db;

/**************************************************************************/

void init_dbs(char *db_path)
{
   if(strlen(db_path) > 1)
   {
     database_home = (char *)malloc((size_t)strlen(db_path) + 1);
     strcpy(database_home, db_path);
   } /* fi */
   else
   {
     database_home = (char *)malloc((size_t)strlen((char *)getenv("DB_HOME")) + 1);
     strcpy(database_home, (char *)getenv("DB_HOME"));
   } /* else */

   GetConfigInfo(database_home);
   db = (DB **)malloc(sizeof(DB *) * NUM_TABLES);
} /* init_dbs */

/**************************************************************************/

void GetConfigInfo(char *loc_database_home)
{
   FILE *fp;
   char line[MAXLINE];
   int num_fields, i;
   int pos;
   char config_file[MAXLINE];
   char tmp[MAXLINE], tmp2[MAXLINE], tmp3[5];

   /* sprintf(config_file, "%s/config\0", loc_database_home); */
   sprintf(config_file, "%s/config", loc_database_home);

   if ((fp = fopen(config_file,"r")) == NULL)
   {
      fprintf(stderr, "fopen: %s\n", strerror(errno));
      exit(1);
   } /* fi */

   /* Grab the Number of Tables from the FIRST line */

   if(fgets(line, MAXLINE, fp) != NULL)
   {
       sscanf(line, "NUM_TABLES: %d", &NUM_TABLES);
       config_info = (struct config_struct **)
                       malloc(sizeof(struct config_struct *) * NUM_TABLES);
   } /* fi */
   else
   {
      fprintf(stderr, "fgets: %s\n", strerror(errno));
      exit(2);
   } /* else */

   pos = 0;
   while(fgets(line, MAXLINE, fp) != NULL)
   {
      if(line[0] != '#')    /* Skip over comment lines */
      {
        config_info[pos] = 
            (struct config_struct *)malloc(sizeof(struct config_struct));

        config_info[pos]->opened = FALSE;

        /* Grab the easy information */

        sscanf(line, "%[^|]|%[^|]|%d|%[^\n]", config_info[pos]->file_name, 
               config_info[pos]->table, &num_fields, tmp);

        config_info[pos]->num_fields = num_fields;

        /* Grab the field names */

        for(i = 0; i < num_fields; i++)
        {
           sscanf(tmp, "%[^|]|%[^\n]", config_info[pos]->fields[i], tmp2);
           strcpy(tmp, tmp2);
        } /* for */

        /* Now grab the Type information */

        for(i = 0; i < num_fields; i++)
        {
           sscanf(tmp, "%[^|]|%[^\n]", tmp3, tmp2);

           if(strcmp(tmp3, "TXT") == 0)
             config_info[pos]->field_types[i] = TXT_TYPE;
           else
             config_info[pos]->field_types[i] = INT_TYPE;

           strcpy(tmp, tmp2);
        } /* for */
        pos++;
      } /* fi */
   } /* while */

   fclose(fp);
} /* GetConfigInfo */

/**************************************************************************/
/* Open all of the databases in anticipation of multiple queries 
       if dbptr = -1.  If dbptr >=0 then only the specific db is opened. */

void open_dbs(int dbptr)
{
   char database[MAXLINE + 1];
   int i, high, low, errno;

   if(dbptr < 0)
   {
      high = NUM_TABLES;
      low = 0;
   } /* fi */
   else
   {
     high = dbptr + 1;
     low = dbptr;
   } /* else */

   for(i = low; i < high; i++)
   {
       /* sprintf(database, "%s/%s\0", database_home, config_info[i]->table); */
       sprintf(database, "%s/%s", database_home, config_info[i]->table);
       if((errno = db_create(&db[i], NULL, 0)) != 0)
         fprintf(stderr, "%s: db_create: %s\n", __FILE__, db_strerror(errno));
       else
       {
          db[i]->set_errfile(db[i], stderr);
          db[i]->set_errpfx(db[i], __FILE__);
          db[i]->set_pagesize(db[i], 16 * 1024);
          db[i]->set_cachesize(db[i], 0, 64 * 1024, 0);
          db[i]->set_flags(db[i], DB_DUP);

#ifdef BDB_3_0_55
          if((errno = db[i]->open(db[i], database, NULL, DB_BTREE, 
                (DB_RDONLY), 0644)) != 0)
#else
	  if((errno = db[i]->open(db[i], NULL, database, NULL, DB_BTREE, 
                (DB_RDONLY), 0644)) != 0)
#endif
             db[i]->err(db[i], errno, "open:%s", database);
       } /* else */
   } /* for */
} /* open_dbs */

/**************************************************************************/

void destroy_dbs(void)
{
  int i;
 
  for(i = 0; i < NUM_TABLES; i++)
  {
     if(config_info[i]->opened)
        db[i]->close(db[i],0);

     free((char *)config_info[i]);
  } /* for */

  /* Free up the config structure & b, and db variables */

  free((char*) config_info);
  free((char*) db);
  free(database_home);
} /* destroy_dbs */

/**************************************************************************/

void btree_query(char *query,             /* Input  */
		 char ***q_results,       /* Output */
                 int   *numberOfResults,  /* Output */
                 int  pos                 /* DB ptr */ )
{
  int i = 0;
  DBT data, key;
  char **results;
  int record_found = FALSE;
  int status = 0;
  int errno;
  DBC *dbcp = NULL;
  int upper_bound = 100;
  
  results =(char **)malloc( sizeof( char *) * upper_bound );
  memset(&key, 0, sizeof(DBT));
  memset(&data, 0, sizeof(DBT));

  key.data  = query ;
  key.size  = sizeof(char) * strlen(query) + 1;
      
  /* Acquire a cursor for the database. */

  if ((errno = db[pos]->cursor(db[pos], NULL, &dbcp, 0)) != 0) 
  {
    fprintf(stderr, "%s: cursor: %s\n", __FILE__, strerror(errno));
    return;
  }
  
  status = dbcp->c_get(dbcp, &key, &data, DB_SET);
  i = 0;
  while(( status == 0 ) && ( data.data != NULL ) &&
	(strcmp( (char *)key.data, query) == 0 ))
  {
      if(i >= upper_bound)
      {
         upper_bound += 100;
         results = (char **)realloc(results, sizeof( char *) * upper_bound);
      } /* fi */

      record_found = TRUE;
      results[i] = (char *)malloc((size_t)(strlen((char *) data.data) + 1));
      strcpy(results[i], (char *)data.data);
      i++;
      memset(&data, 0, sizeof(DBT));
      memset(&key, 0, sizeof(DBT));
     
      status = dbcp->c_get(dbcp, &key, &data, DB_NEXT); 
  } /* while */

  dbcp->c_close(dbcp); 
  
  if(i <= 0)
  {
     free((char *)results);
     results = NULL;
  } /* fi */

  else
    results = (char **)realloc(results, sizeof( char *) * i);

  if (record_found == TRUE)
  {
    *numberOfResults = i;
    *q_results = results;
  } /* fi */
} /*** End btree_query */
