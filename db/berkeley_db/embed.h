/*
% File:	    embed.h
% Module:   Berkeley DB
% Author:   Jim
*/

#define MAXLINE  65536
#define MAXCOLS     10
#define TRUE         1
#define FALSE        0
#define INT_TYPE   350
#define TXT_TYPE   375
 
struct query_struct
{
     int num_fields;
     char *fields[MAXCOLS];
     char *table;
     char *query;
     char *where2;
     char *query2;
};
 
struct config_struct
{
     char file_name[31];        /* formerly MAXLINE */
     char table[31];            /* formerly MAXLINE */
     int opened;		/* Boolean TRUE/FALSE if opened already */
     int num_fields;
     char fields[MAXCOLS][21];  /* formerly 50 */
     int field_types[MAXCOLS];
};
 
struct res_rows_struct
{
    int num_cols;
    int col_type[MAXCOLS];
    int int_result[MAXCOLS];
    char *str_result[MAXCOLS];
};
 
struct results_struct
{
    int num_rows;
    int config_ptr;
    struct res_rows_struct **rows;
};

