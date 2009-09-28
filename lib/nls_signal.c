/*
% File:	    nls_signal.c
% Authors:  Lan
% Purpose:  Provides signal handling
*/

#include <signal.h>
#include "sicstus/sicstus.h"
#include "nls_signal_glue.h" 

/* External interface */

/* long C_establish_signal_handling(void); */
void abort_handler(int arg);


/*****************************************************************************/
/* Establish signal handlers.
*/

long C_establish_signal_handling(void)
{

	signal(SIGTERM, abort_handler);
	signal(SIGPIPE, abort_handler);
	return 1;

} /* C_establish_signal_handling */

void abort_handler(int arg)
{
	/* SP_raise_fault("ABORT"); */
	abort();

} /* abort_handler */
