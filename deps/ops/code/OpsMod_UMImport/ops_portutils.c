/******************************COPYRIGHT*******************************/
/* (C) Crown copyright Met Office. All rights reserved.               */
/* For further details please refer to the file COPYRIGHT.txt         */
/* which you should have received as part of this distribution.       */
/* *****************************COPYRIGHT******************************/

#include <time.h>
#include <memory.h>
#include <errno.h>
#include <utime.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <stdbool.h>
#include <inttypes.h>

#include "ops_c_fort2c_2a.h"

/* Required for VAR:         */
/* DEPENDS ON: ops_ereport_mod.o */

#if defined(C_LOW)

#define fort_get_env ops_fort_get_env

#elif defined(C_LOW_U)

#define fort_get_env ops_fort_get_env_

#else

#define fort_get_env OPS_FORT_GET_ENV

#endif

/* Prototypes */
/* OPS does not use this, but SURF does. */
#if defined(C_LOW)
void ops_date_time (integer *year, integer *month, integer *day, integer *hour,
                    integer *minute, integer *second);
#elif defined(C_LOW_U)
void ops_date_time_ (integer *year, integer *month, integer *day, integer *hour,
                     integer *minute, integer *second);
#else
void OPS_DATE_TIME (integer *year, integer *month, integer *day, integer *hour,
                    integer *minute, integer *second);
#endif

int64_t ops_c2f_strlen(void *str);

void ops_c_get_env(void *env_var_name,int64_t *ev_len,void *ev_contents,
                   int64_t *cont_len,int *ret_code);


void fort_get_env(void *env_var_name,integer *ev_len,void *ev_contents,
                       integer *cont_len,integer *ret_code)
{
  int64_t l_ev_len,l_cont_len;
  int l_ret_code;

  int i;

  char *c_env_var_name;

  l_ev_len=*ev_len;
  l_cont_len=*cont_len;

  c_env_var_name = calloc(*ev_len + 1,1);
  strncpy(c_env_var_name,env_var_name,*ev_len);
  c_env_var_name[*ev_len]='\0';
  sscanf(c_env_var_name,"%s",c_env_var_name);

  ops_c_get_env(c_env_var_name, &l_ev_len, ev_contents, &l_cont_len, &l_ret_code);

  *ret_code=l_ret_code;

  for (i=strlen(ev_contents); i<*cont_len; i++){
   ((char *)ev_contents)[i]=' ';
  }

  free( c_env_var_name );
}

/****************************************************************
 * Miscellaneous wrappers to c and other utility                 *
 ****************************************************************/

void
#if defined(C_LOW)
ops_date_time
#elif defined(C_LOW_U)
ops_date_time_
#else
OPS_DATE_TIME
#endif
(integer *year,   /* year                  */
 integer *month,  /* month   Current date  */
 integer *day,    /* day      and time     */
 integer *hour,   /* hour                  */
 integer *minute, /* minute                */
 integer *second) /* second                */
{
  char s[5];
  time_t a;

  a = time(NULL);

  strftime(s,5,"%Y",localtime(&a));
  *year=atoi(s);
  strftime(s,5,"%m",localtime(&a));
  *month=atoi(s);
  strftime(s,5,"%d",localtime(&a));
  *day=atoi(s);
  strftime(s,5,"%H",localtime(&a));
  *hour=atoi(s);
  strftime(s,5,"%M",localtime(&a));
  *minute=atoi(s);
  strftime(s,5,"%S",localtime(&a));
  *second=atoi(s);
}

void ops_c_get_env
(void *env_var_name, /* Name of environment variable (void * to char[])     */
int64_t *ev_len,     /* length of name                                      */
void *ev_contents,   /* contents of environment variable (void * to char[]) */
int64_t *cont_len,   /* length of contents                                  */
int *ret_code)       /* return code: 0=OK  -1=problems                      */
{
  char message[MAX_OUTSTR];

  char *value;
  size_t len;
 // size_t i;
(void)(ev_len);
  sscanf(env_var_name,"%s",(char *)env_var_name);

  value=getenv(env_var_name);
  if (value==NULL){
    *ret_code=-1;
    return;}
  else{
    *ret_code=0;}

  len=strlen(value);
  if (len > (size_t)(*cont_len)){

    snprintf(message,MAX_OUTSTR,
             "FORT_GET_ENV: Value too long for Allocated Storage");
    snprintf(message,MAX_OUTSTR,
             "FORT_GET_ENV: Environment Variable %s",
             (char *)env_var_name);
    snprintf(message,MAX_OUTSTR,
             "FORT_GET_ENV: Value %s", value);
    ereport("fort_get_env","Value too long for Allocated Storage");
  }
  sprintf(ev_contents,"%s",value);
}

int64_t ops_c2f_strlen(void *str)
{
  return (int64_t)strlen((char *)str);
}
