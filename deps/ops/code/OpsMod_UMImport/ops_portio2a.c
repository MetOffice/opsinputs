/******************************COPYRIGHT*******************************/
/* (C) Crown copyright Met Office. All rights reserved.               */
/* *****************************COPYRIGHT******************************/

/* C language routines for portable version of UM */
/* depends on: c_shum_byteswap.o */

/* Standard header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "c_shum_byteswap.h"

/* Header files for Fortran to C interface */
#include "ops_c_fort2c_2a.h"

/* Header files for I/O */
#include "ops_c_portio.h"

#if defined(C_LOW)

#define ops_buffin32_single ops_buffin32
#define ops_buffin8_single ops_buffin8
#define ops_buffin64_single ops_buffin64
#define ops_buffout32_single ops_buffo32
#define ops_buffout64_single ops_buffout64
#define ops_buffout_single ops_buffout
#define ops_buffin_single ops_buffin
#define ops_setpos8_single ops_setpos8
#define ops_file_open ops_file_open
#define ops_file_close ops_file_close

#elif defined(C_LOW_U)

#define ops_buffin32_single ops_buffin32_
#define ops_buffin8_single ops_buffin8_
#define ops_buffin64_single ops_buffin64_
#define ops_buffout32_single ops_buffo32_
#define ops_buffout64_single ops_buffout64_
#define ops_buffout_single ops_buffout_
#define ops_buffin_single ops_buffin_
#define ops_setpos8_single ops_setpos8_
#define ops_file_open ops_file_open_
#define ops_file_close ops_file_close_


#else

#define ops_buffin32_single OPS_BUFFIN32
#define ops_buffin8_single OPS_BUFFIN8
#define ops_buffin64_single OPS_BUFFIN64
#define ops_buffout32_single OPS_BUFFO32
#define ops_buffout64_single OPS_BUFFOUT64
#define ops_buffout_single OPS_BUFFOUT
#define ops_buffin_single OPS_BUFFIN
#define ops_setpos8_single OPS_SETPOS8
#define ops_file_open OPS_FILE_OPEN
#define ops_file_close OPS_FILE_CLOSE


#endif

/* Prototypes for internally called service functions */

void ops_output_buffer( integer *, um_data_t [], integer *, integer *, integer *,
                        integer *);
void ops_sync_to_cache( integer *);

void ops_buffout64_single(integer *unit,void *array,integer *maxlen,
                          integer *length,real *status);

void ops_setpos8_single(integer *unit,  integer *byte_address);

void ops_buffout32_single(integer *unit,  void *array, integer *maxlen,
                          integer *length, real *status);

void ops_buffin32_single(integer *unit, void *array, integer *maxlen,
                         integer *length, real *status);

#if defined(C_LOW)
void ops_setpos(integer *unit,integer *word_address,integer *err);
#elif defined(C_LOW_U)
void ops_setpos_(integer *unit,integer *word_address,integer *err);
#else
void OPS_SETPOS(integer *unit,integer *word_address,integer *err);
#endif

void ops_buffin64_single (integer *unit, void *array, integer *maxlen,
                          integer *length, real *status);

#if defined(C_LOW)
void ops_buffou8(integer *unit, char array[], integer *maxlen,integer *length,
                 real *status);
#elif defined(C_LOW_U)
void ops_buffou8_(integer *unit, char array[], integer *maxlen,integer *length,
                  real *status);
#else
void OPS_BUFFOU8(integer *unit, char array[], integer *maxlen,integer *length,
                 real *status);
#endif


#if defined(C_LOW)
void ops_setpos32(integer *unit, integer *word32_address, integer *err);
#elif defined(C_LOW_U)
void ops_setpos32_(integer *unit, integer *word32_address, integer *err);
#else
void OPS_SETPOS32(integer *unit, integer *word32_address, integer *err);
#endif

void ops_close_single(int64_t *unit, void *file_name, int64_t *char_len,
                      int64_t *environ_var_flag, int64_t *delete, int64_t *err);

#if defined(C_LOW)
void ops_buffin16(integer *unit, short array[], integer *maxlen, integer *length,
                  real *status);
#elif defined(C_LOW_U)
void ops_buffin16_(integer *unit, short array[], integer *maxlen, integer *length,
                   real *status);
#else
void OPS_BUFFIN16(integer *unit, short array[], integer *maxlen, integer *length,
                  real *status);
#endif

void ops_buffin8_single(integer *unit, void *array, integer *maxlen,
                        integer *length, real *status);

void ops_open_single(int64_t *unit, void *file_name,  int64_t *char_len,
                     int64_t *intent, int64_t *environ_var_flag, int64_t *err);

static char message[ MAX_OUTSTR ];

/* Global variables used for I/O  */

FILE *ops_pf[MAX_UNITS]=
                     {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                      NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                     };
/* Record of the previous file operation for flushing purposes */
enum fileop { UM_READ, UM_WRITE, UM_UNSET } prev_fileop [MAX_UNITS] ;

/* The file position in bytes */
#if defined(LFS)
  static off_t io_position[MAX_UNITS];
#else
  static long io_position[MAX_UNITS];
#endif


/* Define the buffer size - not in def as we want the routine
   to change it to be open */
size_t ops_buffer_size=512*1024;

static integer readonly = 0;

int ops_open_flag[MAX_UNITS] =
     {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

/* Unit properies table - one word per unit, one bit per
property at present */

/* integer file_properties;
moved to mppio.F90 as attributes array top support remote IO
*/

integer *the_unit;

/* Routine to wrap ops_buffout32/64 to be backwards compatible */
void ops_buffout_single(integer *unit, void *array, integer *maxlen,
                        integer *length, real *status)
{
#if defined(FRL8)
  ops_buffout64_single(unit, array, maxlen, length, status);
#else
  ops_buffout32_single(unit, array, maxlen, length, status);
#endif
}

/* Routine to wrap ops_buffin32/64 to be backwards compatible */
void ops_buffin_single(integer *unit, void *array, integer *maxlen,
                       integer *length, real *status)
{
#if defined(FRL8)
  ops_buffin64_single(unit, array, maxlen, length, status);
#else
  ops_buffin32_single(unit, array, maxlen, length, status);
#endif
}

void ops_file_open
(
  integer *unit, void *file_name, integer *char_len,
  integer *intent, integer *environ_var_flag, integer *err
)
{
  int64_t l_unit, l_char_len, l_intent, l_environ_var_flag, l_err;

  l_unit=*unit;
  l_char_len=*char_len;
  l_intent=*intent;
  l_environ_var_flag=*environ_var_flag;

  ops_open_single(&l_unit, file_name, &l_char_len, &l_intent, &l_environ_var_flag,
                  &l_err);

  *err=l_err;
}

void ops_file_close
(
  integer *unit, void *file_name, integer *char_len,
  integer *environ_var_flag, integer *delete, integer *err
)
{
  int64_t l_unit, l_char_len, l_delete, l_environ_var_flag, l_err;

  l_unit=*unit;
  l_char_len=*char_len;
  l_delete=*delete;
  l_environ_var_flag=*environ_var_flag;

  ops_close_single(&l_unit, file_name, &l_char_len, &l_environ_var_flag, &l_delete,
                   &l_err);

  *err=l_err;
}

/* -------------------------------------------------------------------------- */

void ops_buffin64_single
(
integer *unit,    /* Fortran unit                                          */
void *array,      /* Array into which data is read (void * to um_data_t[]) */
integer *maxlen,  /* Number of real numbers to be read                     */
integer *length,  /* Number of real numbers actually read                  */
real *status      /* Return code                                           */
)
{
int k;
char message[200];
the_unit=unit;

if(ops_open_flag[*unit]== 0){

/* Only need to flush if previous file operation was not a READ */

    if(prev_fileop[*unit] == UM_WRITE)
    {
      ops_sync_to_cache(unit);
    }
    prev_fileop[*unit] = UM_READ;

    *length = (integer)(fread(array,WORD64BYTES,(size_t)(*maxlen),
                              ops_pf[(size_t)(*unit)]));

#if defined(LITTLE_END)
c_shum_byteswap(array, *maxlen, WORD64BYTES, message, 200);
#endif

*status=-1.0;
k=feof(ops_pf[*unit]);

    if(k != 0)
    {
      perror("\nOPS_BUFFIN: Read Failed");
      snprintf(message, MAX_OUTSTR ,
       "OPS_BUFFIN: C I/O Error - Return code = %d", k);
      *status=0.0;
    }
        k=ferror(ops_pf[*unit]);
    if(k != 0)
    {
      perror("\nOPS_BUFFIN: Read Failed");
      snprintf(message, MAX_OUTSTR ,
       "OPS_BUFFIN: C I/O Error - Return code = %d", k);
      *status=1.0;
    }
   }
   else
        *status=3.0;

    io_position[(size_t)(*unit)]=
           io_position[(size_t)(*unit)]+(*length * WORD64BYTES);
}

void ops_buffout64_single
(integer *unit,    /* Fortran unit                            */
void *array, /* Array from which data is written        */
integer *maxlen,   /* Number of real numbers to be written    */
integer *length,   /* Number of real numbers actually written */
real *status)      /* Return code                             */
{
  integer elsize;
  char message[200];
  the_unit=unit;
  /* Default size in the size of buffer element */
  elsize = WORD64BYTES;

  integer tmpstatus;

  if(ops_open_flag[*unit]== 0){

/* Only need to flush if previous file operation was not a WRITE */
    if(prev_fileop[*unit] == UM_READ)
    {
      ops_sync_to_cache(unit);
    }
    prev_fileop[*unit] = UM_WRITE;


#if defined(LITTLE_END)
    c_shum_byteswap(array, *maxlen, WORD64BYTES, message, 200);
#endif

    ops_output_buffer(unit, array, maxlen, length, &elsize, &tmpstatus);
    *status=(real)tmpstatus;

#if defined(LITTLE_END)
    c_shum_byteswap(array, *maxlen, WORD64BYTES, message, 200);
#endif
  }
  else
    *status=3.0;
}

void ops_output_buffer
(integer *unit,    /* Fortran unit                            */
um_data_t array[], /* Array from which data is written        */
integer *maxlen,   /* Number of real numbers to be written    */
integer *length,   /* Number of real numbers actually written */
integer *elsize,   /* Size of elements to write               */
integer *status)      /* Return code                             */
{
int k;

the_unit=unit;

    *length = (integer)fwrite(array,(size_t)(*elsize),
                              (size_t)(*maxlen),ops_pf[(size_t)(*unit)]);

    *status=-1;
    k=feof(ops_pf[*unit]);

    if(k != 0)
    {
      perror("\nOPS_BUFFOUT: Write Failed");
      snprintf(message, MAX_OUTSTR ,
       "OPS_BUFFOUT: C I/O Error - Return code = %d", k);
      *status=0;
    }
        k=ferror(ops_pf[*unit]);
    if(k != 0)
    {
      perror("\nOPS_BUFFOUT: Write Failed");
      snprintf(message, MAX_OUTSTR ,
       "OPS_BUFFOUT: C I/O Error - Return code = %d", k);
      *status=1;
    }

    io_position[*unit]=io_position[*unit]+((*length) * (*elsize));
}


void
#if defined(C_LOW)
ops_setpos
#elif defined(C_LOW_U)
ops_setpos_
#else
OPS_SETPOS
#endif
(integer *unit,        /* Fortran unit                                     */
integer *word_address, /* Number of default fortran int/real into file     */
integer *err)          /* Error checking, err = 0 no errors,err = 1 errors */
{
  int k;
#if defined(LFS)
  off_t byte_address;
#else
  long byte_address;
#endif

  the_unit=unit;
  *err = 0;       /* default a successful return */

    /* Only do the seek if we need to */
    if (io_position[*unit] != *word_address*(integer)(sizeof(um_data_t))) {
#if defined(LFS)
      byte_address=(off_t)(*word_address)*(off_t)sizeof(um_data_t);
      k = fseeko(ops_pf[*unit],byte_address,SEEK_SET);
#else /* LFS */
      byte_address=(long)(*word_address)*(long)sizeof(um_data_t);
      k = fseek(ops_pf[*unit],byte_address,SEEK_SET);
#endif /* LFS */
      if(k!=0){
        perror("\nOPS_SETPOS: Seek Failed");
        fprintf(stderr,
         "OPS_SETPOS: Unit %d to Word Address %d Failed with Error Code %d",
         (int) *unit, (int) *word_address, k);
         *err = 1;
         ereport("ops_portio2a:ops_setpos","Failed in fseek[o]");
      }
    io_position[*unit]=byte_address;
    }

}

void ops_open_single
(
  int64_t *unit,             /* Fortran unit                                  */
  void    *file_name,        /* File name or environ. var. (void * to char[]) */
  int64_t *char_len,         /* No of chars in file name                      */
  int64_t *intent,           /* =0 read only,!=0 read and write               */
  int64_t *environ_var_flag, /* =0 file name in environment var,              */
                             /*!=0 explicit file name                         */
  int64_t *err               /* =0 file opened,                               */
                             /* =1 file failed to open because the            */
                             /*    expected environment not set               */
                             /* =<other> file not opened for another reason   */
)
{
   char *fname;
   char *gname;
   integer the_unit_val;

   enum   filestat { old, new };
   enum   filestat filestatus;


   fname = calloc((size_t)(*char_len) + 1,1);

   the_unit_val=(integer)*unit;
   the_unit=&the_unit_val;
   ops_pf[*unit] = NULL;
/* convert file name to C format */

   strncpy( fname, file_name, (size_t)(*char_len) );
   fname[ *char_len ] = '\0';
   sscanf( fname, "%s", fname );

   if ( *environ_var_flag == 0 )  /* File name held in environ var */
   {  gname = getenv( fname );
      if ( gname == NULL ) {
        snprintf(message, MAX_OUTSTR ,
         "OPEN:  WARNING: Environment variable %s not set",
         fname);
        ops_open_flag[*unit]=1;
        *err=1;
        free (fname);
        return;
      }
   }
   else                           /* get file name from argmt fname */
      gname = fname;


   /* Check if file exists */

   if ( access( gname, 0 ) == 0 )  {   /* file exists */

      filestatus = old;
   }
   else  {   /* non-existent file */

      filestatus = new;
   }


   if ( filestatus == old )  {

      if ( *intent == readonly )  {

         if ( ( ops_pf[*unit] = fopen( gname, "rb" ) ) == NULL )  {
            perror("OPEN:  File Open Failed");
            snprintf(message, MAX_OUTSTR ,
              "OPEN:  Unable to Open File %s for Reading", gname );
         }
      }
      else  {   /*  *intent == read_and_write )  */

         if ( ( ops_pf[*unit] = fopen( gname, "r+b" ) ) == NULL )  {
            perror("OPEN:  File Open Failed");
            snprintf(message, MAX_OUTSTR ,
              "OPEN:  Unable to Open File %s for Read/Write", gname );
         }
      }
   }


/* New file - check for write */
   if ( filestatus == new )  {

/* Initialise the file control word to NULL */
      ops_pf[*unit] = NULL;

      if ( *intent == readonly )  {
         snprintf(message, MAX_OUTSTR , "OPEN:  **WARNING: FILE NOT FOUND" );
         snprintf(message, MAX_OUTSTR ,
          "OPEN:  Ignored Request to Open File %s for Reading",
           gname );
      }
      else  {        /*  *intent == read_and_write   */

/* File size not given - just open the file normally */
          if ( ( ops_pf[*unit] = fopen( gname, "w+b" ) ) == NULL )  {
            perror("OPEN:  File Creation Failed");
            snprintf(message, MAX_OUTSTR ,
             "OPEN:  Unable to Open File %s for Read/Write", gname );
          }
          else  {
          }
      }
   }


   /* Set error code and open flag used by ops_buffin and ops_buffout */

   if( ops_pf[*unit] == NULL )  {
      *err = 1;
      ops_open_flag[*unit]=1;
   }
   else  {
      if ( setvbuf( ops_pf[*unit], NULL, _IOFBF, BUFSIZ ) != 0 )  {
         perror("\n**Warning: setvbuf failed");
         *err=1;
         ops_open_flag[*unit]=1;
       }
       else
       {
         *err = 0;
         ops_open_flag[*unit]=0;

/*    set buffer to default size to force buffer alloc on heap */
  /*  setvbuf(ops_pf[*unit],NULL,_IOFBF,BUFSIZ);  See above */
        }
   }
   io_position[*unit]=0;
   prev_fileop[*unit]=UM_UNSET;

free (fname);
}

void ops_close_single
(
int64_t *unit,             /* Fortran unit                                  */
void    *file_name,        /* File name or environ. var. (void * to char[]) */
int64_t *char_len,         /* No of chars in file name                      */
int64_t *environ_var_flag, /* =0 file name in environment var,              */
                           /*!=0 explicit file name                         */
int64_t *delete,           /* =0 do not delete file,!=0 delete file         */
int64_t *err               /* ERROR CHECKING err = 0 no errors,             */
                           /*                err = 1 Errors                 */
)
{
char *fname;
char *gname;
int i;
integer k;
integer the_unit_val;

*err = 1;

the_unit_val=(integer)*unit;
the_unit=&the_unit_val;
fname = calloc((size_t)(*char_len) + 1,1);
/* first check to see if unit has been closed already (or not opened)*/
if(ops_open_flag[*unit]== 0){    /* unit currently open  */

/* close file */

      k=fclose(ops_pf[*unit]);

/* convert file name to C format */
        strncpy(fname,file_name,(size_t)(*char_len));
        fname[*char_len] = '\0';
        for (i=0; i<*char_len; i++){

            if (fname[i] == ' '){
               fname[i] = '\0';
               break;
            }
         }

        if(*environ_var_flag == 0)
          { gname = getenv( fname );
            if ( gname == NULL ) {
              snprintf(message, MAX_OUTSTR ,
               "CLOSE: WARNING: Environment variable %s not set",
               fname);
            ops_open_flag[*unit]=1;
            free( fname );
            return;
            }
          }
        else
          gname=fname;

      if(k==0){

/* delete file */
        if(*delete != 0){
          k=remove(gname);
          if( k != 0){
            fprintf(stderr,
             "CLOSE: Cannot Delete File %s",gname);
            ereport("ops_portio2a:open","Failure in remove() (aka delete)");
          }
          else{  /*normal end to delete so:*/
            ops_open_flag[*unit]=1;     /* set unit flag to closed */
            *err = 0;
          }

        }
        else{
/* file closed */
           ops_open_flag[*unit]=1;     /* set unit flag to closed */
                                 *err = 0;
        }
      }
/* file not closed */
    else {
          snprintf(message, MAX_OUTSTR ,
           "CLOSE: Cannot Close File %s on Unit %d",
           gname, (int) *unit);
    }

}   /* end of test for unit already closed */

free( fname );

}



void ops_buffout32_single
(integer *unit, /* Fortran unit                            */
 void *array, /* Array from which data is written        */
 integer *maxlen, /* Number of real numbers to be written    */
integer * length,/* Number of real numbers actually written */
 real *status) /* Return code                             */
{
#if defined(LITTLE_END)
  integer nsize;                          /* size of array */
  char message[200];
#endif
  integer elsize;  /* Size of elements in array */
  integer istatus;
  /* Default size of element is size of array elements */
  elsize = WORD32BYTES; /* Size of 32 bit word */

  if (ops_open_flag[*unit]==0){
/* Only need to flush if previous file operation was not a WRITE */
    if(prev_fileop[*unit] == UM_READ)
    {
      ops_sync_to_cache(unit);
    }
    prev_fileop[*unit] = UM_WRITE;

#if defined(LITTLE_END)
#if defined (FRL8)
    nsize = (*maxlen+1)/2;
#else
    nsize = *maxlen;
#endif
#endif

#if defined(LITTLE_END)
    c_shum_byteswap(array, *maxlen, WORD32BYTES, message, 200);
#endif

    ops_output_buffer(unit, array, maxlen, length, &elsize, &istatus);
    *status=(real)(istatus);

#if defined (LITTLE_END)
    c_shum_byteswap(array, *maxlen, WORD32BYTES, message, 200);
#endif

  }
}

void ops_buffin32_single
(
integer *unit,   /* Fortran unit                                           */
void *array,     /* Array from which data is read (void * to um_data_t[])  */
integer *maxlen, /* Number of real numbers to be read                      */
integer *length, /* Number of real numbers actually read                   */
real *status     /* Return code                                            */
)
{

  #if defined(LITTLE_END)
  int nsize;
  char message[200];
  #endif

  int k;

    if (ops_open_flag[*unit]==0){
/* Only need to flush if previous file operation was not a READ */
      if(prev_fileop[*unit] == UM_WRITE)
      {
        ops_sync_to_cache(unit);
      }
      prev_fileop[*unit] = UM_READ;

      *length = (integer)fread(array,WORD32BYTES,(size_t)(*maxlen),ops_pf [*unit]);

#if defined(LITTLE_END)
#if defined (FRL8)
      nsize = (int)(*maxlen+1)/2;
#else
      nsize = *maxlen;
#endif
      c_shum_byteswap(array, *maxlen, WORD32BYTES, message, 200);
#endif

      *status=-1.0;
      k=feof(ops_pf[*unit]);
      if(k != 0)
      {
        snprintf(message, MAX_OUTSTR ,"C I/O Error: failed in OPS_BUFFIN32\n");
        snprintf(message, MAX_OUTSTR ,"Return code = %d\n",k);
        *status=0.0;
      }
      k=ferror(ops_pf[*unit]);
      if(k != 0)
      {
        snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFIN32\n");
        snprintf(message, MAX_OUTSTR , "Return code = %d\n",k);
        *status=1.0;
      }
    }
    else
      *status=3.0;
    /* Position of file must be updated */
    io_position[*unit]=io_position[*unit]+(*length*WORD32BYTES);
}

void
#if defined(C_LOW)
ops_buffin16
#elif defined(C_LOW_U)
ops_buffin16_
#else
OPS_BUFFIN16
#endif
(integer *unit,  /* Fortran unit                         */
short array[],     /* Array from which data is read        */
integer *maxlen,  /* Number of real numbers to be read    */
integer * length,/* Number of real numbers actually read */
real *status) /* Return code                          */
{
  int k;
  char message[200];

    if (ops_open_flag[*unit]==0){
/* Only need to flush if previous file operation was not a READ */
      if(prev_fileop[*unit] == UM_WRITE)
      {
        ops_sync_to_cache(unit);
      }
      prev_fileop[*unit] = UM_READ;

      *length = (integer)fread(array,WORD16BYTES,(size_t)(*maxlen),ops_pf [*unit]);

#if defined(LITTLE_END)
      c_shum_byteswap(array, *maxlen, WORD16BYTES, message, 200);
#endif

      *status=-1.0;
      k=feof(ops_pf[*unit]);
      if(k != 0)
      {
        snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFIN16\n");
        snprintf(message, MAX_OUTSTR , "Return code = %d\n", k);
        *status=0.0;
      }
      k=ferror(ops_pf[*unit]);
      if(k != 0)
      {
        snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFIN16\n");
        snprintf(message, MAX_OUTSTR , "Return code = %d\n", k);
        *status=1.0;
      }
    }
    else
      *status=3.0;
    /* Position of file must be updated */
    io_position[*unit]=io_position[*unit]+(*length*WORD16BYTES);
}

void ops_buffin8_single
(
integer *unit,   /* Fortran unit                                      */
void *array,     /* Array into which data is read (void * to char[] ) */
integer *maxlen, /* Number of bytes to be read                        */
integer *length, /* Number of bytes actually read                     */
real *status     /* Return code                                       */
)
{
  int k;

  if(ops_open_flag[*unit]== 0){
    *length = (integer)fread(array,1,(size_t)(*maxlen),ops_pf[*unit]);

        *status=-1.0;
        k=feof(ops_pf[*unit]);
    if(k != 0)
    {
      snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFIN8\n");
      snprintf(message, MAX_OUTSTR , "Return code = %d\n",k);
      *status=0.0;
    }
        k=ferror(ops_pf[*unit]);
    if(k != 0)
    {
      snprintf(message, MAX_OUTSTR ,"C I/O Error: failed in OPS_BUFFIN8\n");
      snprintf(message, MAX_OUTSTR ,"Return code = %d\n",k);
      *status=1.0;
    }
   }
   else
        *status=3.0;

}

void
#if defined(C_LOW)
ops_buffou8
#elif defined(C_LOW_U)
ops_buffou8_
#else
OPS_BUFFOU8
#endif
(integer *unit, /* Fortran unit                            */
char    array[], /* Array from which data is written        */
integer *maxlen,/* Number of bytes to be written           */
integer *length,  /* Number of bytes actually written        */
real *status) /* Return code                             */
{
  int k;

  if(ops_open_flag[*unit]== 0){

/* Only need to flush if previous file operation was not a WRITE */
    if(prev_fileop[*unit] == UM_READ)
    {
      ops_sync_to_cache(unit);
    }
    prev_fileop[*unit] = UM_WRITE;

    *length = (integer)fwrite(array,1,(size_t)(*maxlen),ops_pf[*unit]);

        *status=-1.0;
        k=feof(ops_pf[*unit]);
    if(k != 0)
    {
      snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFOU8\n");
      snprintf(message, MAX_OUTSTR , "Return code = %d\n",k);
      *status=0.0;
    }
        k=ferror(ops_pf[*unit]);
    if(k != 0)
    {
      snprintf(message, MAX_OUTSTR , "C I/O Error: failed in OPS_BUFFOU8\n");
      snprintf(message, MAX_OUTSTR , "Return code = %d\n",k);
      *status=1.0;
    }
   }
   else
        *status=3.0;

}

void ops_setpos8_single
(
integer *unit,        /* Fortran unit                         */
integer *byte_address /* Number of bytes into file            */
)
{
#if defined(LFS)
off_t k;
#else
long k;
#endif

#if defined(LFS)
    k = fseeko(ops_pf[*unit],*byte_address,SEEK_SET);
#else /* LFS */
    k = fseek(ops_pf[*unit],*byte_address,SEEK_SET);
#endif /* LFS */
    if(k!=0){
         snprintf(message, MAX_OUTSTR , "ERROR detected in OPS_SETPOS\n");
         snprintf(message, MAX_OUTSTR ,
                  "word_address = %lld\n", (long long) *byte_address);
         snprintf(message, MAX_OUTSTR , "Return code from fseek = %d\n",(int) k);
    }
}

void
#if defined(C_LOW)
ops_setpos32
#elif defined(C_LOW_U)
ops_setpos32_
#else
OPS_SETPOS32
#endif
(integer *unit,           /* Fortran unit                         */
 integer *word32_address, /* Number of 32bit ints into the file   */
 integer *err)            /* 0: no error                          */
                          /* 1: error occured                     */
{
  int k;
#if defined(LFS)
  off_t byte_address;
#else
  long byte_address;
#endif

  *err=0;
  if (ops_open_flag[*unit]==0) {

    byte_address=(*word32_address)*WORD32BYTES;

#if defined(LFS)
    k=fseeko(ops_pf[*unit],byte_address,SEEK_SET);
#else /* LFS */
    k=fseek(ops_pf[*unit],byte_address,SEEK_SET);
#endif /* LFS */

    if (k != 0) {
      k=errno;
      perror("\nOPS_SETPOS32: Seek failed");
      fprintf(stderr,
        "OPS_SETPOS32: Unit %d to 32bit Word Address %d failed. Error: %d",
        (int) *unit, (int) *word32_address, k);
      the_unit=unit;
      *err=1;
      ereport("ops_portio2a:ops_setpos32","Failed in fseek[o]");
    }

  }
}

void
ops_sync_to_cache
(integer * unit)
{
  if(ops_open_flag[*unit]== 0){
    /* Now flush to disk - we could have used fseek but this is tidier */
    fflush(ops_pf[*unit]);
  }
}
