#ifndef C_FORT2C_TYPES_H
#define C_FORT2C_TYPES_H

/* Declares variable type real to have the same wordlength          */
/* as a Fortran REAL data type, integer to have the same            */
/* wordlength as Fortran INTEGER data type.                         */

#if defined(C_INT)

/* Fortran INTEGER is equivalent to C int */
typedef int integer;
typedef unsigned int u_integer;

#elif defined(C_LONG_INT)

/* Fortran INTEGER is equivalent to C long int */
typedef unsigned long int u_integer;
typedef long int integer;

#elif defined(C_LONG_LONG_INT)

/* Fortran INTEGER is equivalent to C long long int */
typedef unsigned long long int u_integer;
typedef long long int integer;

#else

/* DEFAULT: Fortran INTEGER is equivalent to C int */
typedef unsigned int u_integer;
typedef int integer;

#endif

/* We cannot tell what type of data is being written since we might have
 * packed data which is unsafe to be represented by a real datatype.  Using an
 * integer for um_data_t will allow us to not have to deal with unrepresentable
 * bit patterns in floating point numbers.
 */
#if defined(FRL8)
/* Fortran REAL is equivalent to C double */
typedef double real;
typedef int64_t um_data_t;
#else
/* Fortran REAL is equivalent to C float */
typedef float real;
typedef int32_t um_data_t;
#endif

/* Number of bytes in a word of a specific bit size */
#define WORD16BYTES 2 /* 16 bit */
#define WORD32BYTES 4 /* 32 bit */
#define WORD64BYTES 8 /* 64 bit */

#endif
