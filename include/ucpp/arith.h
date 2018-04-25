/*
 * Integer arithmetic evaluation, header file.
 *
 * (c) Thomas Pornin 2002
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. The name of the authors may not be used to endorse or promote
 *    products derived from this software without specific prior written
 *    permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * This arithmetic evaluator uses two files: this header file (arith.h)
 * and the source file (arith.c). To use this code, the source file should
 * be included from another .c file which defines some macros (see below).
 * Then the functions defined in the arith.c file become available to the
 * including source file. If those functions are defined with external
 * linkage (that is, `ARITH_FUNCTION_HEADER' does not contain `static'),
 * it is possible for other source files to use the arithmetic functions
 * by including the arith.h header only. The source file which includes
 * arith.c should *not* include arith.h.
 *
 * If the #include is for arith.h, the following macros should be
 * defined:
 *
 * -- If the evaluator is supposed to use a native type:
 *   NATIVE_SIGNED            the native signed integer type
 *   NATIVE_UNSIGNED          the native unsigned integer type
 *
 * -- If the evaluator is supposed to use an emulated type:
 *   SIMUL_ARITH_SUBTYPE      the native unsigned type used for the simulation
 *   SIMUL_SUBTYPE_BITS       the native unsigned type size
 *   SIMUL_NUMBITS            the emulated type size
 *
 * -- For both cases:
 *   ARITH_TYPENAME           the central arithmetic type name
 *   ARITH_FUNCTION_HEADER    the qualifiers to add to function definitions
 *
 * The presence (respectively absence) of the NATIVE_SIGNED macro triggers
 * the use of the native type evaluator (respectively simulated type
 * evaluator).
 *
 * If the #include is for arith.c, the macros for arith.h should be defined,
 * and the following should be defined as well:
 *
 * -- If the evaluator is supposed to use a native type:
 *   NATIVE_UNSIGNED_BITS     the native unsigned type size
 *   NATIVE_SIGNED_MIN        the native signed minimum value
 *   NATIVE_SIGNED_MAX        the native signed maximum value
 * (the last two macros must evaluate to signed constant expressions)
 *
 * -- For both cases:
 *   ARITH_WARNING(type)      code to perform on warning
 *   ARITH_ERROR(type)        code to perform on error
 *
 * The macro ARITH_WARNING() and ARITH_ERROR() are invoked with a
 * numerical argument which is one of the enumeration constants
 * defined below (ARITH_EXCEP_*) that identifies the specific problem.
 *
 * If the #include is for arith.c, the macro ARITHMETIC_CHECKS may be
 * defined. When this macro is defined, checks are performed so that all
 * operation which would lead to undefined or implementation-defined
 * behaviour are first reported through ARITH_WARNING(). Code is smaller
 * and faster without these checks, of course. Regardless of the status
 * of that macro, divisions by 0 and overflows on signed division are
 * reported as errors through ARITH_ERROR().
 *
 */

/*
 * 2018 - internals moved to include/internal/arith.h
 */

#ifndef UCPP_ARITH_H
#define UCPP_ARITH_H

enum {
	/* Warnings */
	ARITH_EXCEP_CONV_O,    /* overflow on conversion */
	ARITH_EXCEP_NEG_O,     /* overflow on unary minus */
	ARITH_EXCEP_NOT_T,     /* trap representation on bitwise inversion */
	ARITH_EXCEP_PLUS_O,    /* overflow on addition */
	ARITH_EXCEP_PLUS_U,    /* underflow on addition */
	ARITH_EXCEP_MINUS_O,   /* overflow on subtraction */
	ARITH_EXCEP_MINUS_U,   /* underflow on subtraction */
	ARITH_EXCEP_AND_T,     /* trap representation on bitwise and */
	ARITH_EXCEP_XOR_T,     /* trap representation on bitwise xor */
	ARITH_EXCEP_OR_T,      /* trap representation on bitwise or */
	ARITH_EXCEP_LSH_W,     /* left shift by type width or more */
	ARITH_EXCEP_LSH_C,     /* left shift by negative count */
	ARITH_EXCEP_LSH_O,     /* overflow on left shift */
	ARITH_EXCEP_LSH_U,     /* underflow on left shift */
	ARITH_EXCEP_RSH_W,     /* right shift by type width or more */
	ARITH_EXCEP_RSH_C,     /* right shift by negative count */
	ARITH_EXCEP_RSH_N,     /* right shift of negative value */
	ARITH_EXCEP_STAR_O,    /* overflow on multiplication */
	ARITH_EXCEP_STAR_U,    /* underflow on multiplication */

	/* Errors */
	ARITH_EXCEP_SLASH_D,   /* division by 0 */
	ARITH_EXCEP_SLASH_O,   /* overflow on division */
	ARITH_EXCEP_PCT_D,     /* division by 0 on modulus operator */
	ARITH_EXCEP_CONST_O    /* constant too large */
};

#endif /* UCPP_ARITH_H */
