/*
 * (c) Thomas Pornin 1999 - 2002
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

#ifndef UCPP__UCPPI__
#define UCPP__UCPPI__

#include <ucpp/export.h>
#include <ucpp/tune.h>
#include <ucpp/cpp.h>
#include <ucpp/nhash.h>

/*
 * A macro represented in a compact form; simple tokens are represented
 * by one byte, containing their number. Tokens with a string value are
 * followed by the value (string finished by a 0). Macro arguments are
 * followed by the argument number (in one byte -- thus implying a hard
 * limit of 254 arguments (number 255 is for __VA_ARGS__).
 */
struct comp_token_fifo {
	size_t length;
	size_t rp;
	unsigned char *t;
};

/* These declarations are used only internally by ucpp */

/*
 * S_TOKEN(x)	checks whether x is a token type with an embedded string
 * ttMWS(x)	checks whether x is macro whitespace (space, comment...)
 * ttWHI(x)	checks whether x is whitespace (MWS or newline)
 */
#define S_TOKEN(x)	STRING_TOKEN(x)
#define ttMWS(x)	((x) == NONE || (x) == COMMENT || (x) == OPT_NONE)
#define ttWHI(x)	(ttMWS(x) || (x) == NEWLINE)

/*
 * Function prototypes
 */
/*
 * from lexer.c
 */
#define init_cppm	ucpp_init_cppm
#define close_cppm	ucpp_close_cppm
#define put_char	ucpp_put_char
#define discard_char	ucpp_discard_char
#define next_token	ucpp_next_token
#define grap_char	ucpp_grap_char
#define space_char	ucpp_space_char

#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT ucpp_context_t *init_cppm(void);
ucpp_EXPORT void close_cppm(ucpp_context_t *ucpp_context);
ucpp_EXPORT void put_char(ucpp_context_t *ucpp_context, struct lexer_state *, unsigned char);
ucpp_EXPORT void discard_char(struct lexer_state *);
ucpp_EXPORT int next_token(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int grap_char(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int space_char(int);

#ifdef __cplusplus
}
#endif

/*
 * from assert.c
 */
struct assert {
	hash_item_header head;    /* first field */
	size_t nbval;
	struct token_fifo *val;
};

#define cmp_token_list		ucpp_cmp_token_list
#define handle_assert		ucpp_handle_assert
#define handle_unassert		ucpp_handle_unassert
#define get_assertion		ucpp_get_assertion
#define wipe_assertions		ucpp_wipe_assertions

#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT int cmp_token_list(struct token_fifo *, struct token_fifo *);
ucpp_EXPORT int handle_assert(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int handle_unassert(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT struct assert *get_assertion(ucpp_context_t *ucpp_context, char *);
ucpp_EXPORT void wipe_assertions(ucpp_context_t *ucpp_context);

#ifdef __cplusplus
}
#endif

/*
 * from macro.c
 */
struct macro {
	hash_item_header head;     /* first field */
	int narg;
	char **arg;
	int nest;
	int vaarg;
#ifdef LOW_MEM
	struct comp_token_fifo cval;
#else
	struct token_fifo val;
#endif
};

#define print_token		ucpp_print_token
#define handle_define		ucpp_handle_define
#define handle_undef		ucpp_handle_undef
#define handle_ifdef		ucpp_handle_ifdef
#define handle_ifndef		ucpp_handle_ifndef
#define substitute_macro	ucpp_substitute_macro
#define get_macro		ucpp_get_macro
#define wipe_macros		ucpp_wipe_macros
#define dsharp_lexer		ucpp_dsharp_lexer
#define compile_time		ucpp_compile_time
#define compile_date		ucpp_compile_date
#ifdef PRAGMA_TOKENIZE
#define tokenize_lexer		ucpp_tokenize_lexer
#endif

#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT void print_token(ucpp_context_t *ucpp_context, struct lexer_state *, struct token *, long);
ucpp_EXPORT int handle_define(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int handle_undef(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int handle_ifdef(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int handle_ifndef(ucpp_context_t *ucpp_context, struct lexer_state *);
ucpp_EXPORT int substitute_macro(ucpp_context_t *ucpp_context, struct lexer_state *, struct macro *,
	struct token_fifo *, int, int, long);
ucpp_EXPORT struct macro *get_macro(ucpp_context_t *ucpp_context, char *);
ucpp_EXPORT void wipe_macros(ucpp_context_t *ucpp_context);

ucpp_EXPORT struct lexer_state dsharp_lexer;
ucpp_EXPORT char compile_time[12], compile_date[24];
#ifdef PRAGMA_TOKENIZE
ucpp_EXPORT struct lexer_state tokenize_lexer;
#endif

#ifdef __cplusplus
}
#endif

/*
 * from eval.c
 */
#define strtoconst	ucpp_strtoconst
#define eval_expr	ucpp_eval_expr
#define eval_line	ucpp_eval_line

#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT unsigned long strtoconst(ucpp_context_t *ucpp_context, char *);
ucpp_EXPORT unsigned long eval_expr(ucpp_context_t *ucpp_context, struct token_fifo *, int *, int);
ucpp_EXPORT extern long eval_line;

#ifdef __cplusplus
}
#endif

#define eval_exception	ucpp_eval_exception

#ifdef POSIX_JMP
#define JMP_BUF	sigjmp_buf
#define catch(x)	sigsetjmp((x), 0)
#define throw(x)	siglongjmp((x), 1)
#else
#define JMP_BUF	jmp_buf
#define catch(x)	setjmp((x))
#define throw(x)	longjmp((x), 1)
#endif
#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT JMP_BUF eval_exception;

#ifdef __cplusplus
}
#endif

/*
 * from cpp.c
 */
#define token_name		ucpp_token_name
#define throw_away		ucpp_throw_away
#define garbage_collect		ucpp_garbage_collect
#define init_buf_lexer_state	ucpp_init_buf_lexer_state
#ifdef PRAGMA_TOKENIZE
#define compress_token_list	ucpp_compress_token_list
#endif

#ifdef __cplusplus
extern "C" {
#endif

ucpp_EXPORT char *token_name(struct token *);
ucpp_EXPORT void throw_away(struct garbage_fifo *, char *);
ucpp_EXPORT void garbage_collect(struct garbage_fifo *);
ucpp_EXPORT void init_buf_lexer_state(struct lexer_state *, int);
#ifdef PRAGMA_TOKENIZE
struct comp_token_fifo compress_token_list(struct token_fifo *);
#endif

#ifdef __cplusplus
}
#endif

#define ouch		ucpp_ouch
#define error		ucpp_error
#define warning		ucpp_warning

#endif
