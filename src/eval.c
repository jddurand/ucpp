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

#include "ucpp/tune.h"
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "ucpp/ucppi.h"
#include "ucpp/internal/mem.h"
#include "ucpp/internal/context.h"

/*
 * If you want to hardcode a conversion table, define a static array
 * of 256 int, and make transient_characters point to it.
 */
int *transient_characters = 0;

#define OCTAL(x)       ((x) >= '0' && (x) <= '7')
#define DECIM(x)       ((x) >= '0' && (x) <= '9')
#define HEXAD(x)       (DECIM(x) \
                       || (x) == 'a' || (x) == 'b' || (x) == 'c' \
                       || (x) == 'd' || (x) == 'e' || (x) == 'f' \
                       || (x) == 'A' || (x) == 'B' || (x) == 'C' \
                       || (x) == 'D' || (x) == 'E' || (x) == 'F')
#define OVAL(x)	       ((int)((x) - '0'))
#define DVAL(x)	       ((int)((x) - '0'))
#define HVAL(x)	       (DECIM(x) ? DVAL(x) \
                       : (x) == 'a' || (x) == 'A' ? 10 \
                       : (x) == 'b' || (x) == 'B' ? 11 \
                       : (x) == 'c' || (x) == 'C' ? 12 \
                       : (x) == 'd' || (x) == 'D' ? 13 \
                       : (x) == 'e' || (x) == 'E' ? 14 : 15)

#define ARITH_TYPENAME             big
#define ARITH_FUNCTION_HEADER      static inline

#define ARITH_ERROR(ucpp_context, type)          z_error(ucpp_context, type)
static void z_error(ucpp_context_t *ucpp_context, int type);

#ifdef ARITHMETIC_CHECKS
#define ARITH_WARNING(ucpp_context, type)        z_warn(ucpp_context, type)
static void z_warn(ucpp_context_t *ucpp_context, int type);
#endif

#include "ucpp/internal/arith.c"

static void z_error(ucpp_context_t *ucpp_context, int type)
{
	switch (type) {
	case ARITH_EXCEP_SLASH_D:
		error(ucpp_context, ucpp_context->eval_line, "division by 0");
		break;
	case ARITH_EXCEP_SLASH_O:
		error(ucpp_context, ucpp_context->eval_line, "overflow on division");
		break;
	case ARITH_EXCEP_PCT_D:
		error(ucpp_context, ucpp_context->eval_line, "division by 0 on modulus operator");
		break;
	case ARITH_EXCEP_CONST_O:
		error(ucpp_context, ucpp_context->eval_line, "constant too large for destination type");
		break;
#ifdef AUDIT
	default:
		ouch("erroneous integer error: %d", type);
#endif
	}
	throw(eval_exception);
}

#ifdef ARITHMETIC_CHECKS
static void z_warn(ucpp_context_t *ucpp_context, int type)
{
	switch (type) {
	case ARITH_EXCEP_CONV_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on integer conversion");
		break;
	case ARITH_EXCEP_NEG_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on unary minus");
		break;
	case ARITH_EXCEP_NOT_T:
		warning(ucpp_context, ucpp_context->eval_line,
			"bitwise inversion yields trap representation");
		break;
	case ARITH_EXCEP_PLUS_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on addition");
		break;
	case ARITH_EXCEP_PLUS_U:
		warning(ucpp_context, ucpp_context->eval_line, "underflow on addition");
		break;
	case ARITH_EXCEP_MINUS_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on subtraction");
		break;
	case ARITH_EXCEP_MINUS_U:
		warning(ucpp_context, ucpp_context->eval_line, "underflow on subtraction");
		break;
	case ARITH_EXCEP_AND_T:
		warning(ucpp_context, ucpp_context->eval_line,
			"bitwise AND yields trap representation");
		break;
	case ARITH_EXCEP_XOR_T:
		warning(ucpp_context, ucpp_context->eval_line,
			"bitwise XOR yields trap representation");
		break;
	case ARITH_EXCEP_OR_T:
		warning(ucpp_context, ucpp_context->eval_line,
			"bitwise OR yields trap representation");
		break;
	case ARITH_EXCEP_LSH_W:
		warning(ucpp_context, ucpp_context->eval_line, "left shift count greater than "
			"or equal to type width");
		break;
	case ARITH_EXCEP_LSH_C:
		warning(ucpp_context, ucpp_context->eval_line, "left shift count negative");
		break;
	case ARITH_EXCEP_LSH_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on left shift");
		break;
	case ARITH_EXCEP_RSH_W:
		warning(ucpp_context, ucpp_context->eval_line, "right shift count greater than "
			"or equal to type width");
		break;
	case ARITH_EXCEP_RSH_C:
		warning(ucpp_context, ucpp_context->eval_line, "right shift count negative");
		break;
	case ARITH_EXCEP_RSH_N:
		warning(ucpp_context, ucpp_context->eval_line, "right shift of negative value");
		break;
	case ARITH_EXCEP_STAR_O:
		warning(ucpp_context, ucpp_context->eval_line, "overflow on multiplication");
		break;
	case ARITH_EXCEP_STAR_U:
		warning(ucpp_context, ucpp_context->eval_line, "underflow on multiplication");
		break;
#ifdef AUDIT
	default:
		ouch("erroneous integer warning: %d", type);
#endif
	}
}
#endif

typedef struct {
	int sign;
	union {
		u_big uv;
		s_big sv;
	} u;
} ppval;

static int boolval(ucpp_context_t *ucpp_context, ppval x)
{
	return x.sign ? big_s_lval(ucpp_context, x.u.sv) : big_u_lval(ucpp_context, x.u.uv);
}

#if !defined(WCHAR_SIGNEDNESS)
#  if CHAR_MIN == 0
#    define WCHAR_SIGNEDNESS	0
#  else
#    define WCHAR_SIGNEDNESS	1
#  endif
#endif

/*
 * Check the suffix, return 1 if it is signed, 0 otherwise. 1 is
 * returned for a void suffix. Legal suffixes are:
 * unsigned: u U ul uL Ul UL lu Lu lU LU ull uLL Ull ULL llu LLu llU LLU
 * signed: l L ll LL
 */
static int pp_suffix(ucpp_context_t *ucpp_context, char *d, char *refc)
{
	if (!*d) return 1;
	if (*d == 'u' || *d == 'U') {
		if (!*(++ d)) return 0;
		if (*d == 'l' || *d == 'L') {
			char *e = d + 1;

			if (*e && *e != *d) goto suffix_error;
			if (!*e || !*(e + 1)) return 0;
			goto suffix_error;
		}
		goto suffix_error;
	}
	if (*d == 'l' || *d == 'L') {
		if (!*(++ d)) return 1;
		if (*d == *(d - 1)) {
			d ++;
			if (!*d) return 1;
		}
		if (*d == 'u' || *d == 'U') {
			d ++;
			if (!*d) return 0;
		}
		goto suffix_error;
	}
suffix_error:
	error(ucpp_context, ucpp_context->eval_line, "invalid integer constant '%s'", refc);
	throw(eval_exception);
	return 666;
}

static unsigned long pp_char(ucpp_context_t *ucpp_context, char *c, char *refc)
{
	unsigned long r = 0;

	c ++;
	if (*c == '\\') {
		int i;

		c ++;
		switch (*c) {
		case 'n': r = '\n'; c ++; break;
		case 't': r = '\t'; c ++; break;
		case 'v': r = '\v'; c ++; break;
		case 'b': r = '\b'; c ++; break;
		case 'r': r = '\r'; c ++; break;
		case 'f': r = '\f'; c ++; break;
		case 'a': r = '\a'; c ++; break;
		case '\\': r = '\\'; c ++; break;
		case '\?': r = '\?'; c ++; break;
		case '\'': r = '\''; c ++; break;
		case '\"': r = '\"'; c ++; break;
		case 'u':
			for (i = 0, c ++; i < 4 && HEXAD(*c); i ++, c ++) {
				r = (r * 16) + HVAL(*c);
			}
			if (i != 4) {
				error(ucpp_context, ucpp_context->eval_line, "malformed UCN in %s", refc);
				throw(eval_exception);
			}
			break;
		case 'U':
			for (i = 0, c ++; i < 8 && HEXAD(*c); i ++, c ++) {
				r = (r * 16) + HVAL(*c);
			}
			if (i != 8) {
				error(ucpp_context, ucpp_context->eval_line, "malformed UCN in %s", refc);
				throw(eval_exception);
			}
			break;
		case 'x':
			for (c ++; HEXAD(*c); c ++) r = (r * 16) + HVAL(*c);
			break;
		default:
			if (OCTAL(*c)) {
				r = OVAL(*(c ++));
				if (OCTAL(*c)) r = (r * 8) + OVAL(*(c ++));
				if (OCTAL(*c)) r = (r * 8) + OVAL(*(c ++));
			} else {
				error(ucpp_context, ucpp_context->eval_line, "invalid escape sequence "
					"'\\%c'", *c);
				throw(eval_exception);
			}
		}
	} else if (*c == '\'') {
		error(ucpp_context, ucpp_context->eval_line, "empty character constant");
		throw(eval_exception);
	} else {
		r = *((unsigned char *)(c ++));
	}

	if (transient_characters && r < 256) {
		r = transient_characters[(size_t)r];
	}

	if (*c != '\'' && ucpp_context->emit_eval_warnings) {
		warning(ucpp_context, ucpp_context->eval_line, "multicharacter constant");
	}
	return r;
}

static ppval pp_strtoconst(ucpp_context_t *ucpp_context, char *refc)
{
	ppval q;
	char *c = refc, *d;
	u_big ru;
	s_big rs;
	int sp, dec;

	if (*c == '\'' || *c == 'L') {
		q.sign = (*c == 'L') ? WCHAR_SIGNEDNESS : 1;
		if (*c == 'L' && *(++ c) != '\'') {
			error(ucpp_context, ucpp_context->eval_line,
				"invalid wide character constant: %s", refc);
			throw(eval_exception);
		}
		if (q.sign) {
			q.u.sv = big_s_fromlong(ucpp_context, pp_char(ucpp_context, c, refc));
		} else {
			q.u.uv = big_u_fromulong(ucpp_context, pp_char(ucpp_context, c, refc));
		}
		return q;
	}
	if (*c == '0') {
		/* octal or hexadecimal */
		dec = 0;
		c ++;
		if (*c == 'x' || *c == 'X') {
			c ++;
			d = big_u_hexconst(ucpp_context, c, &ru, &rs, &sp);
		} else {
			d = big_u_octconst(ucpp_context, c, &ru, &rs, &sp);
		}
	} else {
		dec = 1;
		d = big_u_decconst(ucpp_context, c, &ru, &rs, &sp);
	}
	q.sign = pp_suffix(ucpp_context, d, refc);
	if (q.sign) {
		if (!sp) {
			if (dec) {
				error(ucpp_context, ucpp_context->eval_line, "constant too large "
					"for destination type");
				throw(eval_exception);
			} else {
				warning(ucpp_context, ucpp_context->eval_line, "constant is so large "
					"that it is unsigned");
			}
			q.u.uv = ru;
			q.sign = 0;
		} else {
			q.u.sv = rs;
		}
	} else {
		q.u.uv = ru;
	}
	return q;
}

/*
 * Used by #line directives -- anything beyond what can be put in an
 * unsigned long, is considered absurd.
 */
unsigned long strtoconst(ucpp_context_t *ucpp_context, char *c)
{
	ppval q = pp_strtoconst(ucpp_context, c);

	if (q.sign) q.u.uv = big_s_to_u(ucpp_context, q.u.sv);
	return big_u_toulong(ucpp_context, q.u.uv);
}

#define OP_UN(x)	((x) == LNOT || (x) == NOT || (x) == UPLUS \
			|| (x) == UMINUS)

static ppval eval_opun(ucpp_context_t *ucpp_context, int op, ppval v)
{
	if (op == LNOT) {
		v.sign = 1;
		v.u.sv = big_s_fromint(ucpp_context, big_s_lnot(ucpp_context, v.u.sv));
		return v;
	}
	if (v.sign) {
		switch (op) {
		case NOT: v.u.sv = big_s_not(ucpp_context, v.u.sv); break;
		case UPLUS: break;
		case UMINUS: v.u.sv = big_s_neg(ucpp_context, v.u.sv); break;
		}
	} else {
		switch (op) {
		case NOT: v.u.uv = big_u_not(ucpp_context, v.u.uv); break;
		case UPLUS: break;
		case UMINUS: v.u.uv = big_u_neg(ucpp_context, v.u.uv); break;
		}
	}
	return v;
}

#define OP_BIN(x)      ((x) == STAR || (x) == SLASH || (x) == PCT \
                       || (x) == PLUS || (x) == MINUS || (x) == LSH \
                       || (x) == RSH || (x) == LT || (x) == LEQ \
                       || (x) == GT || (x) == GEQ || (x) == SAME \
                       || (x) == NEQ || (x) == AND || (x) == CIRC \
                       || (x) == OR || (x) == LAND || (x) == LOR \
                       || (x) == COMMA)

static ppval eval_opbin(ucpp_context_t *ucpp_context, int op, ppval v1, ppval v2)
{
	ppval r;
	int iv2 = 0;

	switch (op) {
	case STAR:	case SLASH:	case PCT:
	case PLUS:	case MINUS:	case AND:
	case CIRC:	case OR:
		/* promote operands, adjust signedness of result */
		if (!v1.sign || !v2.sign) {
			if (v1.sign) {
				v1.u.uv = big_s_to_u(ucpp_context, v1.u.sv);
				v1.sign = 0;
			} else if (v2.sign) {
				v2.u.uv = big_s_to_u(ucpp_context, v2.u.sv);
				v2.sign = 0;
			}
			r.sign = 0;
		} else {
			r.sign = 1;
		}
		break;
	case LT:	case LEQ:	case GT:
	case GEQ:	case SAME:	case NEQ:
		/* promote operands */
		if (!v1.sign || !v2.sign) {
			if (v1.sign) {
				v1.u.uv = big_s_to_u(ucpp_context, v1.u.sv);
				v1.sign = 0;
			} else if (v2.sign) {
				v2.u.uv = big_s_to_u(ucpp_context, v2.u.sv);
				v2.sign = 0;
			}
		}
		/* fall through */
	case LAND:
	case LOR:
		/* result is signed anyway */
		r.sign = 1;
		break;
	case LSH:
	case RSH:
		/* result is as signed as left operand; convert right
		   operand to int */
		r.sign = v1.sign;
		if (v2.sign) {
			iv2 = big_s_toint(ucpp_context, v2.u.sv);
		} else {
			iv2 = big_u_toint(ucpp_context, v2.u.uv);
		}
		break;
	case COMMA:
		if (ucpp_context->emit_eval_warnings) {
			warning(ucpp_context, ucpp_context->eval_line, "ISO C forbids evaluated comma "
				"operators in #if expressions");
		}
		r.sign = v2.sign;
		break;
#ifdef AUDIT
	default: ouch("a good operator is a dead operator");
#endif
	}

#define SBINOP(context, x)	if (r.sign) r.u.sv = big_s_ ## x (context, v1.u.sv, v2.u.sv); \
			else r.u.uv = big_u_ ## x (context, v1.u.uv, v2.u.uv);

#define NSSBINOP(context, x)	if (v1.sign) r.u.sv = big_s_fromint(context, big_s_ ## x \
			(v1.u.sv, v2.u.sv)); else r.u.sv = big_s_fromint( \
			context, big_u_ ## x (context, v1.u.uv, v2.u.uv));

#define LBINOP(context, x)	if (v1.sign) r.u.sv = big_s_fromint( \
			context, big_s_lval(context, v1.u.sv) x big_s_lval(context, v2.u.sv)); \
			else r.u.sv = big_s_fromint( \
			context, big_u_lval(context, v1.u.uv) x big_u_lval(context, v2.u.uv));

#define ABINOP(context, x)	if (r.sign) r.u.sv = big_s_ ## x (context, v1.u.sv, iv2); \
			else r.u.uv = big_u_ ## x (context, v1.u.uv, iv2);

	switch (op) {
	case STAR: SBINOP(ucpp_context, star); break;
	case SLASH: SBINOP(ucpp_context, slash); break;
	case PCT: SBINOP(ucpp_context, pct); break;
	case PLUS: SBINOP(ucpp_context, plus); break;
	case MINUS: SBINOP(ucpp_context, minus); break;
	case LSH: ABINOP(ucpp_context, lsh); break;
	case RSH: ABINOP(ucpp_context, rsh); break;
	case LT: NSSBINOP(ucpp_context, lt); break;
	case LEQ: NSSBINOP(ucpp_context, leq); break;
	case GT: NSSBINOP(ucpp_context, gt); break;
	case GEQ: NSSBINOP(ucpp_context, geq); break;
	case SAME: NSSBINOP(ucpp_context, same); break;
	case NEQ: NSSBINOP(ucpp_context, neq); break;
	case AND: SBINOP(ucpp_context, and); break;
	case CIRC: SBINOP(ucpp_context, xor); break;
	case OR: SBINOP(ucpp_context, or); break;
	case LAND: LBINOP(ucpp_context, &&); break;
	case LOR: LBINOP(ucpp_context, ||); break;
	case COMMA: r = v2; break;
	}
	return r;
}

#define ttOP(x)		(OP_UN(x) || OP_BIN(x) || (x) == QUEST || (x) == COLON)

static int op_prec(int op)
{
	switch (op) {
	case LNOT:
	case NOT:
	case UPLUS:
	case UMINUS:
		return 13;
	case STAR:
	case SLASH:
	case PCT:
		return 12;
	case PLUS:
	case MINUS:
		return 11;
	case LSH:
	case RSH:
		return 10;
	case LT:
	case LEQ:
	case GT:
	case GEQ:
		return 9;
	case SAME:
	case NEQ:
		return 8;
	case AND:
		return 7;
	case CIRC:
		return 6;
	case OR:
		return 5;
	case LAND:
		return 4;
	case LOR:
		return 3;
	case QUEST:
		return 2;
	case COMMA:
		return 1;
	}
#ifdef AUDIT
	ouch("an unknown species should have a higher precedence");
#endif
	return 666;
}

/*
 * Perform the hard work of evaluation.
 *
 * This function works because:
 * -- all unary operators are right to left associative, and with
 *    identical precedence
 * -- all binary operators are left to right associative
 * -- there is only one non-unary and non-binary operator: the quest-colon
 *
 * If do_eval is 0, the evaluation of operators is not done. This is
 * for sequence point operators (&&, || and ?:).
 */
static ppval eval_shrd(ucpp_context_t *ucpp_context, struct token_fifo *tf, int minprec, int do_eval)
{
	ppval top;
	struct token *ct;

	top.sign = 1;
	if (tf->art == tf->nt) goto trunc_err;
	ct = tf->t + (tf->art ++);
	if (ct->type == LPAR) {
		top = eval_shrd(ucpp_context, tf, 0, do_eval);
		if (tf->art == tf->nt) goto trunc_err;
		ct = tf->t + (tf->art ++);
		if (ct->type != RPAR) {
			error(ucpp_context, ucpp_context->eval_line, "a right parenthesis was expected");
			throw(eval_exception);
		}
	} else if (ct->type == NUMBER || ct->type == CHAR) {
		top = pp_strtoconst(ucpp_context, ct->name);
	} else if (OP_UN(ct->type)) {
		top = eval_opun(ucpp_context, ct->type, eval_shrd(ucpp_context, tf,
			op_prec(ct->type), do_eval));
		goto eval_loop;
	} else if (ttOP(ct->type)) goto rogue_op_err;
	else {
		goto invalid_token_err;
	}

eval_loop:
	if (tf->art == tf->nt) {
		return top;
	}
	ct = tf->t + (tf->art ++);
	if (OP_BIN(ct->type)) {
		int bp = op_prec(ct->type);

		if (bp > minprec) {
			ppval tr;

			if ((ct->type == LOR && boolval(ucpp_context, top))
				|| (ct->type == LAND && !boolval(ucpp_context, top))) {
				tr = eval_shrd(ucpp_context, tf, bp, 0);
				if (do_eval) {
					top.sign = 1;
					if (ct->type == LOR)
						top.u.sv = big_s_fromint(ucpp_context, 1);
					if (ct->type == LAND)
						top.u.sv = big_s_fromint(ucpp_context, 0);
				}
			} else {
				tr = eval_shrd(ucpp_context, tf, bp, do_eval);
				if (do_eval)
					top = eval_opbin(ucpp_context, ct->type, top, tr);
			}
			goto eval_loop;
		}
	} else if (ct->type == QUEST) {
		int bp = op_prec(QUEST);
		ppval r1, r2;

		if (bp >= minprec) {
			int qv = boolval(ucpp_context, top);

			r1 = eval_shrd(ucpp_context, tf, bp, qv ? do_eval : 0);
			if (tf->art == tf->nt) goto trunc_err;
			ct = tf->t + (tf->art ++);
			if (ct->type != COLON) {
				error(ucpp_context, ucpp_context->eval_line, "a colon was expected");
				throw(eval_exception);
			}
			r2 = eval_shrd(ucpp_context, tf, bp, qv ? 0 : do_eval);
			if (do_eval) {
				if (qv) top = r1; else top = r2;
			}
			goto eval_loop;
		}
	}
	tf->art --;
	return top;

trunc_err:
	error(ucpp_context, ucpp_context->eval_line, "truncated constant integral expression");
	throw(eval_exception);
rogue_op_err:
	error(ucpp_context, ucpp_context->eval_line, "rogue operator '%s' in constant integral "
		"expression", operators_name(ct->type));
	throw(eval_exception);
invalid_token_err:
	error(ucpp_context, ucpp_context->eval_line, "invalid token in constant integral expression");
	throw(eval_exception);
}

#define UNARY(x)	((x) != NUMBER && (x) != NAME && (x) != CHAR \
			&& (x) != RPAR)

/*
 * Evaluate the integer expression contained in the given token_fifo.
 * Evaluation is made by precedence of operators, as described in the
 * Dragon Book. The unary + and - are distinguished from their binary
 * counterparts using the Fortran way: a + or a - is considered unary
 * if it does not follow a constant, an identifier or a right parenthesis.
 */
unsigned long eval_expr(ucpp_context_t *ucpp_context, struct token_fifo *tf, int *ret, int ew)
{
	size_t sart;
	ppval r;

	ucpp_context->emit_eval_warnings = ew;
	if (catch(eval_exception)) goto eval_err;
	/* first, distinguish unary + and - from binary + and - */
	for (sart = tf->art; tf->art < tf->nt; tf->art ++) {
		if (tf->t[tf->art].type == PLUS) {
			if (sart == tf->art || UNARY(tf->t[tf->art - 1].type))
				tf->t[tf->art].type = UPLUS;
		} else if (tf->t[tf->art].type == MINUS) {
			if (sart == tf->art || UNARY(tf->t[tf->art - 1].type))
				tf->t[tf->art].type = UMINUS;
		}
	}
	tf->art = sart;
	r = eval_shrd(ucpp_context, tf, 0, 1);
	if (tf->art < tf->nt) {
		error(ucpp_context, ucpp_context->eval_line, "trailing garbage in constant integral "
			"expression");
		goto eval_err;
	}
	*ret = 0;
	return boolval(ucpp_context, r);
eval_err:
	*ret = 1;
	return 0;
}
