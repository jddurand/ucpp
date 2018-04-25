#ifndef UCPP_INTERNAL_ARITH_H
#define UCPP_INTERNAL_ARITH_H

#include <ucpp/arith.h>

#define arith_strc_(x, y)	x ## y
#define arith_strc(x, y)	arith_strc_(x, y)

#define arith_u		arith_strc(u_, ARITH_TYPENAME)
#define arith_s		arith_strc(s_, ARITH_TYPENAME)
#define arith_op_u(op)	arith_strc(ARITH_TYPENAME, arith_strc(_u_, op))
#define arith_op_s(op)	arith_strc(ARITH_TYPENAME, arith_strc(_s_, op))

#define ARITH_DECL_MONO_U_U(op)    ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_u(op)(arith_u x)
#define ARITH_DECL_MONO_U_S(op)    ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_u(op)(arith_u x)
#define ARITH_DECL_MONO_U_I(op)    ARITH_FUNCTION_HEADER int \
                                   arith_op_u(op)(arith_u x)
#define ARITH_DECL_MONO_U_L(op)    ARITH_FUNCTION_HEADER unsigned long \
                                   arith_op_u(op)(arith_u x)
#define ARITH_DECL_MONO_S_U(op)    ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_s(op)(arith_s x)
#define ARITH_DECL_MONO_S_S(op)    ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_s(op)(arith_s x)
#define ARITH_DECL_MONO_S_I(op)    ARITH_FUNCTION_HEADER int \
                                   arith_op_s(op)(arith_s x)
#define ARITH_DECL_MONO_S_L(op)    ARITH_FUNCTION_HEADER long \
                                   arith_op_s(op)(arith_s x)
#define ARITH_DECL_MONO_I_U(op)    ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_u(op)(int x)
#define ARITH_DECL_MONO_L_U(op)    ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_u(op)(unsigned long x)
#define ARITH_DECL_MONO_I_S(op)    ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_s(op)(int x)
#define ARITH_DECL_MONO_L_S(op)    ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_s(op)(long x)
#define ARITH_DECL_MONO_ST_US(op)  ARITH_FUNCTION_HEADER char *arith_op_u(op) \
                                   (char *c, arith_u *ru, arith_s *rs, int *sp)

#define ARITH_DECL_BI_UU_U(op)     ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_u(op)(arith_u x, arith_u y)
#define ARITH_DECL_BI_UI_U(op)     ARITH_FUNCTION_HEADER arith_u \
                                   arith_op_u(op)(arith_u x, int y)
#define ARITH_DECL_BI_UU_I(op)     ARITH_FUNCTION_HEADER int \
                                   arith_op_u(op)(arith_u x, arith_u y)
#define ARITH_DECL_BI_SS_S(op)     ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_s(op)(arith_s x, arith_s y)
#define ARITH_DECL_BI_SI_S(op)     ARITH_FUNCTION_HEADER arith_s \
                                   arith_op_s(op)(arith_s x, int y)
#define ARITH_DECL_BI_SS_I(op)     ARITH_FUNCTION_HEADER int \
                                   arith_op_s(op)(arith_s x, arith_s y)

#ifdef NATIVE_SIGNED

typedef NATIVE_SIGNED arith_s;
typedef NATIVE_UNSIGNED arith_u;

#else

#if SIMUL_NUMBITS > (2 * SIMUL_SUBTYPE_BITS)
#error Native subtype too small for arithmetic simulation.
#endif

#define SIMUL_MSW_WIDTH   (SIMUL_NUMBITS / 2)
#define SIMUL_LSW_WIDTH   ((SIMUL_NUMBITS + 1) / 2)

typedef struct {
	SIMUL_ARITH_SUBTYPE msw, lsw;
} arith_u, arith_s;

#endif

/* functions with the unsigned type */

ARITH_DECL_MONO_S_U(to_u);
ARITH_DECL_MONO_I_U(fromint);
ARITH_DECL_MONO_L_U(fromulong);
ARITH_DECL_MONO_U_I(toint);
ARITH_DECL_MONO_U_L(toulong);

ARITH_DECL_MONO_U_U(neg);
ARITH_DECL_MONO_U_U(not);
ARITH_DECL_MONO_U_I(lnot);
ARITH_DECL_MONO_U_I(lval);

ARITH_DECL_BI_UU_U(plus);
ARITH_DECL_BI_UU_U(minus);
ARITH_DECL_BI_UI_U(lsh);
ARITH_DECL_BI_UI_U(rsh);
ARITH_DECL_BI_UU_I(lt);
ARITH_DECL_BI_UU_I(leq);
ARITH_DECL_BI_UU_I(gt);
ARITH_DECL_BI_UU_I(geq);
ARITH_DECL_BI_UU_I(same);
ARITH_DECL_BI_UU_I(neq);
ARITH_DECL_BI_UU_U(and);
ARITH_DECL_BI_UU_U(xor);
ARITH_DECL_BI_UU_U(or);
ARITH_DECL_BI_UU_U(star);
ARITH_DECL_BI_UU_U(slash);
ARITH_DECL_BI_UU_U(pct);

/* functions with the signed type */

ARITH_DECL_MONO_U_S(to_s);
ARITH_DECL_MONO_I_S(fromint);
ARITH_DECL_MONO_L_S(fromlong);
ARITH_DECL_MONO_S_I(toint);
ARITH_DECL_MONO_S_L(tolong);

ARITH_DECL_MONO_S_S(neg);
ARITH_DECL_MONO_S_S(not);
ARITH_DECL_MONO_S_I(lnot);
ARITH_DECL_MONO_S_I(lval);

ARITH_DECL_BI_SS_S(plus);
ARITH_DECL_BI_SS_S(minus);
ARITH_DECL_BI_SI_S(lsh);
ARITH_DECL_BI_SI_S(rsh);
ARITH_DECL_BI_SS_I(lt);
ARITH_DECL_BI_SS_I(leq);
ARITH_DECL_BI_SS_I(gt);
ARITH_DECL_BI_SS_I(geq);
ARITH_DECL_BI_SS_I(same);
ARITH_DECL_BI_SS_I(neq);
ARITH_DECL_BI_SS_S(and);
ARITH_DECL_BI_SS_S(xor);
ARITH_DECL_BI_SS_S(or);
ARITH_DECL_BI_SS_S(star);
ARITH_DECL_BI_SS_S(slash);
ARITH_DECL_BI_SS_S(pct);

/* conversions from string */
ARITH_DECL_MONO_ST_US(octconst);
ARITH_DECL_MONO_ST_US(hexconst);
ARITH_DECL_MONO_ST_US(decconst);

#endif /* UCPP_INTERNAL_ARITH_H */
