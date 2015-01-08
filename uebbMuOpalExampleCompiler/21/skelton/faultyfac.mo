-- Calculate 1 * 2 * ... * (n-1) * n
-- Expected Out: Syntax error: Expected ')' or ',' at 5.9 but got 0

DEF fac(n:nat):nat ==
IF eq(n 0) THEN 1 ELSE mul(n, fac(sub(n, 1))) FI
DEF MAIN:nat == fac(8)

