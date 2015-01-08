-- Calculate 1 * 2 * ... * (n-1) * n
-- Expected Out: Syntax error: Expected Variable or ',' at 4.9 but got nat

DEF fac(nat:n):nat ==
IF eq(n,0) THEN 1 ELSE mul(n, fac(sub(n, 1))) FI
DEF MAIN:nat == fac(8)

