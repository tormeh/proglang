-- Calculate 1 * 2 * ... * (n-1) * n
-- Expected Out: Expected Number, True, False, Variable or IF at 0.0 but got EOF [position should be 6.15 or something but no]

DEF fac(n:nat):nat ==
IF eq(n,0) THEN 1 ELSE mul(n, fac(sub(n, 1))) FI
DEF MAIN:nat ==

