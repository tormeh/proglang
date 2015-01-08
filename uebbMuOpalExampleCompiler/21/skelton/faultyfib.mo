-- Expected Out: Expected Then at 4.35 but got 1

DEF fib(n:nat):nat ==
IF eq(n,0) THEN 1 ELSE IF eq(n,1) 1 THEN ELSE add(fib(sub(n,1)), fib(sub(n,2))) FI FI
DEF MAIN:nat == fib(8)
