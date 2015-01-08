-- Calculate 1 * 2 * ... * (n-1) * n
-- Expected Out: List(Def(Function_def(Ide(fac),List(Param(Ide(n),Type(nat))),Type(nat)),ITE(Function(Ide(eq),List(Ide(n), Number(0))),Number(1),Function(Ide(mul),List(Ide(n), Function(Ide(fac),List(Function(Ide(sub),List(Ide(n), Number(1))))))))), Def(MAIN(Type(nat)),Function(Ide(fac),List(Number(8)))))

DEF fac(n:nat):nat ==
IF eq(n,0) THEN 1 ELSE mul(n, fac(sub(n, 1))) FI
DEF MAIN:nat == fac(8)

