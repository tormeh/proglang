-- Expected Out: List(Def(Function_def(Ide(fib),List(Param(Ide(n),Type(nat))),Type(nat)),ITE(Function(Ide(eq),List(Ide(n), Number(0))),Number(1),ITE(Function(Ide(eq),List(Ide(n), Number(1))),Number(1),Function(Ide(add),List(Function(Ide(fib),List(Function(Ide(sub),List(Ide(n), Number(1))))), Function(Ide(fib),List(Function(Ide(sub),List(Ide(n), Number(2)))))))))), Def(MAIN(Type(nat)),Function(Ide(fib),List(Number(8)))))

DEF fib(n:nat):nat ==
IF eq(n,0) THEN 1 ELSE IF eq(n,1) THEN 1 ELSE add(fib(sub(n,1)), fib(sub(n,2))) FI FI
DEF MAIN:nat == fib(8)
