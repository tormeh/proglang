DEF minus(a:nat, b:nat):nat == sub(a,b)

DEF combi(a:nat, b:nat, c:nat, d:nat):nat == 
	mul(minus(a,b),add(div(b,c),d))

DEF isNotSubst(a:nat, b:nat):bool == lt(a,b)

DEF isDiv(b:nat, c:nat):bool == not(eq(c,0))

DEF MAIN:nat ==
	IF and(not(isNotSubst(9,6)), or(isDiv(6,3), false)) THEN combi(9,6,3,2) ELSE 0 FI