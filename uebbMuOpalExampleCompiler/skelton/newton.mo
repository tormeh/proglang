-- Square root by Newtons method
-- Main = 2
DEF MAIN:nat == sqrt(4)
DEF sqrt(x:nat):nat == sqrt2(div(x,2),x)
DEF sqrt2(guess:nat, x:nat):nat == IF good(guess,x) THEN guess ELSE sqrt2(improve(guess,x),x) FI
DEF good(guess:nat, x:nat):bool == eq(improve(guess,x),guess)
DEF improve(guess:nat, x:nat):nat == average(guess,div(x,guess))
DEF average(x:nat, y:nat):nat == div(add(x,y),2)
