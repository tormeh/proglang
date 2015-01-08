DEF MAIN:bool ==
	IF and(true, true) THEN 
	  IF and(false, false) THEN true ELSE and(true, false) FI
	FI