(* function call & function as parameter example *)

fun apply(f: int* int -> int, x: int, y: int)=
	f(x, y)

fun plus(x: int, y: int)=
	x + y

