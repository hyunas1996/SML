
fun sum_pair_lists(xs : (int * int) list) =
	if null xs
	then 0
	else #1 (hd(xs)) + #2 (hd(xs)) + sum_pair_lists(tl(xs))

fun firsts(xs: (int*int) list) =
	if null xs
	then []
	else #1(hd(xs)) :: firsts(tl(xs))

fun seconds(xs: (int*int) list) =
	if null xs
	then []
	else #2(hd(xs)) :: seconds(tl(xs))

fun sum_list(xs: int list) =
	if null xs
	then 0
	else hd(xs) + sum_list(tl(xs))

fun sum_pair_lists2(xs: (int*int) list) =
	sum_list(firsts(xs)) + sum_list(seconds(xs))
