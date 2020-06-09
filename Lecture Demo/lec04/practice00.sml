fun zip3 lists = 
	case lists of 
		([], [], []) => []
		| (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3) :: zip3(tl1, tl2, tl3)

fun unzip3 triples = 
	case triples of
		[] => ([], [], [])
		| (a, b, c) :: tail => let val (l1, l2, l3) = unzip3(tail)
				       in (a::l1, b::l2, c::l3)
				       end
