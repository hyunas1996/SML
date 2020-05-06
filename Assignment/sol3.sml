
(* 2016025514 Assignment 3 *)

(* Datatypes *)

(* following type definition is used for below functions. *)


datatype pattern = Wildcard
		  | Variable of string
		  | UnitP
		  | ConstP of int
		  | TupleP of pattern list
		  | ConstructorP of string * pattern 

datatype valu = Const of int 
		| Unit
		| Tuple of valu list
		| Constructor of string * valu


(* p and v match : produces a list of stirng * valu pairs *)

(* match rule *)

(* Wildcard : matches everything & produces empty list of binding *)
(* Variable s : matches any value v and produces the one-element list holding (s,v) *)
(* UnitP : matches only Unit (이건 valu) and produces the empty list of bindings *)
(* ConstP n : (n : integer) matches only Const n and produces empty list of bindings *)
(* TupleP ps : matches a value of the form Tuple vs *)
(* 	       ps랑 vs 길이가 같으면 ps의 ith element는 vs의 ith element 에 match *)
(* ConstructorP(s1, p) : matches Constructor(s2, v) *)
(* 		 	s1이랑 s2가 같은 string이면, p matches v *)
(*			list of bindings 는 list from nested pattern match *)
(*			constructor의 이름 = s1 (=s2) *)





(* #1 check pat function *)
(* pattern의 모든 variable들이 서로 구별가능-distinct- 하다면 true를 반환하는 함수 *)
(* constructor들의 이름은 서로 관게 없음 *)

(* hint : two helper functions - 1) pattern을 받아서 variable에 사용하는 모든 string list 출력 *)
(*				 2) string list 받아서 반복이 있는지 확인*)

fun check_pat pattern =
	let 
		(* helper function 1 *)
		fun variable_list pattern =
			case pattern of 
				Variable s => [s]
				| TupleP ps => foldl(fn (pattern, vs) => (variable_list pattern) @ vs) [] ps
				| ConstructorP(_, pattern) => variable_list pattern
				| _ => []

		
		(* helper function 2 *)
		fun check_repeat v_list =
			case v_list of
				[] => true
				| (hd :: v_list2) => if List.exists (fn s => s = hd) v_list2 
						     then false 
						     else check_repeat v_list2

	in
		check_repeat(variable_list pattern)
	end





(* #2 match function *)
(* 인자 : valu * pattern / 반환 : (string * valu) list option *)

(* NONE : pattern doesn't match *)
(* SOME lst : lst - list of bindings if it does match *)

(* value matches but pattern has no patterns of form Variable s -> SOME [] 출력 *)

(* hint : 7 branches & tuple branch -> List.filter  &  ListPair.zip 사용 *)

fun match (valu, pattern) =
	case (valu, pattern) of
		(_, Wildcard) => SOME []
		| (_, Variable s) => SOME [(s, valu)]
		| (Unit, UnitP) => SOME []


		| (Const n, ConstP np) => if n = np 
					  then SOME [] 
					  else NONE


		| (Tuple vs, TupleP ps) => if List.length vs = List.length ps
					   then let 
							fun check_match(x) =
								case x of
									[] => true
									| x::xs => if match x = NONE
										   then false
										   else check_match(xs)
							
							fun appending_match(x) =
								case x of
									[] => []
									| x::xs => if match x = NONE
										   then appending_match(xs)
										   else valOf(match x) @ appending_match(xs)

						in 
							if check_match(ListPair.zip(vs, ps))
							then SOME(appending_match(ListPair.zip(vs, ps)))
							else NONE
						
						end

					   else NONE				


		| (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
							       then match(v, p)
							       else NONE
		|_ => NONE






(* #3 Rock Paper Scissors tournament game *)


(* datatypes for #3 *)

type name = string

datatype RSP = ROCK | SCISSORS | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref) 
		      | MATCH of tournament * tournament



(* RSP strategy : RSP 나열, 호출 시 RSP pair랑 함수를 출력하고, *)
(* 		  이 함수가 stream의 다음 요소 (가위, 바위 보) 를 결정함 *)

(* example RSP strategies *)

fun onlyOne (one: RSP) = Cons(one, fn() => onlyOne(one))

fun alterTwo (one: RSP, two: RSP) = Cons(one, fn() => alterTwo(two, one))

fun alterThree(one: RSP, two: RSP, three: RSP) = Cons(one, fn() => alterThree(two, three, one))


val r = onlyOne ROCK
val s = onlyOne SCISSORS
val p = onlyOne PAPER

val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)

val srp = alterThree(SCISSORS, ROCK, PAPER)


fun next (strategyRef) =
        let val Cons(rsp, func) = !strategyRef in
                strategyRef := func();
                rsp
        end



(*  가위바위보 토너먼트에서 누가 이겼는지 확인하는 함수 whosWinner *)

(* for example~ *)
(* val winner = whosWinner (MATCH(PLAYER ("s", ref s) , MATCH(PLAYER ("rp", ref rp), PLAYER ("r", ref r)))); *)


(* tournament에서 누가 이겼는지 반환함 *)
(* 즉, PLAYER("rp" ref rp) 가 출력되야함 - 위의 예제에서는 *)

(* 위 예제에서 PLAYER들을 차례대로 1, 2, 3이라고 하면 *)
(* 첫번째 매치 : 2 (r) vs 3 (r) => 비김 *)
(* 첫번째 매치 : 2 (p) vs 3 (r) => 2 이김 *)
(* 두번째 매치 : 1 (s) vs 2 (r) => 2 이김 *)

(* use pattern matching to implement! *)

fun whosWinner(t) =
        let
                fun firstPlayerWin(str1, str2) =

                        let
                                val next_str1 = next(str1)
                                val next_str2 = next(str2)

                        in
                                case(next_str1, next_str2) of

                                (SCISSORS, SCISSORS) => firstPlayerWin(str1, str2) (* 비긴 경우 다시 *)
                                |(SCISSORS, ROCK) => false
                                |(SCISSORS, PAPER) => true

                                |(ROCK, SCISSORS) => true
                                |(ROCK, ROCK) => firstPlayerWin(str1, str2) (* 비긴 경우 다시 *)
                                |(ROCK, PAPER) => false

                                |(PAPER, SCISSORS) => false
                                |(PAPER, ROCK) => true
                                |(PAPER, PAPER) => firstPlayerWin(str1, str2) (* 비긴 경우 다시 *)

                        end

        in
                case t of

                        PLAYER p => PLAYER p

                        | MATCH(PLAYER(name1, ref1), PLAYER(name2, ref2)) =>

                                if firstPlayerWin(ref1, ref2) (*true면 첫번째 선수가 이김*)
                                then PLAYER(name1, ref1)
                                else PLAYER(name2, ref2)

                        | MATCH(tourna1, tourna2) =>
                                whosWinner(MATCH(whosWinner(tourna1), whosWinner(tourna2)))

        end


