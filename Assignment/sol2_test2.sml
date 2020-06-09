(* 2016025514 Assignment 2 *)

(* #1. Simple Eval *)

(* simple porpositional logics *)

datatype expr = NUM of int              (* NUM(10), NUM 10 이런 식으로 사용 *)
                | PLUS of expr * expr   (* PLUS( NUM 2, NUM 3 ) 이런 식으로 사용 *)
                | MINUS of expr * expr

datatype formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr


(* write eval function that takes a formula value *)
(* and returns the Boolean value fo the formula *)


(* eval : formula -> bool *)
fun eval(x : formula) =
        case x of
                TRUE => true
                | FALSE => false

                | NOT x_tmp => if eval(x_tmp) = true
                           then false
                           else true

                | ANDALSO(x_tmp, y_tmp) => if eval(x_tmp) = true andalso eval(y_tmp) = true
                                           then true
                                           else false

                | ORELSE(x_tmp, y_tmp) => if eval(x_tmp) = true orelse eval(y_tmp) = true
                                          then true
                                          else false

                (* IMPLY 는 첫번째 인자가 true, 두번째 인자가 false인 경우는 false *)
                (* 그 외는 모두 true *)
                | IMPLY(x_tmp, y_tmp) => if eval(x_tmp) = true andalso eval(y_tmp) = false
                                         then false
                                         else true

                (* expr1이 더 작으면 true, expr2가 더 작으면 false *)
                (* 테스트 예 : eval ( LESS ( MINUS ( NUM 10, NUM 2 ), NUM 10 ) ); *)
                | LESS(expr1, expr2) => let fun eval2(x2 : expr) =

                                                        case x2 of
                                                                NUM num => num
                                                                | PLUS(x_tmp2, y_tmp2) => eval2(x_tmp2) + eval2(y_tmp2)
                                                                | MINUS(x_tmp2, y_tmp2) => eval2(x_tmp2) - eval2(y_tmp2)

                                        in
                                                if eval2(expr1) < eval2(expr2)
                                                then true
                                                else false

                                        end


(* ------------------------------------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------------------------------------- *)


(* #2. Check MetroMap *)

(* metropolitan area datatype *)

type name = string

datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro



(* checkMetro 함수는 주어진 metro가 제대로 정의된 경우에 연산함 *)
(* metro의 제대로된 정의는, AREA 안에서 STATION 이름이 나와야함 *)
(* 그렇다고 해서 AREA와 STATION이 바로 연결될 필요는 없음 *)
(* ex : AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))) 도 올바른 definition *)

(* checkMetro: metro -> bool *)
(* 제대로 definintion -> true, 제대로 definition X -> false *)

fun checkMetro(m : metro) =
        (* check 함수 :  m의 definition 올바른지 체크할 떄 사용 *)

        let fun check(line) =

                (* 인자가 STATION, AREA, CONNECT 일때에 따라서 취해야할 액션 정의 *)
                case line of

                        STATION line_name => (line_name :: [])

                        | AREA(line_name, line_metro) => let val check_tmp = check(line_metro)
                                                        (* check2 함수 : AREA의 metro에 대해서 체크 재귀 *)
                                                         in
                                                                let fun check2(line2) =

                                                                        (* base case *)
                                                                        if null line2
                                                                        then []

                                                                        else
                                                                                (* head에 name 들어가 있음, tail로만 재귀 *)
                                                                                (* head에 name 없으면, head 붙여서 재귀 *)
                                                                                let val check_tmp2 = check2(tl line2)
                                                                                in
                                                                                        if hd line2 = line_name
                                                                                        then check_tmp2
                                                                                        else (hd line2) :: check_tmp2
                                                                                end

                                                                in check2(check_tmp)
                                                                end

                                                        end

                        (* 첫번째 인자랑 두번째 인자 각각에 대해서 체크하고 @ 처리*)
                        | CONNECT(line_metro1, line_metro2) => (check line_metro1 @ check line_metro2)


        in
                (* checkMetro 함수 인자 (진짜 체크 대상) 의 정의가 올바로 됐는지 확인하는 부분 *)
                let val result = check(m)

                (* 위의 recursion 다 돌고 안에 남아 있음 잘못된 정의, 아니면 올바른 정의 *)
                in if null result then true else false

                end

        end


(* ------------------------------------------------------------------------------------------------------------------------- *)

(* #2 test *)

(* true test ex *)

(* checkMetro(AREA("a", STATION "a")) *)
(* checkMetro(AREA("a", AREA("a", STATION "a"))) *)
(* checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) *)
(* checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) *)

(* false test ex *)

(* checkMetro(AREA("a", STATION "b")) *)
(* checkMetro(AREA("a", AREA("a", STATION "b"))) *)
(* checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) *)
(* checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) *)



(* ------------------------------------------------------------------------------------------------------------------------- *)

(* #3 Lazy List *)

(* long or even infinite list datatype *)
(* lazyList : polymorphic - 다형성 *)

datatype 'a lazyList = nullList
                      | cons of 'a * (unit -> 'a lazyList)

(* function of zero argument 호출시, list의 rest를 표현하는 lazy list 를 반환함 *)


(* ------------------------------------------------------------------------------------------------------------------------- *)


(* seq(first, last) 함수 : 두 개의 정수를 인자로 받아 들이고, *)
(*                         첫번째 인자부터 시작해서 두번째 인자까지의 정수로 이뤄진 lazy list 반환 *)

fun seq(first : int, last : int) =

        (* base case *)
        if first = last
        then cons(first, fn() => nullList)

        (* recursion 사용해서 list 만들어가기 *)
        else cons(first, fn() => seq(first + 1, last) )


(* ------------------------------------------------------------------------------------------------------------------------- *)


(* infSeq(first) 함수 : 하나의 정수를 인자로 받아 들이고, *)
(*                      인자에서부터 무한한 정수로 이뤄진 lazy list 반환 *)

fun infSeq(first : int) =
        cons(first, fn() => infSeq(first +1))


(* ------------------------------------------------------------------------------------------------------------------------- *)


(* firstN(lazyListVal, n) 함수 : lazyList랑 정수 한 개를 인자로 받아들이고, *)
(*                               lazyList 초반의 n개 변수 (n : 인자로 들어온 정수)를 지닌 *)
(*                               ordinary SML list를 반환한다. *)
(*                               만일 n보다 적은 수의 요소가 lazyList에 들어 있으면, 모든 요소 반환한다. *)

fun firstN(lazyListVal, n) =
        case lazyListVal of
                nullList => []
                | cons(hd, f) => if n = 1
                                 then hd::[]

                                 (* recursion 사용 *)
                                 else hd::firstN(f(), n-1)

(* ------------------------------------------------------------------------------------------------------------------------- *)


(* Nth(lazyListVal, n) 함수 : lazyList랑 정수 한 개를 인자로 받아들이고, *)
(*                            n번째 value를 나타내는 option을 출력함 - 계수는 1부터!!! *)
(*                            만일 n보다 적은 수의 요소가 lazyList에 들어있으면, NON 출력 *)

fun Nth(lazyListVal, n) =
        case (lazyListVal, n) of

                (* firstN 함수와 다르게 option을 반환해야 함!! *)
                (nullList, _) => NONE

                (* lazyList에 한 개 있는 경우 *)
                | (cons(hd, f), 1) => SOME hd

                (* recursion 사용 *)
                | (cons(hd, f), n) => Nth(f(), n-1)



(* ------------------------------------------------------------------------------------------------------------------------- *)

(* filterMultiples(lazyListVal, n) 함수 : n과 n의 배수를 첫번째 인자에서 제거한 새로운 lazy list를 반환함*)

fun filterMultiples(lazyListVal, n) =
        case lazyListVal of
                nullList => nullList
                |cons(hd, f) => let val tmp = f()
                                in
                                        (* hd가 n의 배수인 경우 *)
                                        if hd mod n = 0
                                        (* hd 제외하고 recursion *)
                                        then filterMultiples(tmp, n)
                                        (* hd 연결해서 recursion *)
                                        else cons(hd, fn() => filterMultiples(tmp, n))
                                end

