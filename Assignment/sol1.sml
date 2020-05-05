
(* 2016025514 Assignment 1 *)

(* Merge Lists *)

fun merge(x1: int list, x2: int list) =
	
	(* base case *)

	if null x1
	then x2

	else if null x2
	then x1

	(* 둘 다 Null인 case도 고려해야하나 했는데, 알아서 처리해주는 듯 *)
	
	else if hd(x1) < hd(x2) 
	then hd(x1) :: merge(tl(x1), x2)

	else hd(x2) :: merge(x1, tl(x2))	


(* Reverse List *)

fun reverse(xs : int list) =

	(*base case*)

	if null xs
	then []
	
	(* use ML built-in function rev *)

	else rev(xs)


(* Sigma Function *)

fun sigma(a : int, b: int, f: int -> int) =

	(* base case*)
	if a = b
	then 
		let val n = b
		in f(n)
		end
	
	(* recursion *)
	else
		let val n = a
		in f(n) + sigma(a+1, b, f)
		end


(* Digits Function *)

fun digits(x: int) = 
	
	(* base case ???? - 한 자리수 되면 recursion 종료 *)
	if x < 10 
	then [x]

	else
		let val arg = digits(x div 10)

		(* arg :: x mod 10을 사용했더니 계속 에러가 발생해서
		구글링을 통해서 list 간의 결합하는 cons 이외의 방식을 찾음
		@ 연산은 list 간에만 성립하는 듯*)

		in arg @ [x mod 10]

		end
	

(* 추가적인 아이디어: 리스트의 숫자 총 합을 구하는
sum_list 함수를helper function으로 정의하면 좋을 듯 *)

fun sum_list(xs : int list) =
	if null xs                        
	then 0                        
	else hd(xs) + sum_list(tl(xs))



(* digital Root *)

fun digitalRoot(n : int) =
	(* 인자로 받은 숫자의 각 자리수 합이 한 자리수가 될때까지 연산 해서 최종 숫자 리턴*)
	
	(* 위에서 정의한 digits 함수 사용 가능 - 과제 명세 *)
	
	(* base case *)
	if n < 10
	then n

	else 
		let val digit_list = digits(n)
		in digitalRoot(sum_list(digit_list))
		end
		

(* additivePersistence *)

fun additivePersistence(n : int) =
	
	(* 최종적으로 출력할 count variable binding *)
	let val cnt = 0

	in
		let
			(* count를 올릴지 말지 결정하는 함수 -> 재귀 대상 *)
			(* 원래는 recurse 함수가 인자를 n만 받는 경우만 생각했는데,
			문제를 해결할 방법이 그렇게 하니까 너무 복잡해지는 것 같아서,
			고민하다가 count 변수를 두번째 인자로 받고, 재귀할 때마다
			두번째 인자의 값을 하나씩 올려주는 방식으로 구현함 *)

			fun recurse(n: int, tmp_cnt: int) =
				if sum_list(digits(n)) < 10 
				then tmp_cnt
				else recurse(sum_list(digits(n)), tmp_cnt + 1)

		in
			if n < 10
			then cnt
			else recurse(n, cnt+1)
		end

	end
		
