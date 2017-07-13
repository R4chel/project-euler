open Core

let is_palindrome s =
  String.equal s (String.rev s)
;;

let rec is_eqaul l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> is_equal tl1 tl2  
  | _ -> false
;; 

let is_palindrome l =
  is_eqaul l (List.rev l)
;;

let is_binary_palindrome n =
  let rec to_binary n =
    n % 2 :: (to_binary (n / 2))
  in
  let binary = to_binary n in
  is_palindrome binary
;;
