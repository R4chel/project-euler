open Core

let is_palindrome s =
  String.equal s (String.rev s)
;;

let rec is_equal l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> is_equal tl1 tl2  
  | _ -> false
;; 

let is_palindrome l =
  is_equal l (List.rev l)
;;

let is_binary_palindrome n =
  let rec to_binary n =
    n % 2 :: (to_binary (n / 2))
  in
  let binary = to_binary n in
  is_palindrome binary
;;

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)
;;

let is_pythagorean_triplet x y z =
  let a = Int.min x y in
  let b = Int.min (Int.max x y) z in
  let c = Int.max (Int.max x y) z in
  a * a + b * b = c * c 

let print_int n =
  Out_channel.output_string stdout (Int.to_string n);
  Out_channel.newline stdout;
;;
