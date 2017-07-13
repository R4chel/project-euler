open Core

module CS = Base.Container_intf.Continue_or_stop
module FS = Base.Container_intf.Finished_or_stopped_early

let next_largest_prime primes =
  match Set.max_elt primes with
  | None -> 2
  | Some 2 -> 3
  | Some p ->
    let result = 
      List.fold_until (List.range ~stride:2 (p+2) (2*p)) ~init:primes ~f:(fun primes n ->
        match Set.find primes ~f:(fun p -> n % p = 0) with
        | Some factor -> CS.Continue primes
        | None -> CS.Stop n
     )
    in
    match result with
    | FS.Finished primes -> failwith "disproved betrand's postulate. oops"
    | FS.Stopped_early n -> n
;;

let next_prime primes =
  match Set.max_elt primes with
  | None -> Int.Set.add primes 2
  | Some 2 -> Int.Set.add primes 3
  | Some p ->
    let result = 
      List.fold_until (List.range ~stride:2 (p+2) (2*p)) ~init:primes ~f:(fun primes n ->
        match Set.find primes ~f:(fun p -> n % p = 0) with
        | Some factor -> CS.Continue primes
        | None -> CS.Stop (Int.Set.add primes n)
     )
    in
    match result with
    | FS.Finished primes -> failwith "disproved betrand's postulate. oops"
    | FS.Stopped_early primes -> primes
;;

let primes_less_then n =
  List.fold (List.range ~stride:2 3 n) ~init:(Int.Set.of_list [2]) ~f:(fun primes n -> 
    match Set.find primes ~f:(fun p -> n % p = 0) with
    | Some factor -> primes
    | None -> (Int.Set.add primes n)
  )
;;

let nth_prime n =
  let primes =
    List.fold (List.range 0 n) ~init:Int.Set.empty ~f:(fun primes i ->
        next_prime primes 
      )
  in
  assert (Set.length primes = n);
  Set.max_elt_exn primes
;;

let rec largest_prime_factor primes n =
  match Set.find primes ~f:(fun p -> n % p = 0) with
  | Some factor ->
    Int.max factor (largest_prime_factor primes (Int.(/) n factor))
  | None ->
    if n = 1 then 1 else largest_prime_factor (next_prime primes) n
;;

let sum_less_than n =
  let primes = primes_less_then n in
  Set.fold primes ~init:0 ~f:Int.(+)
;;

let drop_digit_right n = n / 10

let int_to_digits n = 
  Int.to_string n
  |> String.to_list
  |> List.map ~f:Char.get_digit_exn
;;

let rec primes_at_least primes n =
  match Set.max_elt primes with
  | Some p when p >= n -> primes
  | Some _ | None -> primes_at_least (next_prime primes) n
;;

let is_truncatable primes n =
  (* let primes = primes_at_least n in *)
  let digits = int_to_digits n in
  let left =
    List.fold_until digits ~init:0 ~f:(fun n digit ->
      let new_num = n * 10 + digit in
      if Set.mem primes new_num
      then CS.Continue new_num
      else CS.Stop new_num
      )
    |> function
    | FS.Stopped_early _ -> false
    | FS.Finished _ -> true
  in
  let right, _, _ =
    List.fold_right digits ~init:(true, 0, 0) ~f:(fun digit (b, index, n) ->
        if b
        then (
          let new_number = n + digit * (Int.pow 10 index) in
          (Set.mem primes new_number, index + 1, new_number)
        )
        else (b, index, n)
      )
  in
  n > 10 && left && right
;;

let rec find_next_truncatable_prime primes =
  let next_prime = next_largest_prime primes in
  let primes = Set.add primes next_prime in
  if is_truncatable primes next_prime
  then primes, next_prime
  else find_next_truncatable_prime primes
;;

let find_truncatable_primes () =
  let truncatable_primes, primes = 
    List.fold (List.range 0 10) ~init:([], Int.Set.empty) ~f:(fun (acum, primes) i ->
        Out_channel.output_string stdout (Int.to_string i);
        Out_channel.newline stdout;
        let primes, next_prime = find_next_truncatable_prime primes in
        Out_channel.output_string stdout (Int.to_string next_prime);
        Out_channel.newline stdout;
        next_prime :: acum, primes
    )
  in
  List.fold truncatable_primes ~init:0 ~f:Int.(+)
;;

let find_truncatable_primes_v2 () =
  let primes = primes_less_then 1_000_000 in
  let truncatable_primes =
    Set.filter primes ~f:(is_truncatable primes)
  in
  Set.fold truncatable_primes ~init:0 ~f:(fun sum i ->
      Out_channel.output_string stdout (Int.to_string i);
      Out_channel.newline stdout;
      sum + i
  )
;;

