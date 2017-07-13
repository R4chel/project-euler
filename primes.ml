open Core

module CS = Base.Container_intf.Continue_or_stop
module FS = Base.Container_intf.Finished_or_stopped_early

let next_prime primes =
  match Set.max_elt primes with
  | None -> Int.Set.add primes 2
  | Some p ->
    let result = 
      List.fold_until (List.range (p+1) (2*p)) ~init:primes ~f:(fun primes n ->
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
  List.fold (List.range 2 n) ~init:(Int.Set.empty) ~f:(fun primes n -> 
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
