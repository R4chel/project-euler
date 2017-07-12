open Core

let problem_1 n =
  List.fold (List.range 0 n) ~init:0 ~f:(fun sum i ->
      if i % 3 = 0 || i % 5 = 0
      then sum + i
      else sum
    )
;;

let problem_2 () =
  let rec fib_n values n =
    match Int.Map.find values n with
    | Some result -> (values, result)
    | None ->
      let values, n_minus_1 = fib_n values (n - 1) in
      let values, n_minus_2 = fib_n values (n - 2) in
      let result = n_minus_2 + n_minus_1 in
      (Int.Map.add values ~key:n ~data:result, result)
  in
  let values = Int.Map.of_alist_exn [(0, 1); (1, 2)] in
  let result =
    List.fold_until (List.range 0 4_000_000) ~init:(values, 0) ~f:(fun (values, sum) i ->
        let values, fib = fib_n values i in
        let acum =
          if fib % 2 = 0
          then (values, sum + fib)
          else (values, sum)
        in
        if fib > 4_000_000
        then Base.Container_intf.Continue_or_stop.Stop acum
        else Base.Container_intf.Continue_or_stop.Continue acum
      )
  in
  match result with
  | Base.Container_intf.Finished_or_stopped_early.Stopped_early (values, result) -> result
  | Finished (values, result) -> failwith "oops"
;;

let problem_6 n =
  let sum, squares_sum =
    List.fold (List.range 0 (n + 1)) ~init:(0, 0) ~f:(fun (sum, squares_sum) i ->
       ( sum + i, squares_sum + (i * i))  
      )
  in
  (sum * sum) - squares_sum
;;

let problem_8 n =
  let s =
    "73167176531330624919225119674426574742355349194934
      96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450"
  in
  let nums =
    String.to_list s
  |> List.filter_map ~f:Char.get_digit
  in
  let product l =
    List.fold l ~init:1 ~f:(fun acum n -> acum * n)
  in
  List.fold (List.range 0 ((List.length nums)-n+1)) ~init:0 ~f:(fun max i ->
      let result = product (List.slice nums i (i+n)) in
      Int.max max result
  )
;;

let problem_14 () = 
  let rec collatz values n =
    match Int.Map.find values n with
    | Some result -> (values, result)
    | None ->
      let values, result = 
        if n % 2 = 0
        then collatz values (n/2)
        else collatz values (3 * n + 1)
      in
      let values = Int.Map.add values ~key:n ~data:(result + 1) in
      (values, result + 1)
  in
  let values = Int.Map.of_alist_exn [(1, 1)] in
  let values, max_index, max_result = 
    List.fold (List.range 2 1_000_000) ~init:(values, 1, 1) ~f:(fun (values, max_index, max_result) i ->
      let values, result = collatz values i in
      if result > max_result
      then (values, i, result)
      else (values, max_index, max_result)
    )
  in
  max_index
;;

let problem_15 () =
  let n_choose_k n k =
    let numerator =
      List.fold
        (List.range ~stop:`inclusive (n-k+1) n)
        ~init:(Big_int.big_int_of_int 1)
        ~f:(fun prod i -> Big_int.mult_int_big_int i prod)
    in
    let denominator = 
      List.fold
        (List.range ~stop:`inclusive 1 k)       
        ~init:(Big_int.big_int_of_int 1)
        ~f:(fun prod i -> Big_int.mult_int_big_int i prod)
    in
    Big_int.div_big_int numerator denominator
    |> Big_int.int_of_big_int
  in
  n_choose_k 40 20
;;

let () =
  (* let problem_1 = problem_1 1000 in *)
  (* Out_channel.output_string stdout ("Problem 1: " ^ (Int.to_string problem_1)); *)
  (* let problem_2 = problem_2 () in *)
  (* Out_channel.output_string stdout ("Problem 2: " ^ (Int.to_string problem_2)); *)
  (* let problem_6 = problem_6 100 in *)
  (* Out_channel.output_string stdout ("Problem 6: " ^ (Int.to_string problem_6)); *)
  let problem_8 = problem_8 13 in
  Out_channel.output_string stdout ("Problem 8: " ^ (Int.to_string problem_8));
  (* let problem_14 = problem_14 () in *)
  (* Out_channel.output_string stdout ("Problem 14: " ^ (Int.to_string problem_14)); *)
  (* let problem_15 = problem_15 () in *)
  (* Out_channel.output_string stdout ("Problem 15: " ^ (Int.to_string problem_15)); *)
;;
