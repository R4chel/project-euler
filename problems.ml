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

let () =
  (* let problem_1 = problem_1 1000 in *)
  (* Out_channel.output_string stdout ("Problem 1: " ^ (Int.to_string problem_1)); *)
  let problem_2 = problem_2 () in
  Out_channel.output_string stdout ("Problem 2: " ^ (Int.to_string problem_2));
