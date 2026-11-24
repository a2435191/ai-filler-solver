let () = Random.init 31
let reference_impl = Int.popcount
let candidate_impl = Filler.Intrinsics.popcount

let test_case n : unit Alcotest.test_case =
  let name = Printf.sprintf "0x%16x" n in
  let test_fn () =
    Alcotest.(check int "" (reference_impl n) (candidate_impl n))
  in
  (name, `Always, test_fn)

let random_test_case () : unit Alcotest.test_case =
  let n = Int64.to_int (Random.bits64 ()) in
  test_case n

let random_test_cases number = List.init number (fun _ -> random_test_case ())
let edge_cases = List.map test_case [ 0; 1; -1; Int.max_int; Int.min_int ]
let x = Printf.fprintf

let () =
  Alcotest.run "Filler.Intrinsics"
    [ ("random test cases", random_test_cases 10); ("edge cases", edge_cases) ]
