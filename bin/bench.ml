(* Benchmark driver comparing Board.S implementations.

   Usage (from the repo root; use the release profile, see below):

     dune exec --profile release bin/bench.exe -- Board_basic Board_bit
     dune exec --profile release bin/bench.exe -- -depth 6 -only minimax
     dune exec --profile release bin/bench.exe -- -help

   With no module names, every implementation in [Impls.list] is benchmarked. The
   first module listed is the baseline for the "vs" column.

   Every module runs the same workloads on the same boards (fixed seed). Each
   timing is an ordinary-least-squares fit of time against number of runs
   (Bechamel), reported with its r²; an r² well below 0.99 means the
   measurement was noisy and is worth re-running.

   Release vs dev: the dev profile keeps [assert]s, so [check_inv] runs inside
   [move] for some implementations and [Minimax.heuristic] asserts on every
   leaf. That measures the invariant checks as much as the boards; the release
   profile passes -noassert (see the root dune file). *)

open Filler
open Bechamel
open Toolkit

(* ------------------------------------------------------------------------ *)
(* Implementations: add new ones here or in [Impls.list]                    *)
(* ------------------------------------------------------------------------ *)

let impls = Impls.list

(* ------------------------------------------------------------------------ *)
(* Shared workload data (identical for every implementation)                *)
(* ------------------------------------------------------------------------ *)

let n_boards = 256 (* power of two, so cycling is a mask *)

let squares, playout_moves =
  Random.init 0xBE7C4;
  let squares = Array.init n_boards (fun _ -> IO.random_squares ()) in
  (* For each board, a fixed sequence of indices into the 4 valid moves *)
  let moves =
    Array.init n_boards (fun _ -> Array.init 64 (fun _ -> Random.int 4))
  in
  (squares, moves)

let copy_squares = Array.map Array.copy

(* ------------------------------------------------------------------------ *)
(* Workloads                                                                *)
(* ------------------------------------------------------------------------ *)

let workload_names =
  [
    "of_squares";
    "get (56 cells)";
    "get_corner (both)";
    "region_size (both)";
    "valid_moves";
    "move (4 valid)";
    "playout";
    "minimax";
  ]

module Workloads (M : Board.S) = struct
  module BM = Board.Make (M)
  module MM = Minimax.Make (M)

  (* Separate copies per implementation: some implementations alias the
     squares they are built from. *)
  let boards = Array.map (fun sq -> M.of_squares (copy_squares sq)) squares
  let sq_copies = Array.map copy_squares squares

  (* Each benchmark closure walks through the boards so no single board's
     shape dominates, and branch predictors can't memorize one board. *)
  let cycler arr =
    let i = ref 0 in
    fun () ->
      let x = arr.(!i land (n_boards - 1)) in
      incr i;
      x

  let cells =
    List.concat_map
      (fun y -> List.init Constants.width (fun x -> (y, x)))
      (List.init Constants.height Fun.id)

  let play b moves =
    let rec go b p k =
      let us = M.region_size b Us and opp = M.region_size b Opp in
      if Board.is_done us opp || k = Array.length moves then b
      else
        let c = List.nth (BM.valid_moves b) moves.(k) in
        go (M.move b c p) (Board.other_player p) (k + 1)
    in
    go b Board.Us 0

  let fn ~depth = function
    | "of_squares" ->
        let next = cycler sq_copies in
        fun () -> ignore (Sys.opaque_identity (M.of_squares (next ())))
    | "get (56 cells)" ->
        let next = cycler boards in
        fun () ->
          let b = next () in
          List.iter (fun c -> ignore (Sys.opaque_identity (M.get b c))) cells
    | "get_corner (both)" ->
        let next = cycler boards in
        fun () ->
          let b = next () in
          ignore (Sys.opaque_identity (M.get_corner b Us));
          ignore (Sys.opaque_identity (M.get_corner b Opp))
    | "region_size (both)" ->
        let next = cycler boards in
        fun () ->
          let b = next () in
          ignore
            (Sys.opaque_identity (M.region_size b Us + M.region_size b Opp))
    | "valid_moves" ->
        let next = cycler boards in
        fun () -> ignore (Sys.opaque_identity (BM.valid_moves (next ())))
    | "move (4 valid)" ->
        let next = cycler boards in
        fun () ->
          let b = next () in
          List.iter
            (fun c -> ignore (Sys.opaque_identity (M.move b c Us)))
            (BM.valid_moves b)
    | "playout" ->
        let i = ref 0 in
        fun () ->
          let k = !i land (n_boards - 1) in
          incr i;
          ignore (Sys.opaque_identity (play boards.(k) playout_moves.(k)))
    | "minimax" ->
        let next = cycler boards in
        fun () ->
          ignore (Sys.opaque_identity (MM.minimax ~max_depth:depth (next ())))
    | w -> invalid_arg ("unknown workload " ^ w)
end

(* ------------------------------------------------------------------------ *)
(* Measurement                                                              *)
(* ------------------------------------------------------------------------ *)

type result = { ns : float; words : float; r2 : float }

let instances = Instance.[ monotonic_clock ]
let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]

(* Allocation is measured directly rather than with Bechamel's
   [minor_allocated], which reads [Gc.quick_stat]: on OCaml 5 that counter only
   advances at minor collections, so small workloads report 0. *)
let words_per_run f =
  let n = 1000 in
  f ();
  let before = Gc.minor_words () in
  for _ = 1 to n do
    f ()
  done;
  (Gc.minor_words () -. before) /. float_of_int n

let measure cfg name f =
  let test = Test.make ~name (Staged.stage f) in
  let raw = Benchmark.run cfg instances (List.hd (Test.elements test)) in
  let fit = Analyze.one ols Instance.monotonic_clock raw in
  {
    ns = (match Analyze.OLS.estimates fit with Some [ x ] -> x | _ -> nan);
    words = words_per_run f;
    r2 = Option.value ~default:nan (Analyze.OLS.r_square fit);
  }

let pp_time ns =
  if Float.is_nan ns then "n/a"
  else if ns < 1e3 then Printf.sprintf "%.1f ns" ns
  else if ns < 1e6 then Printf.sprintf "%.2f us" (ns /. 1e3)
  else Printf.sprintf "%.2f ms" (ns /. 1e6)

(* ------------------------------------------------------------------------ *)
(* CLI                                                                      *)
(* ------------------------------------------------------------------------ *)

let () =
  let depth = ref 4
  and quota = ref 0.5
  and only = ref []
  and names = ref []
  and show_alloc = ref true in

  let usage =
    "bench.exe [options] [MODULE ...]\n\nModules: "
    ^ String.concat ", " (List.map fst impls)
    ^ "\nWorkloads: "
    ^ String.concat ", " workload_names
    ^ "\n\nOptions:"
  in
  Arg.parse
    [
      ("-depth", Arg.Set_int depth, "N  minimax search depth (default 4)");
      ("-quota", Arg.Set_float quota, "S  seconds per measurement (default 0.5)");
      ( "-only",
        Arg.String (fun s -> only := String.split_on_char ',' s),
        "W1,W2  only run workloads whose name starts with one of these" );
      ( "-show-alloc",
        Arg.Bool (fun b -> show_alloc := b),
        "Whether to print the # of allocations (default true)" );
    ]
    (fun s -> names := s :: !names)
    usage;
  let selected =
    match List.rev !names with
    | [] -> impls
    | ns ->
        List.map
          (fun n ->
            match
              List.find_opt
                (fun (m, _) ->
                  String.lowercase_ascii m = String.lowercase_ascii n)
                impls
            with
            | Some impl -> impl
            | None ->
                Printf.eprintf "Unknown module %s. Known: %s\n" n
                  (String.concat ", " (List.map fst impls));
                exit 2)
          ns
  in
  let workloads =
    match !only with
    | [] -> workload_names
    | prefixes ->
        List.filter
          (fun w ->
            List.exists (fun p -> String.starts_with ~prefix:p w) prefixes)
          workload_names
  in
  (* Not [assert false]: that form is kept even under -noassert *)
  let asserts_on =
    let on = ref false in
    assert (
      on := true;
      true);
    !on
  in
  if asserts_on then
    print_endline
      "WARNING: assertions are enabled (dev profile); timings include \
       invariant checks. Use `dune exec --profile release ...`.\n";

  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second !quota) ~kde:None
      ~stabilize:false ()
  in
  (* Instantiate each implementation's workloads once *)
  let runners =
    List.map
      (fun (name, (module M : Board.S)) ->
        let module W = Workloads (M) in
        (name, W.fn ~depth:!depth))
      selected
  in
  let col = 26 in
  Printf.printf "%-22s" "workload";
  List.iteri
    (fun i (n, _) ->
      Printf.printf "%-*s" col (if i = 0 then n ^ " (base)" else n))
    selected;
  print_newline ();
  List.iter
    (fun w ->
      let label =
        if w = "minimax" then Printf.sprintf "minimax d=%d" !depth else w
      in
      Printf.printf "%-22s%!" label;
      (* Measure all modules for this workload back to back, so slow drift in
         machine load affects them similarly *)
      let results =
        List.map (fun (n, fn) -> measure cfg (w ^ "/" ^ n) (fn w)) runners
      in
      let base = (List.hd results).ns in
      List.iteri
        (fun i r ->
          let vs =
            if i = 0 then "" else Printf.sprintf " %.2fx" (base /. r.ns)
          in
          Printf.printf "%-*s" col (pp_time r.ns ^ vs))
        results;
      print_newline ();
      if !show_alloc then (
        Printf.printf "%-22s" "  alloc (words), r²";
        List.iter
          (fun r ->
            Printf.printf "%-*s" col (Printf.sprintf "%.0f, %.3f" r.words r.r2))
          results;
        print_newline ()))
    workloads;
  print_endline
    "\n\
     \"Nx\" = speedup over the base module (higher is faster). Boards are \
     uniform random (IO.random_squares), fixed seed."
