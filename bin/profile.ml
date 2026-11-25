(* Sampling profiler for Board.S implementations, producing folded stacks
   for a flamegraph.

   Usage (release profile, so you profile what you benchmark):

     dune exec --profile release bin/profile.exe -- Board_bit_with_cache \
       -workload minimax -depth 8 -seconds 10 -o profile.folded
     flamegraph.pl profile.folded > profile.svg

   flamegraph.pl is Brendan Gregg's script from
   https://github.com/brendangregg/FlameGraph (inferno-flamegraph also reads
   the same format). Options: -lines adds file:line to each frame,
   -mark-inlined tags frames flambda inlined, -keep-recursion keeps one frame
   per level of recursive calls instead of merging them.

   How it works: a CPU-time timer (setitimer ITIMER_PROF) fires every
   [-interval] seconds; the OCaml signal handler records the current call
   stack with [Printexc.get_callstack]. Frames are named from OCaml's own
   debug info (the same tables that make exception backtraces work), so they
   carry module, function, file and line, including frames that flambda
   inlined. That works on macOS too, where the linker drops OCaml's DWARF.

   Caveats:
   - OCaml runs signal handlers at its next poll point (allocations, function
     prologues, loop back-edges), so samples land on those points rather than
     on arbitrary instructions. Attribution to functions is reliable; exact
     lines inside a tight loop less so.
   - Time spent inside C code (e.g. the popcount stub) or the GC is charged to
     the OCaml frame that called it. *)

open Filler

(* ------------------------------------------------------------------------ *)
(* Sampling                                                                 *)
(* ------------------------------------------------------------------------ *)

let samples : Printexc.raw_backtrace list ref = ref []
let n_samples = ref 0

let start_sampling ~interval =
  Sys.set_signal Sys.sigprof
    (Sys.Signal_handle
       (fun _ ->
         incr n_samples;
         samples := Printexc.get_callstack 256 :: !samples));
  ignore
    (Unix.setitimer Unix.ITIMER_PROF
       { Unix.it_interval = interval; it_value = interval })

let stop_sampling () =
  ignore
    (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; it_value = 0. });
  Sys.set_signal Sys.sigprof Sys.Signal_ignore

(* ------------------------------------------------------------------------ *)
(* Symbolization and folding                                                *)
(* ------------------------------------------------------------------------ *)

(* "Filler__Board_bit_with_cache.fill.go" -> "Board_bit_with_cache.fill.go" *)
let short_name name =
  let drop prefix s =
    if String.starts_with ~prefix s then
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    else s
  in
  name |> drop "Filler__" |> drop "Dune__exe__"

(** Frame label: function name, plus [file:line] when [~lines] is set. With
    lines, each call site becomes its own frame, which splits callers apart;
    without, frames merge per function, which is usually what you want. *)
let label ~lines ~mark_inlined slot =
  let name =
    match Printexc.Slot.name slot with Some n -> short_name n | None -> "?"
  in
  let name =
    if mark_inlined && Printexc.Slot.is_inline slot then name ^ " [inlined]"
    else name
  in
  match (lines, Printexc.Slot.location slot) with
  | true, Some { filename; line_number; _ } ->
      Printf.sprintf "%s (%s:%d)" name (Filename.basename filename) line_number
  | _ -> name

(** Frames that belong to the profiler, not the profiled program *)
let is_profiler_frame l =
  String.starts_with ~prefix:"Profile." l
  || String.starts_with ~prefix:"Stdlib__Printexc" l
  || String.starts_with ~prefix:"Stdlib__Sys" l

(** Collapse recursion: when a frame reappears further up the stack, drop the
    cycle in between, so every level of a recursive search shares one frame.
    [frames] is root-first. *)
let fold_recursion frames =
  List.fold_left
    (fun acc f ->
      (* acc is innermost-first *)
      let rec cut = function
        | [] -> None
        | g :: rest -> if g = f then Some (g :: rest) else cut rest
      in
      match cut acc with Some kept -> kept | None -> f :: acc)
    [] frames
  |> List.rev

let write_folded ~lines ~mark_inlined ~recursion path =
  (* Symbolize each distinct return address once; one address can expand
     to several frames when calls were inlined into it. *)
  let symbols : (Printexc.raw_backtrace_entry, string list) Hashtbl.t =
    Hashtbl.create 1024
  in
  let frames_of entry =
    match Hashtbl.find_opt symbols entry with
    | Some fs -> fs
    | None ->
        let fs =
          match Printexc.backtrace_slots_of_raw_entry entry with
          | None -> []
          | Some slots ->
              Array.to_list (Array.map (label ~lines ~mark_inlined) slots)
        in
        Hashtbl.add symbols entry fs;
        fs
  in
  let counts : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  List.iter
    (fun bt ->
      (* entries are innermost-first; flamegraphs want root-first *)
      let frames =
        Printexc.raw_backtrace_entries bt
        |> Array.to_list |> List.concat_map frames_of
        |> List.filter (fun l -> not (is_profiler_frame l))
        |> List.rev
      in
      let frames = if recursion then frames else fold_recursion frames in
      if frames <> [] then
        let key = String.concat ";" frames in
        Hashtbl.replace counts key
          (1 + Option.value ~default:0 (Hashtbl.find_opt counts key)))
    !samples;
  let oc = open_out path in
  Hashtbl.iter (fun stack n -> Printf.fprintf oc "%s %d\n" stack n) counts;
  close_out oc

(* ------------------------------------------------------------------------ *)
(* Workloads                                                                *)
(* ------------------------------------------------------------------------ *)

(* The implementation being profiled. It is fixed at compile time, rather
   than picked at run time from [Impls.list], so flambda sees the concrete
   module and can make direct (and inlinable) calls into it, as in the real
   program; going through a first-class module would turn every board call
   into an unknown function call. Change both lines to profile another one. *)
module Impl = Board_bit_with_cache

let impl_name = "Board_bit_with_cache"

module Workload (M : Board.S) = struct
  module BM = Board.Make (M)
  module MM = Minimax.Make (M)

  let boards = Array.init 64 (fun _ -> M.of_squares (IO.random_squares ()))

  let minimax ~depth b =
    ignore (Sys.opaque_identity (MM.minimax ~max_depth:depth b))

  let playout b =
    let rec go b p =
      if Board.is_done (M.region_size b Us) (M.region_size b Opp) then ()
      else
        let moves = BM.valid_moves b in
        let c = List.nth moves (Random.int (List.length moves)) in
        go (M.move b c p) (Board.other_player p)
    in
    go b Board.Us

  (** Run [kind] over the boards, round-robin, until [seconds] of CPU time *)
  let run ~kind ~depth ~seconds =
    let deadline = Sys.time () +. seconds in
    let i = ref 0 in
    while Sys.time () < deadline do
      let b = boards.(!i mod Array.length boards) in
      incr i;
      match kind with `Minimax -> minimax ~depth b | `Playout -> playout b
    done;
    !i
end

(* ------------------------------------------------------------------------ *)
(* CLI                                                                      *)
(* ------------------------------------------------------------------------ *)

module W = Workload (Impl)

let () =
  let kind = ref `Minimax
  and depth = ref 6
  and seconds = ref 5.
  and interval = ref 0.001
  and lines = ref false
  and recursion = ref false
  and mark_inlined = ref false
  and out = ref "profile.folded" in
  Arg.parse
    [
      ( "-workload",
        Arg.Symbol
          ( [ "minimax"; "playout" ],
            fun s -> kind := if s = "minimax" then `Minimax else `Playout ),
        " what to run (default minimax)" );
      ("-depth", Arg.Set_int depth, "N  minimax depth (default 6)");
      ("-seconds", Arg.Set_float seconds, "S  CPU seconds to sample (default 5)");
      ( "-interval",
        Arg.Set_float interval,
        "S  sampling interval in seconds (default 0.001)" );
      ("-lines", Arg.Set lines, " label frames with file:line too");
      ( "-mark-inlined",
        Arg.Set mark_inlined,
        " suffix frames that flambda inlined with [inlined]" );
      ( "-keep-recursion",
        Arg.Set recursion,
        " keep one frame per recursion level (default: merge them)" );
      ("-o", Arg.Set_string out, "FILE  output (default profile.folded)");
    ]
    (fun s ->
      raise
        (Arg.Bad
           (Printf.sprintf
              "unexpected argument %S: the implementation is fixed at compile \
               time (Impl in bin/profile.ml)"
              s)))
    ("profile.exe [options]\n\nProfiles " ^ impl_name ^ ".\n\nOptions:");
  Random.init 0xF1A3E;
  start_sampling ~interval:!interval;
  let iterations = W.run ~kind:!kind ~depth:!depth ~seconds:!seconds in
  stop_sampling ();
  write_folded ~lines:!lines ~mark_inlined:!mark_inlined ~recursion:!recursion
    !out;
  Printf.printf "%s: %d %s runs (%.3f ms per), %d samples -> %s\n" impl_name
    iterations
    (match !kind with `Minimax -> "minimax" | `Playout -> "playout")
    (1000. *. !seconds /. float_of_int iterations)
    !n_samples !out
