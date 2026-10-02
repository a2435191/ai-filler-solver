open Board

val random : unit -> squares
(** Return a uniform random board *with corners of different colors* *)

val print : squares -> unit
(** Pretty-print board to stdout *)

val parse : string -> squares
