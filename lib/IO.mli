open Board

val random_squares : unit -> squares
(** Return a uniform random board *with corners of different colors* *)

val print_squares : squares -> unit
(** Pretty-print board to stdout *)

val parse_to_squares : string -> squares
