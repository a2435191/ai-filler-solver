type t
(** The type of 7x8 game boards *)

(* Basic helper functions *)

val get : t -> int * int -> Color.t
(** [get board (y, x)] returns the color of the square at index [(y, x)] *)

val set : t -> int * int -> Color.t -> unit
(** [set board (y, x) color] sets (in-place) the color of the square at index
    [(y, x)] to [color] *)

val get_corner : t -> Player.t -> Color.t
(** [get_corner board player] is like [get] but always returns the corner color
    corresponding to [player] *)

val set_corner : t -> Player.t -> Color.t -> unit
(** [set_corner board player color] is like [set] but always sets the corner
    color corresponding to [player] to [color] *)

val check_inv : t -> t
(** Return the input if it satisfies all the invariants, otherwise raise *)

val random : unit -> t
(** Return a uniform random board *with corners of different colors* *)

val print : t -> unit
(** Pretty-print board to stdout *)

val parse : string -> t
(** Parse a newline-delimited string as a board. Accepts the format output by
    [print], or letters for each of the colors (see [Color.from_string]) *)

(* Compute information required for heuristic functions *)

val region_size : t -> Player.t -> int
(** Count the size of the colored-in region starting at a corner (corresponding
    to either player) *)

val move : t -> Color.t -> Player.t -> t
(** [move board color player] computes the new board if player [player] makes
    move [color] on board [board] *)
