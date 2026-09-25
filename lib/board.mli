type t
(** The type of 7x8 game boards *)

(* Constants *)

val height : int
val width : int
val total_squares : int
val squares_to_tie : int

(* Basic helper functions *)

val get_coord : t -> int * int -> Color.t
val set_coord : t -> int * int -> Color.t -> unit

(** There are two players in the game, and we represent the player that we're
    trying to help win as [Us], and the other player as [Opp] *)
type player =
  | Us  (** The player that we're trying to help win *)
  | Opp  (** The player that we're trying to help lose *)

val player_to_corner : player -> int * int
val other_player : player -> player
val get : t -> player -> Color.t
val set : t -> player -> Color.t -> unit
val check_inv : t -> t
val random : unit -> t
val print : t -> unit
val parse : string -> t

(* Compute information required for heuristic functions *)

val region_size : t -> player -> int
(** Count the size of the colored-in region starting at a corner (corresponding
    to either player) *)

val move : t -> Color.t -> player -> t
(** [move board color player] computes the new board if player [player] makes
    move [color] on board [board] *)
