(* Just a list of implementations of Board.S for bin/bench.ml and test/test_filler.ml *)

let list : (string * (module Board.S)) list =
  [%get_impl_info
    "Board_basic" "Board_with_cache" "Board_bit" "Board_bit_with_cache"]
