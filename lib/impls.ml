(* Just a list of implementations of Board.S for bin/bench.ml and test/test_filler.ml *)

let list : (string * (module Board.S)) list =
  [
    ("Board_basic", (module Board_basic));
    ("Board_with_cache", (module Board_with_cache));
    ("Board_bit", (module Board_bit));
    ("Board_bit_with_cache", (module Board_bit_with_cache));
  ]
