(* Just a list of implementations of Board.S for bin/bench.ml and test/test_filler.ml *)

let list : (string * (module Board.S)) list =
  [
    [%get_impl_info "Board_basic"];
    [%get_impl_info "Board_with_cache"];
    [%get_impl_info "Board_bit"];
    [%get_impl_info "Board_bit_with_cache"];
  ]
