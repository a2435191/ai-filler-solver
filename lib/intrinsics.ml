(* Note: input is not [@untagged] because we'd have to convert from 64- to
   63-bit anyway, otherwise negative numbers would be wrong: -1 = 0x7ffff... in
   63-bit 2's complement, but 0xffff... as passed when untagged *)
external popcount : int -> (int[@untagged])
  = "fast_popcount_bytecode" "fast_popcount_native"
[@@noalloc]
