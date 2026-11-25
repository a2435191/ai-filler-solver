#!/bin/zsh

dune exec --profile release bin/profile.exe -- Board_bit_with_cache -depth 8 -seconds 10 -o /tmp/profile.folded\
&& ~/FlameGraph/flamegraph.pl --countname samples /tmp/profile.folded > /tmp/profile.svg\
&& open -a Safari /tmp/profile.svg