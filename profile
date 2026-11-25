#!/bin/zsh

mkdir -p tmp\
&& dune exec --profile release bin/profile.exe -- -depth 10 -seconds 10 -o ./tmp/profile.folded\
&& ~/FlameGraph/flamegraph.pl --countname samples ./tmp/profile.folded > ./tmp/profile.svg\
&& open -a "Brave Browser" ./tmp/profile.svg