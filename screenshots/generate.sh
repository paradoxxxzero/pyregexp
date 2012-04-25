#!/bin/sh
for i in {1..4}; do
montage "pyregexp$i*.png" -mode Concatenate -tile 2x2 -frame 2 -geometry +0+0 -background transparent "pyregexp$i.png";
done
