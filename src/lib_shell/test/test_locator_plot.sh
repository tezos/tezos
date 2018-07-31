#!/bin/bash

# plots the output of 'dune build @runtest_locator'

set -e

size_chain=$(grep 'size_chain' $1 | awk '{print $2}')
exp_limit=$(grep 'exp_limit' $1 | awk '{print $2}')
locator_limit=$(grep 'locator_limit' $1 | awk '{print $2}')
runs=$(grep 'runs' $1 | awk '{print $2}')

echo "\
input=\"${1}\";
set terminal svg;
#set terminal dumb;
set key top left;
#set logscale y;
set title '# stored predecessors 12, runs ${runs}, size chain ${size_chain}, exp limit ${exp_limit}, locator limit ${locator_limit}'
set xlabel 'size locator';
set ylabel 'time (seconds)';
plot input using 1:2 ls 1 title 'exponential', \
     input using 1:3 ls 2 title 'linear'
" | gnuplot > ${1}.svg
