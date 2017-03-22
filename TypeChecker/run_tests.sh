#!/bin/bash
for i in `seq 1 49`;
do
    echo 'CM.make "sources.cm"; Main.tycheck "../tiger/testcases/test'$i'.tig";' | sml >> test_output.txt
done
