#!/bin/bash
for i in `seq 1 49`;
do  
    echo "##################test num $i##################" >> test_output.txt
    echo 'CM.make "sources.cm"; Main.tycheck "../tiger/testcases/test'$i'.tig";' | sml >> test_output.txt
done
