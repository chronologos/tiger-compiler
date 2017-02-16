#!/bin/bash
for i in `seq 1 49`;
do
    echo 'CM.make "sources.cm"; Parse.parse "../tiger/testcases/test'$i'.tig";' | sml >> test_output.txt
done
