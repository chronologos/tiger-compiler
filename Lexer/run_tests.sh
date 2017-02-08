#!/bin/bash

for i in {1...49}
  echo 'CM.make "sources.cm"; Parse.parse "tests/appel/test$i.tig";' | sml > test_results/test$i.txt
