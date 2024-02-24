#!/bin/bash

LEO_BIN=./wolfram
ELLIOT_BIN=./wolfram_elliot

make -C . > /dev/null

RULES=(30 90 110)
LINES=(80 100 120)
MOVES=(-20 0 20)
WINDOW=(20 40 60 200)
START=(0 10 50 100)

for rule in ${RULES[@]}; do
  for line in ${LINES[@]}; do
    for move in ${MOVES[@]}; do
      for window in ${WINDOW[@]}; do
        for start in ${START[@]}; do
          FLAGS="--rule $rule --lines $line --move $move --window $window --start $start"
          $LEO_BIN $FLAGS | cat -e > leo_output
          $ELLIOT_BIN $FLAGS | cat -e > elliot_output
          if ! diff leo_output elliot_output > /dev/null; then
            diff leo_output elliot_output
            echo "Test failed with flags: $FLAGS"
            exit 1
          fi
          echo "Test passed with flags: $FLAGS"
        done
      done
    done
  done
done

echo "All tests passed"

