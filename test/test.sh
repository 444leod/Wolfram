#!/bin/bash

make -C . > /dev/null

LEO_BIN=./wolfram
ELLIOT_BIN=./wolfram_elliot
RULES=(30 90 110)
LINES=(80 100 120)
MOVES=(-20 0 20)
WINDOW=(20 40 60 200)
START=(0 10 50 100)

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

declare -a ELLIOT_WINS

for rule in ${RULES[@]}; do
  for line in ${LINES[@]}; do
    for move in ${MOVES[@]}; do
      for window in ${WINDOW[@]}; do
        for start in ${START[@]}; do
          FLAGS="--rule $rule --lines $line --move $move --window $window --start $start"
          
          START_TIME_LEO=$(date +%s%N)
          $LEO_BIN $FLAGS | cat -e > leo_output
          END_TIME_LEO=$(date +%s%N)
          TIME_LEO=$(echo "scale=3; ($END_TIME_LEO - $START_TIME_LEO) / 1000000000" | bc)

          START_TIME_ELLIOT=$(date +%s%N)
          $ELLIOT_BIN $FLAGS | cat -e > elliot_output
          END_TIME_ELLIOT=$(date +%s%N)
          TIME_ELLIOT=$(echo "scale=3; ($END_TIME_ELLIOT - $START_TIME_ELLIOT) / 1000000000" | bc)

          if ! diff leo_output elliot_output > /dev/null; then
            diff leo_output elliot_output
            echo "Test failed with flags: $FLAGS"
            exit 1
          fi

          echo "Test with flags: $FLAGS"
          if (( $(echo "$TIME_LEO < $TIME_ELLIOT" | bc -l) )); then
            echo -e "${GREEN}Leo execution time: $TIME_LEO seconds${NC}"
            echo "Elliot execution time: $TIME_ELLIOT seconds"
          elif (( $(echo "$TIME_LEO > $TIME_ELLIOT" | bc -l) )); then
            echo "Leo execution time: $TIME_LEO seconds"
            echo -e "${RED}Elliot execution time: $TIME_ELLIOT seconds${NC}"
            ELLIOT_WINS+=("Test with flags: $FLAGS\nLeo execution time: $TIME_LEO seconds\nElliot execution time: $TIME_ELLIOT seconds")
          else
            echo "WE THE SAME"
            echo "Leo execution time: $TIME_LEO seconds"
            echo "Elliot execution time: $TIME_ELLIOT seconds"
          fi
          echo "----------------------------------------"
        done
      done
    done
  done
done

echo "All tests passed"

if [ ${#ELLIOT_WINS[@]} -ne 0 ]; then
  echo "Tests where Elliot was faster:"
  for win in "${ELLIOT_WINS[@]}"; do
    echo -e "$win"
    echo "----------------------------------------"
  done
fi
