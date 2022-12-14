#!/usr/bin/bash

BIN_DIR="./.cabal-sandbox/bin/"

echo "Building to directory $BIN_DIR"
cabal clean
cabal build
cabal install

for DAY in day-*; do
  echo "Testing $DAY"
  INPUT_FOLDER=$DAY/input.txt
  EXPECTED_OUT1=`cat $DAY/output1.txt`
  EXPECTED_OUT2=`cat $DAY/output2.txt`

  TIME_BEFORE="$(date +'%s%3N')"
  OUT1=$($BIN_DIR/$DAY-1 < $INPUT_FOLDER)
  TIME_AFTER="$(date +'%s%3N')"
  TIME=`echo "$TIME_AFTER - $TIME_BEFORE" | bc`
  if [ "$OUT1" == "$EXPECTED_OUT1" ]; then
    echo -e "\tAssignment 1 SUCCESS ran in $TIME ms."
  else
    echo -e "\tAssignment 1 FAILURE"
    echo -e "\tExpected $EXPECTED_OUT1 but was $OUT1"
  fi

  TIME_BEFORE="$(date +'%s%3N')"
  OUT2=$($BIN_DIR/$DAY-2 < $INPUT_FOLDER)
  TIME_AFTER="$(date +'%s%3N')"
  TIME=`echo "$TIME_AFTER - $TIME_BEFORE" | bc`
  if [ "$OUT2" == "$EXPECTED_OUT2" ]; then
    echo -e "\tAssignment 2 SUCCESS ran in $TIME ms."
  else
    echo -e "\tAssignment 2 FAILURE"
    echo -e "\tExpected $EXPECTED_OUT2 but was $OUT2"
  fi
done
