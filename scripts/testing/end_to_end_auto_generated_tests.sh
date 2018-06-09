#!/bin/bash

processTestResult() {
  if [ "$1" == "0" ]; then
    mv $2 passed_tests
  else
    mv $2 failed_tests
  fi
}

file_dir="$(dirname "$0")"

cabal install

mkdir -p passed_tests
mkdir -p failed_tests

for i in `seq 1 $1`;
do
  harmony-spec > $i.hmy  
  $file_dir/test_good.sh $i.hmy 
  processTestResult $? $i.hmy
done
