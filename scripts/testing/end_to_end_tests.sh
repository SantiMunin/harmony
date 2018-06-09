#!/bin/bash

file_dir="$(dirname "$0")"

. $file_dir/common.sh

cabal install

echo "Checking good examples"
for file in `ls examples/good`; do
  sh $file_dir/test_good.sh examples/good/$file $1
  exitWithMessageIfNotEquals 0 $? "Good test case failed, aborting"
done

echo "Checking bad examples"
for file in `ls examples/bad`; do
  sh $file_dir/test_bad.sh examples/bad/$file
  exitWithMessageIfNotEquals 0 $? "Bad test did not fail, aborting"
done

printf "\n\033[01;32m%s\033[00m\n" "EVERYTHING OK"
