#!/bin/sh

file_dir="$(dirname "$0")"

cabal install

echo "Checking good examples"
for file in `ls examples/good`; do
  sh $file_dir/test_good.sh examples/good/$file
done

echo "Checking bad examples"
for file in `ls examples/bad`; do
  sh $file_dir/test_bad.sh examples/bad/$file
done

printf "\n\033[01;32m%s\033[00m\n" "EVERYTHING OK"
