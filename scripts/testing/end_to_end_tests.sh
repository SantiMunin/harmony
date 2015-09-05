#!/bin/sh

file_dir="$(dirname "$0")"

exitWithMessageIfNotEquals() {
  if [ $1 != $2 ]; then
    echo ""
    printf "\033[01;31m%s\033[00m" "[ERROR] ";
    printf "$3\n";
    exit 1;
  fi
}

cabal install

echo "Checking good examples"
for file in `ls examples/good`; do
  sh $file_dir/test_good.sh examples/good/$file
  exitWithMessageIfNotEquals 0 $? "good test case failed"
done

echo "Checking bad examples"
for file in `ls examples/bad`; do
  sh $file_dir/test_bad.sh examples/bad/$file
  exitWithMessageIfNotEquals 0 $? "bad test did not fail"
done

printf "\n\033[01;32m%s\033[00m\n" "EVERYTHING OK"
