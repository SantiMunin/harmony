#!/bin/sh

file_dir="$(dirname "$0")"

checkBad() {
  echo "\nTrying to generate target for examples/bad/$1 (it should fail)"
  harmony -sjs -cpython examples/bad/$1 2> /dev/null
  if [ $? = 0 ]; then
    echo "File examples/bad/$1 compiled successfully (it shouldn't)";
    exit 1;
  fi
} 

PORT=3123
MONGO_ADD="mongodb://localhost/test_db"

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
