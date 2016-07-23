#!/bin/bash

checkBad() {
  echo "\nTrying to generate target for $1 (it should fail)"
  harmony -sjs -cpython examples/bad/$1 2> /dev/null
  if [ $? = 0 ]; then
    echo "File $1 compiled successfully (it shouldn't)";
    exit 1
  fi
} 

checkBad $1

printf "\n\033[01;32m%s\033[00m" "[OK]"
printf " $1 failed at compile time (bad example)\n"
