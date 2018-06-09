#!/bin/bash

PORT=3123
MONGO_ADD="mongodb://localhost"
MONGO_DB="_test_db"

RED_PRINTF_FORMAT="\033[01;31m%s\033[00m"

printErrorInRed() {
  echo ""
  printf $RED_PRINTF_FORMAT "[ERROR] ";
  printf "$1\n";
}

exitWithMessageIfNotEquals() {
  if [ $1 != $2 ]; then
    printErrorInRed "$3"
    exit 1;
  fi
}

exitWithMessageIfFailure() {
  if [ $1 != 0 ]; then
    printErrorInRed "$2"
    exit 1;
  fi
}
