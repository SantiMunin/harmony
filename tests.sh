#!/bin/sh

exitWithMessageIfFailure() {
  if [ $1 != 0 ]; then
    echo ""
    printf "\033[01;31m%s\033[00m\n" "ERROR: $2";
    exit 1;
  fi
}

checkGood() {
  echo "Generating target for examples/good/$1"
  harmony -sjs -cpython examples/good/$1
  exitWithMessageIfFailure $? "Harmony couldn't compile examples/good/$1"
  printf "%s" "Installing node.js dependencies..."
  cd harmony_output/server/js
  npm-cache install 
  exitWithMessageIfFailure $? "There was a problem while executing npm-cache install"
  cd ../../..
  echo "Executing server in background"
  node harmony_output/server/js/server.js $PORT $MONGO_ADD &
  NODE_PID=$!
  echo "Server pid: $NODE_PID"
  sleep 5
  echo "Executing tests against http://localhost:$PORT"
  python harmony_output/client/python/test.py http://localhost:$PORT > /dev/null
  TEST_OUTPUT=$?
  echo "Killing server (pid: $NODE_PID)"
  kill -9 $NODE_PID
  exitWithMessageIfFailure $TEST_OUTPUT "There was a problem while executing the tests (or they failed)."
}

checkBad() {
  echo "Trying to generate target for examples/bad/$1 (it should fail)"
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
  checkGood $file
done

echo "Checking bad examples"
for file in `ls examples/bad`; do
  checkBad $file
done

printf "\n\033[01;32m%s\033[00m\n" "EVERYTHING OK"
