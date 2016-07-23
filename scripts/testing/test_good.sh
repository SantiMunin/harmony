#!/bin/bash

source $(dirname "$0")/common.sh

checkGood() {
  echo "\nGenerating Javascript and Python targets for $1 (using npm-cache: $2)"
  harmony -sjs -cpython $1
  exitWithMessageIfFailure $? "Harmony couldn't compile $1"
  printf "%s\n" "Installing node.js dependencies..."
  cd harmony_output/server/js
  if [ $2 == 1 ]; then
   npm-cache install
  else
    npm install
  fi
  exitWithMessageIfFailure $? "There was a problem while executing npm-cache install"
  cd ../../..
  echo "Executing server in background"
  node harmony_output/server/js/server.js $PORT $MONGO_ADD $MONGO_DB > /dev/null &
  NODE_PID=$!
  sleep 4
  "Does $NODE_PID exist?"
  ps aux | grep $NODE_PID
  kill -0 $NODE_PID
  exitWithMessageIfFailure $? "There was a problem while deploying the server"
  echo "Server pid: $NODE_PID"
  sleep 4
  curl -X DELETE http://localhost:$PORT/_wipedatabase
  sleep 1
  echo "Executing tests against http://localhost:$PORT"
  pip install -r harmony_output/client/python/requirements.txt
  python harmony_output/client/python/test.py http://localhost:$PORT
  TEST_OUTPUT=$?
  echo "Killing server (pid: $NODE_PID)"
  kill -9 $NODE_PID
  exitWithMessageIfFailure $TEST_OUTPUT "There was a problem while executing the tests for $1 (or they failed)."
}

checkJavaGood() {
  echo "\nGenerating Java target for $1"
  harmony -cjava $1
  exitWithMessageIfFailure $? "Harmony couldn't compile examples/good/$1"
  printf "%s" "Compiling Java target... "
  mvn -f harmony_output/client/java/pom.xml compile | grep 'BUILD SUCCESS' > /dev/null
  exitWithMessageIfFailure $? "Maven could not compile the generated Java target for examples/good/$1"
  printf "%s\n" "OK"
  # TODO: once tests are implemented, this should use a server and execute them
}

checkGood $1 $2
checkJavaGood $1

printf "\n\033[01;32m%s\033[00m" "[OK] "
printf "$1\n"
