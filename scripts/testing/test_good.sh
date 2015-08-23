#!/bin/sh

exitWithMessageIfFailure() {
  if [ $1 != 0 ]; then
    echo ""
    printf "\033[01;31m%s\033[00m" "[ERROR] ";
    printf "$2\n";
    exit 1;
  fi
}

checkGood() {
  echo "\nGenerating Javascript and Python targets for $1"
  harmony -sjs -cpython $1
  exitWithMessageIfFailure $? "Harmony couldn't compile $1"
  printf "%s\n" "Installing node.js dependencies..."
  cd harmony_output/server/js
  npm-cache install 
  exitWithMessageIfFailure $? "There was a problem while executing npm-cache install"
  cd ../../..
  echo "Executing server in background"
  node harmony_output/server/js/server.js $PORT $MONGO_ADD > /dev/null &
  NODE_PID=$!
  echo "Server pid: $NODE_PID"
  sleep 5
  echo "Executing tests against http://localhost:$PORT"
  pip install -r harmony_output/client/python/requirements.txt
  python harmony_output/client/python/test.py http://localhost:$PORT > /dev/null
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

PORT=3123
MONGO_ADD="mongodb://localhost/test_db"

checkGood $1
checkJavaGood $1

printf "\n\033[01;32m%s\033[00m" "[OK] "
printf "$1\n"
