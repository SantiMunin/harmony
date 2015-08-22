#!/bin/sh

exitWithMessageIfFailure() {
  if [ $1 != 0 ]; then
    echo ""
    printf "\033[01;31m%s\033[00m\n" "ERROR: $2";
    exit 1;
  fi
}

checkGood() {
  echo "\nGenerating target for examples/good/$1"
  harmony -sjs -cpython examples/good/$1
  exitWithMessageIfFailure $? "Harmony couldn't compile examples/good/$1"
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
  exitWithMessageIfFailure $TEST_OUTPUT "There was a problem while executing the tests (or they failed)."
}

checkJavaGood() {
  echo "\nGenerating Java target for examples/good/$1"
  harmony -cjava examples/good/$1
  exitWithMessageIfFailure $? "Harmony couldn't compile examples/good/$1"
  printf "%s" "Compiling Java target... "
  mvn -f harmony_output/client/java/pom.xml compile | grep 'BUILD SUCCESS' > /dev/null
  exitWithMessageIfFailure $? "Maven could not compile the generated Java target for examples/good/$1"
  printf "%s" "OK"
  # TODO: once tests are implemented, this should use a server and execute them
}

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
  checkGood $file
  checkJavaGood $file
done

echo "Checking bad examples"
for file in `ls examples/bad`; do
  checkBad $file
done

printf "\n\033[01;32m%s\033[00m\n" "EVERYTHING OK"
