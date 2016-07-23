#!/bin/bash

source $(dirname "$0")/common.sh

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
pip install -r harmony_output/client/python/requirements.txt

echo "Python testing command: python harmony_output/client/python/test.py http://localhost:$PORT"

echo "Executing server in background for port: $PORT"
node harmony_output/server/js/server.js $PORT $MONGO_ADD $MONGO_DB


