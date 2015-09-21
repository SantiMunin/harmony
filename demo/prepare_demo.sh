#!/bin/sh

PORT=8250
DB=demo_db

cp harmony_output/client/python/client.py demo
cd harmony_output/server/js
npm-cache install
cd ../../..
killall -9 node
echo "Running server in localhost:$PORT ..."
node harmony_output/server/js/server.js $PORT mongodb://localhost $DB &
sleep 1
curl -X DELETE http://localhost:$PORT/_wipedatabase

