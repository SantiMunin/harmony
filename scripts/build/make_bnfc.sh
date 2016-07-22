#!/bin/sh

# Compiles the language specification if it is needed, removing unnecessary files.

DEST_DIR=gen

makeBnfc() {
  bnfc -d language-spec/Language.cf; 
  rm Language/Test.hs
  rm Language/Print.hs
  mkdir -p $DEST_DIR 
  mv Language/ $DEST_DIR
  rm -rf Language/
}

printf "%s" "Checking status of language specification..."
if [ ! -d '$DEST_DIR' ]; then
  printf " %s\n" "Language specification needs to be compiled."
  makeBnfc;
  exit 0
fi

if [ 'language-spec/Language.cf' -nt $DEST_DIR ]; then
  printf " %s\n" "Language specification needs to be re-compiled";
  rm -rf $DEST_DIR;
  makeBnfc;
  exit 0
fi

printf "%s\n" " OK"
