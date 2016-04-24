#!/bin/sh

# Compiles the language specification if it is needed, removing all unnecessary files and appending
# a pragma to tell HLint to ignore those files.

makeBnfc() {
  bnfc -d language-spec/Language.cf; 
  rm Language/Test.hs
  rm Language/Print.hs
  mv Language gen/
}

printf "%s" "Checking status of language specification..."
if [ ! -d 'gen/Language' ]; then
  printf " %s\n" "Language specification needs to be compiled"
  makeBnfc;
  exit 0
fi

if [ 'language-spec/Language.cf' -nt gen/Language/ ]; then 
  printf " %s\n" "Language specification needs to be re-compiled";
  rm -rf gen/Language;
  makeBnfc;
  exit 0
fi

printf "%s\n" " OK"
