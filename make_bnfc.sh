#!/bin/sh

# Compiles the language specification if it is needed, removing all unnecessary files and appending
# a pragma to tell HLint to ignore those files.

if [ 'language-spec/Language.cf' -nt src/Language/ ];
then 
  echo "Language specification needs to be compiled";
  bnfc -d language-spec/Language.cf; 
  rm -rf src/Language;
  rm Language/Test.hs
  rm Language/Print.hs
  mv Language src
fi
