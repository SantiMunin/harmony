# Harmony [![Hackage version](https://img.shields.io/hackage/v/harmony.svg?style=flat)](https://hackage.haskell.org/package/harmony) [![Build Status](https://secure.travis-ci.org/SantiMunin/harmony.svg?branch=master)](http://travis-ci.org/SantiMunin/harmony)
Harmony is a web service specification compiler that generates implementation (server and client) and tests.

## Targets supported

+ Server
  * Node.js (`-sjs`)
+ Client 
  + Python (`-cpython`)
    + Includes [Hypothesis](https://github.com/DRMacIver/hypothesis) based tests.
  + Java (`-cjava`

## Installation

+ From Hackage: `cabal install harmony`
+ From source code: `git clone https://www.github.com/SantiMunin/harmony && cd harmony && cabal install`

## Usage

After installing the package (`cabal install`), you will get the `harmony` executable. Afterwards:

```
Usage: harmony [OPTION...] input_file
  -c[CLIENTS]     --client[=CLIENTS]         Desired output for the client: {-cpython, -cjava}
  -s[SERVERS]     --server[=SERVERS]         Desired output for the server: {-sjs}
  -o[OUTPUT_DIR]  --output_dir[=OUTPUT_DIR]  Output path
```
Check the [Targets supported](#targets-supported) section to figure out the code related to each target. Also, refer to the [wiki](https://github.com/SantiMunin/harmony/wiki) for more information.

## Syntax

The input file should follow the syntax specified in `language_spec/Language.cf`. Example:

    service_name: HouseworkService
    service_version: 1.0.0

    enum Color { RED, BLUE, BLACK }

    struct Task {
       // Primary key
       @PK
       name : String,
       color : Color
    }

    struct Person {
      name: String,
      age: Int,
      tasks: [Task]
    }

    resource Person ("/person")

## Contributing

See the [wiki](https://github.com/SantiMunin/harmony/wiki).
