Harmony is a web service specification compiler that generates implementation (server and client) and tests.

## Targets supported

+ Server
  * Node.js
+ Client
  + Python
+ Test: TODO

## Usage

After installing the package (`cabal install`), you will get the `harmony` executable. Afterwards:

    Usage: harmony [OPTION...] input_file
      -c[CLIENTS]     --client[=CLIENTS]         Desired output for the client
      -s[SERVERS]     --server[=SERVERS]         Desired output for the server
      -o[OUTPUT_DIR]  --output_dir[=OUTPUT_DIR]  Output path


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



