# davl-cli

Command line interface for the `DAVL` Holiday tracking application, driven by `DAML`.

WIP: See [TODOs](/cli/TODO.md).

`davl-cli` connects (via `gRPC`) to a DAML ledger  hosting the davl-DAML-model

### build

    cd /path/to/daml
    language-support/hs/bindings/export-package.sh <HERE>
    cd -
    stack build

### run

    daml sandbox # different terminal
    (cd ..; daml deploy)

    stack run davl-cli
    stack run davl-cli -- Bob

### diagnostics

    davl-exe: GRPCIOTimeout
You haven't started the sandbox ledger.

    davl-exe: user error (cant find package containing Davl)
You haven't deployed the DAML model.


### using the CLI

    $ stack run davl-cli Alice
    'Alice'> help
    'Alice'> give Bob
    'Alice'> give Bob
    'Alice'> give Charlie

    $ stack run davl-cli Bob
    'Bob> summary
    'Bob> claim Alice
    'Bob> summary

    $ stack run davl-cli Alice
    'Alice> summary

### test

    ./test.sh
