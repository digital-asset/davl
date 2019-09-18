# pina-cli

Command line interface for `pina-colada` Holiday tracking application, driven by DAML.

`pina-cli` connects (via `gRPC`) to a DAML ledger  hosting the pina-DAML-model

### build

    cd /path/to/daml
    language-support/hs/bindings/export-package.sh <HERE>
    cd -
    stack build

### run

    daml sandbox # different terminal
    (cd ..; daml deploy)

    stack run pina-cli
    stack run pina-cli -- Bob

### diagnostics

    pina-exe: GRPCIOTimeout
You haven't started the sandbox ledger.

    pina-exe: user error (cant find package containing Pina)
You haven't deployed the DAML model.


### using the CLI

Current WIP output:

    $ stack run pina-cli
    replaying 0 transactions
    'Alice'> !Bob
    'Alice'> ('Bob') .
    -> (GiftSent'Bob')
    'Alice'> ('Bob') .
    -> (GiftSent'Bob')
    'Alice'> ('Bob')

### test

- TODO
