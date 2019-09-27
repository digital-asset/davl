
# Developer Log

This file records thoughts and ideas which occur during development, including:

- _What worked well?_
- _What was tricky?_
- _What didn't work at all?_
- _How did being DAML driven help things?_'
- _How could DAML be improved?_

These notes should be mainly regarded as append-only. But we can fixup and improve earlier entries if we wish, including deleting stuff which on reflection seems on low value.

Martin suggest that perhaps these file could be hosted as Github pages.

One of the hopes for these notes is they might form the basis of future blog posts about the experiences of team-davl in developing a kick-ass DAML ledger app.


### Tue 17th September (Nick)

- Nick and Gary pairing to get a quick first cut for `davl-cli`. As gary puts it: _What we've done today is basically taking Nick's [chat app](https://github.com/digital-asset/daml/tree/master/language-support/hs/bindings/examples/chat) and changing the core datatypes from under it, then try to play whack-a-mole with the compiler error messages._
- Constructing/Interpreting `DAML-LF` values (in Haskell) for communication over the ledger API could be easier. When there are mistakes, error messages from the runtime system could be more illuminating.


### Wed 18th September (Nick)

- created subdir for `davl-cli` code and added README
- created this diary


### Thu 19th September (Nick)

- fixing model version 1. DAML and spec.
- choosing simpler un-compunded names: i.e `Holiday`, `Vacation`

- renamed this project to `davl` - _DA Vacation Ledger_