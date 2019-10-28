
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

### Tue 17th September

- Nick and Gary pairing to get a quick first cut for `davl-cli`. As gary puts it: _What we've done today is basically taking Nick's [chat app](https://github.com/digital-asset/daml/tree/master/language-support/hs/bindings/examples/chat) and changing the core datatypes from under it, then try to play whack-a-mole with the compiler error messages._
- Constructing/Interpreting `DAML-LF` values (in Haskell) for communication over the ledger API could be easier. When there are mistakes, error messages from the runtime system could be more illuminating.

### Wed 18th September

- created subdir for `davl-cli` code and added README
- created this diary

### Thu 19th September

- fixing model version 1. DAML and spec.
- choosing simpler un-compunded names: i.e `Holiday`, `Vacation`
- renamed this project to `davl` - _DA Vacation Ledger_

### Fri 26th September

- continuing to hack on the CLI
- trying to figure out the best way to organize the code to be malliable for incremental changes
- CLI now support workflow: give/claim + summary query: as boss (grouped by employee) / as employee (grouped by boss)

- one thing that is clearer is that the core in-App representation just needs to be [Event] (Create/Archive) received from the ledger. And these can be further processed on demand... in particular to determine the active contract set

### Fri 4th October

- Here are some things we learned while implementing a simple CLI for the V1 model
[here](/notes/v1-learnings.md).
- Here is the discussion following the v1 demo on Friday 2019-10-04
[here](/notes/v1-post-demo-discussion.md).

### Mon 7th October

- Following the demo and discussions on Friday, we are now considering what features to add in the V2 model
And how we plan to migrate from V1 to V2.


### Mon 28th October

- The rewrite for V2 of the DAML model is now complete (as planned in the redesign.text). And merged into master.

Following discussions during the GetTogether, there are some changes we might try:

(A) Try (again) to avoid explicit management of observers everywhere, by using a disclosure technique to allow partys to see contracts which they are not an observer of. Previously, Bernhard suggested something similar(identical?), but I didn't manage to get it to work. But with new insight, I want to try again.

(B) Avoid explicit management within the DAML of lists of active contracts, i.e. for `Holiday` allocation and booked `Vacation`, by instead having these passed as extra arguments to the choices where they are needed, moving the burden of tracking active contracts to the user on the other side of the Ledger API.

I think we can try (A) as part of V2. Assuming it works, it only changes(simplifies) the implementation.
But perhaps we should leave (B) to the next version; following discussion within the DAVL team.
