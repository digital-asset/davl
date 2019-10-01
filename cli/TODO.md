
### DONE
+ UI: as boss, see list of requests, with stable counting index
+ UI: as employee, see list of requests
+ Denial: type, ledger-trans
+ UI: deny (take index from request list) + text
+ add new workflow steps to CLI regression: request/deny
+ refactor1: makeLedgerCommand

### Goal, for Thurday demo
+ Vacation: Domain, LedgerTranslation
+ UI: approve (take index from request list)
+ UI: list denials
+ UI: list vacations
- Agg: +count denials
- Agg: +count vacations
- connect to remote host/port

### Misc:
- UI: giveN, claimAll
- UI: fix handling when reading int/date fails
- UI: always show pending-requests to bosses

- fix UI to make request only on allocations with no pending request
- OR, deal with requests which contain references to now-spent allocations - deny fails!

- date support in CLI, something better than days-since-epoch!
- refactor: extractEvents
- make aggregation code less of a pain
- UI: boss/employee mode: `data Role = AsBoss Party | AsEmployee Party`
- Local request numbering is not quite stable in face of async updates

### Model:
- Model: denials should be observed by the boss/denier
- Model: auto-claim, by having employee register with Boss
