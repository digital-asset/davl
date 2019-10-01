
### Request(finish):
+ UI: as boss, see list of requests, with stable counting index
+ UI: as employee, see list of requests

### Deny:
+ Denial: type, ledger-trans
+ UI: deny (take index, default 0) + text

### Test, refactor
+ add new workflow steps to CLI regression: request/deny
> refactor1: makeLedgerCommand
- refcator2: extractEvents

### Approve:
- Vacation: type, ledger-trans
- UI: approve (take index from request list)
- UI: as boss/employee, see list of vacations

### query
- Agg: +count denials
- Agg: +count vacations
- list denials
- list vacations (booked vacations)

### Misc:
- fix UI to make request only on allocations with no pending request
- date support in CLI, something better than days-since-epoch!
- factoring common code
- make aggregation code less of a pain
- UI: boss/employee mode: `data Role = AsBoss Party | AsEmployee Party`
- UI: handling when read int/date failed
- Model: denials should be observed by the boss/denier
- Local request numbering is not quite stable in face of async updates

