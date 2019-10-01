
### Request(finish):
- UI: as boss, see list of requests, with stable counting index
- UI: as employee, see list of requests
DONE

### Deny:
- Denial: type, ledger-trans
- UI: deny (take index, default 0) + text
- Agg: +count denials
DONE

- UI: as boss/employee, see list of denials (can do nothing - one day clear)

### Approve:
- Vacation: type, ledger-trans
- UI: approve (take index from request list)
- Agg: + count vacations
- UI: as boss/employee, see list of vacations

### Misc:
- fix UI to make request only on allocations with no pending request
- date support in CLI, something better than days-since-epoch!
- factoring common code
- make aggregation code less of a pain
- UI: boss/employee mode: `data Role = AsBoss Party | AsEmployee Party`
- UI: handling when read int/date failed
- Model: denials should be observed by the boss/denier
- Local request numbering is not quite stable in face of async updates

