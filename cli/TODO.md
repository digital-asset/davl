
### Request(finish):
- UI: as boss, see list of requests, with stable counting index
- UI: as employee, see list of requests

### Deny:
- Denial: type, ledger-trans
- UI: deny (take index, default 0) + text
- Agg: +count denials
- UI: as boss/employee, see list of denials (can do nothing - one day clear)

### Approve:
- Vacation: type, ledger-trans
- UI: approve (take index from request list)
- Agg: + count vacations
- UI: as boss/employee, see list of vacations

### Misc:
- date support in CLI, something better than days-since-epoch!
- factoring common code
- make aggregation code less of a pain
- UI: boss/employee mode: `data Role = AsBoss Party | AsEmployee Party`
