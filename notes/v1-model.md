

# V1 Model for `DAVL` holiday tracking application, driven by `DAML`

Following a general acceptance of the ideas contained in the initial thoughts
[here](/notes/model-early-thoughts.md),
let's specify what is supported in version 1 in more detail.

This document should be read in conjuction with the DAML model
[here](/daml/Model.daml).

Here are some things we learned while implementing a simple CLI for the V1 model
[here](/notes/v1-learnings.md).

Here is the discussion following the v1 demo on Friday 2019-10-04
[here](/notes/v1-post-demo-discussion.md).


## Terminology

Grounding the DAML model terminology in the real world...

### Roles

- `boss` - one who provides `Holiday` allocation to an employee.
- `employee` - one who spends holiday allocation on booked `Vacation`.

### Contracts

- `Holiday` - A day of holiday allocation.
- `Vacation` - A booked vacation day, _bought_ with a holiday allocation.
- `Gift` -  The means by which holiday allocation is provided from boss to employee.
- `Request` - ... from an employee to _spend_ a `Holiday` on a specific dated `Vacation`.
- `Denial` - ... from a boss regarding a vacation `Request`.

### Choices

- `Claim` - by an employee, to turn a `Gift` into a `Holiday`.
- `Approve` - by a boss to turn a `Request` into a `Vacation`.
- `Deny` - by a boss, to turn a `Request` into a `Denial`.

## Workflow

### As boss

- Offer a holiday `Gift` to an employee worth N-days of `Holiday` allocation. _(create*)_
- `Deny` a `Request` for a dated `Vacation`. _(exercise)_
- `Approve` a `Request` for a dated `Vacation`. _(exercise)_

### As Employee

- `Claim` (all) holiday `Gift` days from a given boss. _(exercise*)_
- Make a `Request` (to a specific boss) to spend one day of `Holiday` allocation (which must not currently have any associated pending request) on a dated `Vacation`. _(create)_


### Bad Workflow prevented by authorization

- `Holiday` allocation cannot be unilaterally revoked by a boss (or employee).
- Only the employee can request to spend their holiday allocation.
- Booked `Vacation` cannot be unilaterally revoked by a boss (or employee).
- Evidence of `Denial`s cannot be hidden by a boss.

### Additional Workflows

Possible in the DAML model, but without a corresponding high-level action.

- Boss cancelling a gift before it has been claimed. _(archive)_
- Employee cancelling a request before it has been approved. _(archive)_
- Multiple pending requests for the same holiday allocation. This is a potentially useful feature to support an employee to request a vacation on one of a set of days, to be selected by the boss. We might expose this functionality in a later version.


## Queries

### As boss

- What `Gift`s from me remain unclaimed? _#days/employee_
- What `Holiday` allocations (from `Gifts` I sent) remain unspent? _#days/employee_
- Of these unspent `Holiday` allocation, which have a pending `Request` associated with them? _#days/employee_
- What `Request`s must I attend to? _list of date,employee_
- What `Denial`s have I made? _list of date,employee_
- What `Vacation`s have I approved? _list of date,employee_

### As Employee

- What `Gift`s do I have unclaimed? _#days/boss_
- What unspent `Holiday` days do I have? _#days/boss_
- What `Vacation`s have I booked? _list of date,boss_
- What `Denial`s have I received? _list of date,boss_

### Privacy aspects

- Only a boss/employee pair can see the gifts and allocations between them.


## Aspects not modelled

- Boss/employee relationship.
- Atomic requests for multiple vacation days.
- Fractional vacation days.
- Vacation schedule visibility for related team members (whatever that means).

### Missing validation checks

- Anyone can be a boss simply by gifting some holiday.
- Someone can be their own boss.
- Employees can have multiple days vacations booked on the same day.

### Liveness issues

- A request can be ignored forever.
- A boss can deny every request.
- Employees may never spend their holiday allocation.
