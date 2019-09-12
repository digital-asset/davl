
# What we should model?

### Terminology: *Holiday* / *Vacation*

*Are these works interchangeable (just US/UK variations), or will they mean subtlely different concepts in our model?* For now, we stick with Holiday.

### *v1* / *v2*

In the following, the terms *v1* and *v2* are short-hands to indicate the first MVP and some later version. In general, we should explicitly aim to always be as small-as-possible for out MPV, especially if we know *for sure* we will want the v2 feature. We want to feel all the pain of upgrade!


### Features to be modeled in DAML

In order of increasing importance:

- Querying holiday-allocations and booked holidays
- Workflow for holiday requests
- Modelling holidays as items of value


### Who are the Parties?

We have a notion of *worker* and *boss*. The worker is the one who feels the benefit of a holiday; the boss is the one who feels the obligation.


### Modelling holidays as items of value

We Need to model:

- `HolidayAllocation`
- `BookedHoliday`

The worker regards holidays (both holiday allocation and booked holidays) as items of value which they own. The worker may exchange `HolidayAllocation` for `BookedHoliday` (subject to boss approval), which then gives the worker the right not to work on a given day. Holidays are rather like cash, but cash which can only be spent on one kind of thing.

#### `HolidayAllocation`

This can be modelled as a contract. It should signed by both boss and worker.
The boss must sign because she has the obligation. The worker mus sign because he has the right, which cannot be unilaterally removed by the boss.
We might regard holiday allocation as allocated only for a specific year or other time period - but lets make this a **v2** feature.


#### Holiday Units

What the smallest unit of holiday? For *v1*, let's make it single day. Perhaps in *v2* we can support half or quarter days.
We could choose to model the `HolidayAllocation` and `BookedHoliday` contracts including a given number of days. But there is no need for this complication. Instead we can have a separate contract for each separate day. Perhaps in the future there will be some reason to keep holidays aggregated together in a single contract - but lets wait and see if our hand is forced this way.


#### `BookedHoliday`

This is a holiday booked for a certain day. A `BookedHoliday` is what the worker obtains when he *spends* a `HolidayAllocation`.

A `BookedHoliday` is not automatically transferable to other dates. When a worker has a `BookedHoliday`, he has the assurance that his right to take this day off work cannot be unilaterally revoked by the boss. On the other hand, he has the obligation to take the holiday on the stated day. Perhaps in v2 we allow the feature to ask the boss to reschedule.

A holiday can only transition from allocation to booked if both boss and worker agree. We will use a standard DAML proposal mechanism for request/accept. We choose in v1 to have the worker request and the boss accept, but there is no reason in principal why the proposal couldn't be in the opposite direction.

The important thing is that the boss can accept or deny (perhaps too many other people have booked holidays on that same day). But what is to stop the boss from always denying? This is a real issue - But one we choose to ignore in v1.  Perhaps in v2 we add a given date for every holiday allocation, past which a holiday request becomes undeniable!


### Workflow for holiday requests

We already touched on the basic workflow. Worker requests; Boss accepts.
This would be performed using a `Request` choice of a `HolidayAllocation`.
We will need informational contracts to carry the dates being requested,
and in the case of a denied request, some reason given by the boss.

- `HolidayRequest`
- `HolidayDenial`

We already touched on the issue of what stops a boss denying every request & how this might be handled on v2. But there is the additional issue of what will incentivize the boss from making any kind of reply... again we might imagine v2 extensions which automatically accept a request after a given time has elapsed (perhaps a chance for triggers/bots here).


### Were does holiday come from?

Holiday can be regarded as a *gift* from the boss to the worker. In reality, it will correspond to the amount stated on the worker's employment contract. Each year, or month, or whatever, the boss must allocate the required number of days to the worker.

Because `HolidayAllocation` must be signed by boss and worker, this gift will have to go through the normal propose/accept mechanism. But there is never a reason or way for the worker not to accept. Perhaps another chance for trigger automation in v2.

Note: we seem to have already multiple instances of the propose/accept pattern. Shoukd these be unified using generic template? Maybe! v2.


### Querying holiday-allocations and booked holidays

What questions should the system support & who can ask then? How about for v1...

- How many days of holiday allocation does worker W have? *This can be asked only by W or their boss. This information should not be available for anyone else.*

- What days has worker W booked? *Anyone can ask this*

- Who has booked a given date? *Again, anyone can ask this*

More:

- How many days of allocated-holiday remain unbooked. *Would be nice if anyone can ask this, but this might be tricky because of the privacy of holiday allocation - v2 feature for sure*

## Further modelling issues

- When are contracts archived?
- Different bosses?
- Need for aggregation, contract keys?

### When are contracts archived?

A `HoldayAllocation` will be archived when it is spent on a `BookedHoliday`. There is never any reason to archive a `BookedHoliday` (until we support re-schedule in v2), and so it can stand as a permanent record of that holiday taken or to-be taken.

But is this the correct approach? And does it matter anyway? *What would happen if the boss/worker got together and arranged for such contracts to be archived, as the DAML model allows.*

Perhaps this kind of archival makes it difficult to ask questions about the past - unless the ledger ensures that even archived contracts are available forever over the Leger API.

### Different bosses?

How do different bosses fit into this system? Can a worker have holiday allocated from multiple bosses? Does each boss represent a separate space, with privacy between spaces.. i.e. imagine each boss as being the representative for a different company.

### Need for aggregation, contract keys?

Is there a reason to explicitly model the aggregation of holidays? If we do this, then we might want to ensure that there is only one contact per worker/boss pair or whatever, and so need contract keys. *But are we forced into doing this, or can we keep it simpler?* At least, let's be simpler for v1.
