
# What we learned about the V1 model

We learnt the following while exposing the functionality of the V1 DAVL model over a simple CLI.


### Give/Claim workflow is clumbsy

The current workflow for generating new `Holiday` allocation is clumbsy. The boss has to `Gift` the employee, who then has to `Claim` the `Gift`.
This doesn't match the natural intuition of holiday allocation.
But we do it because it is necessary for an employee to be a signatory of the allocation.

However, a better model would be for the employee to register with a boss, which then delegates the rights for the boss to create new `Holiday` allocation on behalf of that employee.
This also answers the question of who can be who's boss!
We will probably do this in next version.


### Getting the correct signatories is important

Currently a boss cannot see the `Denial` contracts stemming from the `Deny` choices they exercised. This is a shortcoming.
A simple fix would be to add the boss as an observer on the `Denial` contracts, but this is not enough!
The boss should in fact be a signatory on a `Denial` contract if the contract is to have any worth to the employee (i.e. as a record of unfair denials), because without a signature from the boss, then the denial can be faked - the contract created by the employee alone.


### Be careful when referencing an external contract by ContractId

Because, when that other contract has been _spent_ (archived), a `fetch` on the contractId will fail.

This causes an small issue in the current model because of the way multiple `Request`s can be created which reference the same `Holiday` allocation.
We like this model; the behaviour is flexible. It allows functionality such as an employee making a holiday request for one of a set of dates, but allowing the boss to make the final choice of the precise date.

However, in the current model, a `Request` can only be exercised by an `Approve` or `Deny` choice. Of course we want the `approve` to fail, or else we have a situation where the same `Holiday` allocation is spent twice. But unfortunately, the `Deny` choice also fails, which is counter intuitive. This occurs because the definition of `Deny` make a call to `fetch allocationId`, a call which is entirely unnecessary. However, the model is not completely broken, the employee can simply archive the now useless `Request` contract.
