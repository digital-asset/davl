
# Discussion following v1 demo

### Fraction allocation / Aggregated allocation

The V1 model does not support fractional or aggregated holiday allocation, because we explicitly chose to do the simplest thing possible. There are different ways we might extend this:

We could move from modelling `Holiday` tokens, to a holiday account, which tracks multiple holiday days. This may still be an integer amount, or we could allow it to be fractional. We must consider whether to allow negative days. And we must consider if multiple account contracts may exist for a given employee, or else ensure aggregation into a single contract always occurs.

### `Denial` signatories

We discussed who the signatories should be on a `Denial` contract. In V1 only the employee is a signatory. We agreed this is not right. The boss must be signatory, or else the contract has no value to the employee as a record of the denial. But the employee must also remain a signatory, or else the boss can unilaterally archive the contract, which is not desirable.

### Idea: Delay `Holiday` choice until `Request` is accepted.

The V1 model has issues regarding the way multiple `Request` contracts can be created which refer to the same `Holiday` allocation. It was suggested that the model might be simplified by delaying the choice of which `Holiday` allocation to spend on the `Vacation` until the `Request` has been approved.

### Boss Registration / Authority

We discussed the clumsiness in the V1 workflow for gifting new holiday allocation, which requires an explicit `Claim` step by the employee. One solution is to have the employee register with who they regard as their boss, and thus delegate the right for the boss to create them new `Holiday` allocation.

This idea of who can be who's boss could be further extended by having a top-down workflow from some super-boss, where the authority to be a boss of another, and the right to create their holiday allocation is delegated down the management hierarchy.

### UI/Model logic split

Even though the V1 model encodes all core workflow in DAML, there are still places where some workflow logic, however simple, is encoded is the UI. For instance: To gift an employee N days holiday allocation, the UI must create N separate `Holiday` contracts on the ledger. This (although trivial) logic for _gift-N_ would have to be repeated in every different UI. To avoid this, the following modelling guideline is suggested:

> _Every user level workflow step should correspond with the choice of some template._

### Migration via central authority

Following a discussion on the challenges of migration from one model version to another, it was suggested that to make upgrading really simple, the issue of authority can be completely sidestepped by having a single `Admin` signatory as the only signatory on all contracts.

### Explicit checks vs Signatory authority

Should the model have more explicit assertions to help guide the reader? For example, when the `Request_Approve` choice is exercised, should the DAML code explicitly check that the `Holiday` allocation referenced by the `Request` belongs to the employee making the request?

Indeed it would be terrible if it were possible for one employee to _spend_ another employees holiday allocation - and in fact this is not possible in the current model, because the `Request_Approve` archives the holiday allocation being spent, and this will fail without the authority of the employee who the allocation belongs to. But would the DAML be better if this were made explicit, as well as being enforced by signatory authority?

### Role-based contracts

Should our model have the explicit notion of roles: Using a fictional situation, if my boss is Alice and she leaves, and my new boss is Angelina, I don't want to lose my existing allocation. Apparently this issue is addressed in the DAML docs, under the heading of _Role-based contracts_.
