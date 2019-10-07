
# Initial thoughts for V2

Our goal is to learn how to upgrade the existing contracts from the V1 model,
so let's not make the V2 step too big!

### Proposal

- Fix signatories on `Denial` contract
- Simplify the workflow for creating holiday allocation, by having employees registered with their boss.
- Model the company as an explicit party, which delegates holiday-management authority to bosses.
    - The company will be the signatory on the `Holiday` and `Vacation` contracts.
- Delay selection of `Holiday` allocation used to pay for a `Vacation` until a request is approved.
- Support all non-core workflow (such as `giftNdays`) within the model, as choices of some template.
- Support multi-day holiday requests which are approved/denied atomically.
- Support visibility of the booked vacation ledger to all employees in the company.

### NEGA-Proposal

Let's not do the following, at least not for V2

- Fractional holidays

