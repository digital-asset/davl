# Upgrading DAVL from v4 to v5


## How the v4 DAML model works

Three parties are involved in every vacation that gets booked:

1. `company`: The operator of DAVL, representing Digital Asset in our case.
1. `employee`: The employee requesting to book the vacation.
1. `boss`: The manager of the employee, who has to approve the request.

Each employee has a role contract, of template `EmployeeRole`, which is signed by both the employee and the company. This role contract has a key, which is only the employee party. The role contract also defines the boss of the employee. Each employee also has a contract carrying their remaining vacation allocation, of template `EmployeeVacationAllocation`, which also has the employee as its key. These two contracts are created via a standard proposal workflow initiated by the company. The employee is asked to accept their proposal when they try to log into the DAVL UI for the first time. If they do not accept, they cannot use DAVL.

In order to book a vacation, the employee calls the `EmployeeRole_RequestVacation` choice on their role contract. This creates a `VacationRequest` contract, which is signed by the employee and the company and gives the boss a choice to accept the request. When the boss accepts, a `Vacation` contract signed by employee, company and boss get created and the respective number of vacation days gets deducted from the employee's `VacationAllocation` contract.


## What the v5 DAML models changes

The v5 model is an exact copy of the v4 model with one addition: The v4 model does not allow for canceling vacation requests that haven't yet been approved. The v5 model fixes this problem by adding a `VacationRequest_Cancel` choice to the `VacationRequest` template, which is controlled by the employee and archives the contract. That is the only difference between v4 and v5.


## How the upgrade model and its automation work

The upgrade model consists of two templates, `UpgradeProposal` and `UpgradeAgreement`. To initiate an upgrade, the company creates an `UpgradeProposal` contract for each employee. This proposal is signed by the company and gives the employee a choice to accept. The company creates these proposals using a DAML Script that fetches all `EmployeeRole` contracts and creates an according `UpgradeProposal` for each of them.

Once the employee has accepted their `UpgradeProposal` by exercising the `UpgradeProposal_Accept` choice via the DAVL UI, the employee's `EmployeeRole` and `EmployeeVacationAllocation` contracts from the v4 model get archived and equivalent contracts from the v5 model get created. This is done as part of the `UpgradeProposal_Accept` choice.

To upgrade the `VacationRequest` and `Vacation` contracts, the company runs a DAML Trigger. Whenever this trigger finds a vacation request from the v4 model and an upgrade agreement for the same employee, it exercises the `UpgradeAgreement_UpgradeVacationRequest` choice of the agreement with the request as the argument. This replaces the v4 request with a v5 request. Whenever the trigger finds a vacation contract from the v4 model and upgrade agreements for both the employee and the boss, it exercises the `UpgradeAgreement_UpgradeVacation` choice of the boss' upgrade agreement with the vacation and employee's agreement as arguments. This replaces the v4 vacation with a v5 vacation.

To upgrade `EmployeeRoleProposal` contracts, the company could run a DAML Script which finds all v4 contracts and replaces them by v5 contracts. This is not required for us and hence not implemented.


## How the DAVL UI handles the upgrade

Since we cannot expect all employees to accept their upgrade proposal at the same times, the DAVL UI needs to run in a dual mode where it can handle v4 and v5 contracts at the same time. The UI also needs to provide a mechanism for employees to actually accept their upgrade proposals. We do this as part of the login process. Before this upgrade, when an employee clicked the "Log in" button in the UI, it sent a `fetchByKey` request to the ledger to find the employee's v4 role contract. If that lookup failed, the employee could not log in. Now, the UI instead requests the employee's v5 role contract. If that lookup fails, it requests an upgrade proposal for the employee. If that lookup succeeds, a message box is shown to ask for the employee's agreement to the upgrade. If the employee accepts, the `UpgradeProposal_Accept` choice on the upgrade proposal gets exercised. Once this exercise has succeeded, the employee is logged in. If there is neither a v5 role contract for the employee nor an upgrade proposal or if the employee does not accept the upgrade proposal, the login fails.

Once the employee is logged in, they can see a few different lists of contracts:

1. Their pending vacation requests.
1. Their approved vacations.
1. The pending vacation requests of their subordinates.
1. The approved vacations of their subordinates.
1. The vacation allocations of their subordinates.

The logged in employee can also see their own vacation allocation.

Since the DAML Trigger run by the company will automatically upgrade all of the employee's vacation requests, it is sufficient to only display v5 vacation requests for them. The employee's boss might have or not have accepted their upgrade agreement yet. Thus, it is necessary to fetch v4 and v5 vacation contracts for the employee. Some of employee's subordinates might have already accepted their upgrade proposals whereas other might not have done so. Hence, all the contracts for the employee's subordinates need to be fetched for the v4 and v5 models. When the employee approves a vacation request of one of their subordinates, the UI needs to distinguish whether that `VacationRequest` contract is from model v4 or v5 in order to call the `VacationRequest_Accept` choice from the same model.
