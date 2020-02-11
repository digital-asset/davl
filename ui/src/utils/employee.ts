import { Party } from '@daml/types';
import { CreateEvent } from '@daml/ledger';
import { ordString, Ord, contramap } from 'fp-ts/lib/Ord';
import * as v3 from '@daml2ts/davl-v3/lib/davl-v3/DAVL';

export type EmployeeSummary = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
}

export const ordEmployeeSummaryOnName: Ord<EmployeeSummary> =
  contramap((summary: EmployeeSummary) => summary.employee)(ordString);

export const prettyEmployeeSummaries = (allocations: CreateEvent<v3.EmployeeVacationAllocation>[]): EmployeeSummary[] => {
  const staff = allocations.map(({payload: {employeeRole: {employee, boss}, remainingDays}}) =>
    ({employee, boss, remainingVacationDays: remainingDays}));
  staff.sort(ordEmployeeSummaryOnName.compare);
  return staff;
}
