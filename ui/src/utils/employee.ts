import { Party } from '@digitalasset/daml-json-types';
import { CreateEvent } from '@digitalasset/daml-ledger-fetch';
import { ordString, Ord, contramap } from 'fp-ts/lib/Ord';
import * as v3 from '../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';

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
