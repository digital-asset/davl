import { Party } from '../ledger/Types';
import { ordString, Ord, contramap } from 'fp-ts/lib/Ord';

export type EmployeeSummary = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
}

export const ordEmployeeSummaryOnName: Ord<EmployeeSummary> =
  contramap((summary: EmployeeSummary) => summary.employee)(ordString);
