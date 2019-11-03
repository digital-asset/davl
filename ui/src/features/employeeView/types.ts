import { Party } from '../../ledger/Types';

export type Summary = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
}
