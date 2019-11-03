import { AnyContractId, Party, ContractId } from '../ledger/Types';
import * as davl from '../daml/DAVL';

export type Vacation = {
  contractId: AnyContractId;
  employee: Party;
  boss: Party;
  fromDate: string;
  toDate: string;
}

export const makeVacation = <T extends {}>({contractId}: ContractId<T>, {employeeRole: {employee, boss}, fromDate, toDate}: davl.Vacation) =>
  ({contractId, employee, boss, fromDate, toDate})
