import { AnyContractId, Party, ContractId } from '../ledger/Types';
import * as DAVL from '../daml/DAVL';
import { contramap, Ord, ordString } from 'fp-ts/lib/Ord';
import moment from 'moment';

export type Vacation = {
  contractId: AnyContractId;
  employee: Party;
  boss: Party;
  fromDate: string;
  toDate: string;
}

export const makeVacation = <T extends {}>({contractId}: ContractId<T>, {employeeRole: {employee, boss}, fromDate, toDate}: DAVL.Vacation) =>
  ({contractId, employee, boss, fromDate, toDate})

export const vacationLength = (vacation: {fromDate: string; toDate: string}): number => {
  const fromDate = moment(vacation.fromDate, 'YYYY-MM-DD');
  const toDate = moment(vacation.toDate, 'YYYY-MM-DD');
  return Math.round(moment.duration(toDate.diff(fromDate)).asDays()) + 1;
}

export const ordVacationOnFromDate: Ord<Vacation> =
  contramap((vacation: Vacation) => vacation.fromDate)(ordString);
