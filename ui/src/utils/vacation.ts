import { Party, ContractId, Contract } from '@digitalasset/daml-json-types';
import * as v3 from '../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { contramap, Ord, ordString, getDualOrd } from 'fp-ts/lib/Ord';
import { partition } from 'fp-ts/lib/Array';
import moment from 'moment';

export type Vacation = {
  contractId: ContractId<unknown>;
  employee: Party;
  boss: Party;
  fromDate: string;
  toDate: string;
}

export const makeVacation = <T extends {}>(contractId: ContractId<T>, {employeeRole: {employee, boss}, fromDate, toDate}: v3.Vacation) =>
  ({contractId, employee, boss, fromDate, toDate})

export const vacationLength = (vacation: {fromDate: string; toDate: string}): number => {
  const fromDate = moment(vacation.fromDate, 'YYYY-MM-DD');
  const toDate = moment(vacation.toDate, 'YYYY-MM-DD');
  return Math.round(moment.duration(toDate.diff(fromDate)).asDays()) + 1;
}

export const ordVacationOnFromDate: Ord<Vacation> =
  contramap((vacation: Vacation) => vacation.fromDate)(ordString);

export type Vacations = {
  upcoming: Vacation[];
  past: Vacation[];
}

export const emptyVacations: Vacations = {
  upcoming: [],
  past: [],
}

export const prettyRequests = (requestContracts: Contract<v3.VacationRequest>[]): Vacation[] => {
  const requests: Vacation[] =
    requestContracts.map(({contractId, argument}) => makeVacation(contractId, argument.vacation));
  requests.sort(ordVacationOnFromDate.compare);
  return requests;
}


export const splitVacations = (vacationContracts: Contract<v3.Vacation>[]) => {
  const today = moment().format('YYYY-MM-DD');
  const vacations = vacationContracts.map((vacation) => makeVacation(vacation.contractId, vacation.argument))
  const {left: upcoming, right: past} =
    partition((vacation: Vacation) => vacation.fromDate <= today)(vacations);
  upcoming.sort(ordVacationOnFromDate.compare);
  past.sort(getDualOrd(ordVacationOnFromDate).compare);
  return {upcoming, past};
}
