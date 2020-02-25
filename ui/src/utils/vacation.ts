import { Party, ContractId } from "@daml/types";
import { CreateEvent } from "@daml/ledger";
import * as v4 from "@daml2ts/davl/lib/davl-0.0.4/DAVL";
import * as v5 from "@daml2ts/davl/lib/davl-0.0.5/DAVL/V5";
import { contramap, Ord, ordString, getDualOrd } from "fp-ts/lib/Ord";
import { partition } from "fp-ts/lib/Array";
import moment from "moment";

export type Vacation = {
  contractId: ContractId<unknown>;
  employee: Party;
  boss: Party;
  fromDate: string;
  toDate: string;
  version: "v4" | "v5";
};

// TODO(MH): Ideally, `VacationCreateEvent` and `VacationRequestCreateEvent`
// would set the third parameter of `CreateEvent` as well. Unfortunately,
// @daml/react does not properly use this parameter yet. This will be fixed in
// the next release.
type VacationCreateEvent =
  | CreateEvent<v4.Vacation, undefined>
  | CreateEvent<v5.Vacation, undefined>;

type VacationRequestCreateEvent =
  | CreateEvent<v4.VacationRequest, undefined>
  | CreateEvent<v5.VacationRequest, undefined>;

export const makeVacation = <T extends object>(
  contract: VacationCreateEvent,
): Vacation => {
  let version: Vacation["version"];
  switch (contract.templateId) {
    case v4.Vacation.templateId:
      version = "v4";
      break;
    case v5.Vacation.templateId:
      version = "v5";
      break;
    default:
      // NOTE(MH): This is the fallout of the above comment on the third
      // paramter of `CreateEvent`.
      throw Error(`Invalid template id for Vacation: ${contract.templateId}`);
  }
  const vacation = contract.payload;
  return {
    contractId: contract.contractId,
    employee: vacation.employeeRole.employee,
    boss: vacation.employeeRole.boss,
    fromDate: vacation.fromDate,
    toDate: vacation.toDate,
    version,
  };
};

export const makeVacationFromRequest = <T extends object>(
  contract: VacationRequestCreateEvent,
): Vacation => {
  let version: Vacation["version"];
  switch (contract.templateId) {
    case v4.VacationRequest.templateId:
      version = "v4";
      break;
    case v5.VacationRequest.templateId:
      version = "v5";
      break;
    default:
      // NOTE(MH): This is the fallout of the above comment on the third
      // paramter of `CreateEvent`.
      throw Error(
        `Invalid template id for VacationRequest: ${contract.templateId}`,
      );
  }
  const vacation = contract.payload.vacation;
  return {
    contractId: contract.contractId,
    employee: vacation.employeeRole.employee,
    boss: vacation.employeeRole.boss,
    fromDate: vacation.fromDate,
    toDate: vacation.toDate,
    version,
  };
};

export const vacationLength = (vacation: {
  fromDate: string;
  toDate: string;
}): number => {
  const fromDate = moment(vacation.fromDate, "YYYY-MM-DD");
  const toDate = moment(vacation.toDate, "YYYY-MM-DD");
  return Math.round(moment.duration(toDate.diff(fromDate)).asDays()) + 1;
};

export const ordVacationOnFromDate: Ord<Vacation> = contramap(
  (vacation: Vacation) => vacation.fromDate,
)(ordString);

export type Vacations = {
  upcoming: Vacation[];
  past: Vacation[];
};

export const emptyVacations: Vacations = {
  upcoming: [],
  past: [],
};

export const prettyRequests = (
  requestContracts: readonly VacationRequestCreateEvent[],
): Vacation[] => {
  const requests: Vacation[] = requestContracts.map(makeVacationFromRequest);
  requests.sort(ordVacationOnFromDate.compare);
  return requests;
};

export const splitVacations = (
  vacationContracts: readonly VacationCreateEvent[],
) => {
  const today = moment().format("YYYY-MM-DD");
  const vacations = vacationContracts.map(makeVacation);
  const { left: upcoming, right: past } = partition(
    (vacation: Vacation) => vacation.fromDate <= today,
  )(vacations);
  upcoming.sort(ordVacationOnFromDate.compare);
  past.sort(getDualOrd(ordVacationOnFromDate).compare);
  return { upcoming, past };
};
