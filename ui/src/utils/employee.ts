import { Party } from "@daml/types";
import { CreateEvent } from "@daml/ledger";
import { ordString, Ord, contramap } from "fp-ts/lib/Ord";
import * as v5 from "@daml2ts/davl/lib/davl-0.0.5/DAVL/V5";

export type EmployeeSummary = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
};

export const ordEmployeeSummaryOnName: Ord<EmployeeSummary> = contramap(
  (summary: EmployeeSummary) => summary.employee,
)(ordString);

export const prettyEmployeeSummaries = (
  allocations: readonly CreateEvent<v5.EmployeeVacationAllocation>[],
): EmployeeSummary[] => {
  const staff = allocations.map(
    ({
      payload: {
        employeeRole: { employee, boss },
        remainingDays,
      },
    }) => ({ employee, boss, remainingVacationDays: remainingDays }),
  );
  staff.sort(ordEmployeeSummaryOnName.compare);
  return staff;
};
