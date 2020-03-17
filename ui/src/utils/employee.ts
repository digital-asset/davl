import { Party } from "@daml/types";
import { CreateEvent } from "@daml/ledger";
import { ordString, Ord, contramap } from "fp-ts/lib/Ord";
import v4 from "@daml.js/davl-0.0.4";
import v5 from "@daml.js/davl-0.0.5";

export type EmployeeSummary = {
  employee: Party;
  boss: Party;
  remainingVacationDays: string;
};

export type EmployeeVacationAllocation =
  | CreateEvent<v4.DAVL.EmployeeVacationAllocation>
  | CreateEvent<v5.DAVL.V5.EmployeeVacationAllocation>

export const ordEmployeeSummaryOnName: Ord<EmployeeSummary> = contramap(
  (summary: EmployeeSummary) => summary.employee,
)(ordString);

export const prettyEmployeeSummaries = (
  allocations: readonly EmployeeVacationAllocation[],
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
