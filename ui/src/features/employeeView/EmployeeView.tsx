import React from "react";
import Requests from "./Requests";
import SummaryView from "./SummaryView";
import { Segment } from "semantic-ui-react";
import VacationListSegment from "../../components/VacationListSegment";
import { toast } from "react-semantic-toasts";
import { useStreamQuery, useParty, useStreamFetchByKey } from "@daml/react";
import v4 from "@daml.js/davl-0.0.4";
import v5 from "@daml.js/davl-0.0.5";
import { VacationCreateEvent, splitVacations } from "../../utils/vacation";
import { EmployeeSummary } from "../../utils/employee";

const EmployeeView: React.FC = () => {
  const party = useParty();
  const allocation = useStreamFetchByKey(
    v5.DAVL.V5.EmployeeVacationAllocation,
    () => party,
    [party],
  );
  let summary: EmployeeSummary | null = null;
  if (allocation.contract) {
    const {
      payload: {
        employeeRole: { employee, boss },
        remainingDays,
      },
    } = allocation.contract;
    summary = {
      employee,
      boss,
      remainingVacationDays: remainingDays,
    };
  }

  const {
    loading: loadingVacationsV4,
    contracts: vacationContractsV4,
  } = useStreamQuery(
    v4.DAVL.Vacation,
    () => ({ employeeRole: { employee: party } }),
    [party],
  );
  const {
    loading: loadingVacationsV5,
    contracts: vacationContractsV5,
  } = useStreamQuery(
    v5.DAVL.V5.Vacation,
    () => ({ employeeRole: { employee: party } }),
    [party],
  );
  const loadingVacations = loadingVacationsV4 || loadingVacationsV5;
  const contracts = (vacationContractsV4 as VacationCreateEvent[]).concat(vacationContractsV5 as VacationCreateEvent[]);
  const vacations = splitVacations(contracts);

  const handleCancelVacation = () =>
    toast({
      title: "Not yet implemented",
      type: "error",
      description: "Canceling vacation is not yet implemented.",
      time: 0,
    });

  return (
    <Segment.Group>
      {summary ? (
        <SummaryView {...summary} loading={allocation.loading} />
      ) : (
        <p>Loading summary...</p>
      )}
      <Requests employeeSummary={summary} />
      <VacationListSegment
        header="Upcoming Vacations"
        loading={loadingVacations}
        viewer="employee"
        vacations={vacations.upcoming}
        onClickVacation={handleCancelVacation}
        icon="cancel"
      />
      <VacationListSegment
        header="Past Vacations"
        loading={loadingVacations}
        viewer="employee"
        vacations={vacations.past}
        onClickVacation={handleCancelVacation}
        icon="cancel"
      />
    </Segment.Group>
  );
};

export default EmployeeView;
