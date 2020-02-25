import React from "react";
import VacationListSegment from "../../components/VacationListSegment";
import { Vacation, prettyRequests, splitVacations } from "../../utils/vacation";
import { Segment } from "semantic-ui-react";
import Staff from "./Staff";
import { useStreamQuery, useExercise, useParty } from "@daml/react";
import * as v4 from "@daml2ts/davl/lib/davl-0.0.4/DAVL";
import * as v5 from "@daml2ts/davl/lib/davl-0.0.5/DAVL/V5";
import { toast } from "react-semantic-toasts";

const BossView: React.FC = () => {
  const party = useParty();

  const vacationsV4 = useStreamQuery(
    v4.Vacation,
    () => ({ employeeRole: { boss: party } }),
    [party],
  );
  const vacationsV5 = useStreamQuery(
    v5.Vacation,
    () => ({ employeeRole: { boss: party } }),
    [party],
  );
  const vacations = splitVacations([
    ...vacationsV4.contracts,
    ...vacationsV5.contracts,
  ]);
  const loadingVacations = vacationsV4.loading || vacationsV5.loading;

  const requestsV4 = useStreamQuery(
    v4.VacationRequest,
    () => ({ vacation: { employeeRole: { boss: party } } }),
    [party],
  );
  const requestsV5 = useStreamQuery(
    v5.VacationRequest,
    () => ({ vacation: { employeeRole: { boss: party } } }),
    [party],
  );
  const requests = prettyRequests([
    ...requestsV4.contracts,
    ...requestsV5.contracts,
  ]);
  const loadingRequests = requestsV4.loading || requestsV5.loading;

  const [exerciseApproveRequestV4] = useExercise(
    v4.VacationRequest.VacationRequest_Accept,
  );
  const [exerciseApproveRequestV5] = useExercise(
    v5.VacationRequest.VacationRequest_Accept,
  );

  const handleApproveRequest = async (vacation: Vacation) => {
    switch (vacation.version) {
      case "v4":
        await exerciseApproveRequestV4(vacation.contractId, {});
        break;
      case "v5":
        await exerciseApproveRequestV5(vacation.contractId, {});
        break;
    }
    toast({
      title: "Success",
      type: "success",
      time: 3000,
      description: "Request successfully approved.",
    });
  };

  return (
    <Segment.Group>
      <Staff />
      <VacationListSegment
        header="Pending Vacation Approvals"
        loading={loadingRequests}
        viewer="boss"
        vacations={requests}
        onClickVacation={handleApproveRequest}
        icon="check"
      />
      <VacationListSegment
        header="Upcoming Vacations"
        loading={loadingVacations}
        viewer="boss"
        vacations={vacations.upcoming}
        onClickVacation={() => {
          /* do nothing */
        }}
        icon="info"
      />
      <VacationListSegment
        header="Past Vacations"
        loading={loadingVacations}
        viewer="boss"
        vacations={vacations.past}
        onClickVacation={() => {
          /* do nothing */
        }}
        icon="info"
      />
    </Segment.Group>
  );
};

export default BossView;
