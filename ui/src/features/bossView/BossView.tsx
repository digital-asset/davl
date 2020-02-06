import React from 'react';
import VacationListSegment from '../../components/VacationListSegment';
import { Vacation, prettyRequests, splitVacations } from '../../utils/vacation';
import { Segment } from 'semantic-ui-react';
import Staff from './Staff';
import { useStreamQuery, useExercise, useParty } from '../../daml-react-hooks';
import * as v3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { toast } from 'react-semantic-toasts';

const BossView: React.FC = () => {
  const party = useParty();

  const {loading: loadingVacations, contracts: vacationContracts} =
    useStreamQuery(v3.Vacation, () => ({employeeRole: {boss: party}}), [party]);
  const vacations = splitVacations(vacationContracts);

  const {loading: loadingRequests, contracts: requestsContracts} =
    useStreamQuery(v3.VacationRequest, () => ({vacation: {employeeRole: {boss: party}}}), [party]);
  const requests = prettyRequests(requestsContracts);

  const [exerciseApproveRequest] =
    useExercise(v3.VacationRequest.VacationRequest_Accept);

  const handleApproveRequest = async (vacation: Vacation) => {
    await exerciseApproveRequest(vacation.contractId, {});
    toast({
      title: 'Success',
      type: 'success',
      time: 3000,
      description: 'Request successfully approved.',
    });
  }

  return (
    <Segment.Group>
      <Staff />
      <VacationListSegment
        header='Pending Vacation Approvals'
        loading={loadingRequests}
        viewer='boss'
        vacations={requests}
        onClickVacation={handleApproveRequest}
        icon='check'
      />
      <VacationListSegment
        header='Upcoming Vacations'
        loading={loadingVacations}
        viewer='boss'
        vacations={vacations.upcoming}
        onClickVacation={() => { /* do nothing */ }}
        icon='info'
      />
      <VacationListSegment
        header='Past Vacations'
        loading={loadingVacations}
        viewer='boss'
        vacations={vacations.past}
        onClickVacation={() => { /* do nothing */ }}
        icon='info'
      />
    </Segment.Group>
  );
}

export default BossView;
