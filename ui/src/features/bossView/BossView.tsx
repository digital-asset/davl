import React from 'react';
import VacationListSegment from '../../components/VacationListSegment';
import { useDispatch, useSelector } from 'react-redux';
import { approveRequest } from './bossViewReducer';
import { Vacation, prettyRequests, splitVacations } from '../../utils/vacation';
import { Segment } from 'semantic-ui-react';
import Staff from './Staff';
import { useQuery } from '../../app/damlReducer';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { getLedger } from '../../app/store';

const BossView: React.FC = () => {
  const dispatch = useDispatch();
  const party = useSelector(getLedger).party;

  const {loading: loadingVacations, contracts: vacationContracts} =
    useQuery(v3.Vacation, () => ({employeeRole: {boss: party}}), [party]);
  const vacations = splitVacations(vacationContracts);

  const {loading: loadingRequests, contracts: requestsContracts} =
    useQuery(v3.VacationRequest, () => ({vacation: {employeeRole: {boss: party}}}), [party]);
  const requests = prettyRequests(requestsContracts);

  const handleApproveRequest = (vacation: Vacation) =>
    dispatch(approveRequest(vacation.contractId));

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
        onClickVacation={() => {}}
        icon='info'
      />
      <VacationListSegment
        header='Past Vacations'
        loading={loadingVacations}
        viewer='boss'
        vacations={vacations.past}
        onClickVacation={() => {}}
        icon='info'
      />
    </Segment.Group>
  );
}

export default BossView;
