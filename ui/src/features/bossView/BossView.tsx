import React from 'react';
import VacationListSegment from '../../components/VacationListSegment';
import { useDispatch, useSelector } from 'react-redux';
import { loadAll, approveRequest } from './bossViewReducer';
import { RootState } from '../../app/rootReducer';
import { Vacation } from '../../utils/vacation';
import { Segment } from 'semantic-ui-react';
import Staff from './Staff';

const BossView: React.FC = () => {
  const dispatch = useDispatch();
  React.useEffect(() => { dispatch(loadAll()); }, [dispatch]);

  const requests = useSelector((state: RootState) => state.bossView.requests);
  const vacations = useSelector((state: RootState) => state.bossView.vacations);
  const loadingRequests = useSelector((state: RootState) => state.bossView.loadingRequests);
  const loadingVacations = useSelector((state: RootState) => state.bossView.loadingVacations);

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
