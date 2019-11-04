import React, { useEffect } from 'react';
import Requests from './Requests';
import { useDispatch, useSelector } from 'react-redux';
import { loadAll } from './employeeViewReducer';
import { RootState } from '../../app/rootReducer';
import SummaryView from './SummaryView';
import { Segment } from 'semantic-ui-react';
import VacationListSegment from '../../components/VacationListSegment';
import { toast } from 'react-semantic-toasts';

const EmployeeView: React.FC = () => {
  const dispatch = useDispatch();
  useEffect(() => { dispatch(loadAll()); }, [dispatch]);

  const upcomingVacations = useSelector((state: RootState) => state.employeeView.upcomingVacations);
  const pastVacations = useSelector((state: RootState) => state.employeeView.pastVacations);

  const summary = useSelector((state: RootState) => state.employeeView.summary);

  const handleCancelVacation = () => toast({
    title: 'Not yet implemented',
    type: 'error',
    description: 'Canceling vacation is not yet implemented.',
    time: 0,
  });

  return (
    <Segment.Group>
      {summary ? <SummaryView {...summary} /> : <p>Loading summary...</p>}
      <Requests />
      <VacationListSegment
        header='Upcoming Vacations'
        viewer='employee'
        vacations={upcomingVacations}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
      <VacationListSegment
        header='Past Vacations'
        viewer='employee'
        vacations={pastVacations}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
    </Segment.Group>
  );
}

export default EmployeeView;
