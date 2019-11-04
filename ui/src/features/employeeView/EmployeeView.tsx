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

  const vacations = useSelector((state: RootState) => state.employeeView.vacations);

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
        vacations={vacations.upcoming}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
      <VacationListSegment
        header='Past Vacations'
        viewer='employee'
        vacations={vacations.past}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
    </Segment.Group>
  );
}

export default EmployeeView;
