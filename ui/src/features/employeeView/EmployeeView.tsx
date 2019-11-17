import React, { useEffect, useMemo } from 'react';
import Requests from './Requests';
import { useDispatch, useSelector } from 'react-redux';
import { loadSummary } from './employeeViewReducer';
import { RootState } from '../../app/rootReducer';
import SummaryView from './SummaryView';
import { Segment } from 'semantic-ui-react';
import VacationListSegment from '../../components/VacationListSegment';
import { toast } from 'react-semantic-toasts';
import { getLedger } from '../../app/store';
import { useQuery } from '../../app/damlReducer';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { splitVacations } from '../../utils/vacation';


const EmployeeView: React.FC = () => {
  const dispatch = useDispatch();
  useEffect(() => { dispatch(loadSummary()); }, [dispatch]);

  const summary = useSelector((state: RootState) => state.employeeView.summary);
  const loadingSummary = useSelector((state: RootState) => state.employeeView.loadingSummary);

  const party = useSelector(getLedger).party;
  const query = useMemo(() => ({employeeRole: {employee: party}}), [party]);
  const {loading: loadingVacations, contracts: vacationContracts} = useQuery(v3.Vacation, query);
  const vacations = splitVacations(vacationContracts);


  const handleCancelVacation = () => toast({
    title: 'Not yet implemented',
    type: 'error',
    description: 'Canceling vacation is not yet implemented.',
    time: 0,
  });

  return (
    <Segment.Group>
      {summary ? <SummaryView {...summary} loading={loadingSummary} /> : <p>Loading summary...</p>}
      <Requests />
      <VacationListSegment
        header='Upcoming Vacations'
        loading={loadingVacations}
        viewer='employee'
        vacations={vacations.upcoming}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
      <VacationListSegment
        header='Past Vacations'
        loading={loadingVacations}
        viewer='employee'
        vacations={vacations.past}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
    </Segment.Group>
  );
}

export default EmployeeView;
