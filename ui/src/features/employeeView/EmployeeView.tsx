import React, { useEffect } from 'react';
import Ledger from '../../ledger/Ledger';
import Requests from './Requests';
import { useDispatch, useSelector } from 'react-redux';
import { loadAll } from './reducer';
import { RootState } from '../../app/rootReducer';
import SummaryView from './SummaryView';
import { Segment } from 'semantic-ui-react';
import VacationListSegment from '../../components/VacationListSegment';

type Props = {
  ledger: Ledger;
}

const EmployeeView: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  useEffect(() => { dispatch(loadAll(ledger)); }, [dispatch, ledger]);
  const vacations = useSelector((state: RootState) => state.employeeView.approved);

  const summary = useSelector((state: RootState) => state.employeeView.summary);

  const handleCancelVacation = () => alert('Canceling vacation is not yet implemented.');

  return (
    <Segment.Group>
      {summary ? <SummaryView {...summary} /> : <p>Loading summary...</p>}
      <Requests ledger={ledger} />
      <VacationListSegment
        header='Approved Vacations'
        vacations={vacations}
        onClickVacation={handleCancelVacation}
        icon='cancel'
      />
    </Segment.Group>
  );
}

export default EmployeeView;
