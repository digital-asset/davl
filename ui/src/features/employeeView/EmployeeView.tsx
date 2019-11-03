import React, { useEffect } from 'react';
import Ledger from '../../ledger/Ledger';
import RequestView from './RequestView';
import Approved from './Approved';
import Pending from './Pending';
import { useDispatch, useSelector } from 'react-redux';
import { loadAll } from './reducer';
import { RootState } from '../../app/rootReducer';
import SummaryView from './SummaryView';
import { Segment } from 'semantic-ui-react';

type Props = {
  ledger: Ledger;
}

const EmployeeView: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  useEffect(() => { dispatch(loadAll(ledger)); }, [dispatch, ledger]);

  const summary = useSelector((state: RootState) => state.employeeView.summary);

  return (
    <Segment.Group>
      {summary ? <SummaryView {...summary} /> : <p>Loading summary...</p>}
      <RequestView ledger={ledger} />
      <Pending ledger={ledger} />
      <Approved ledger={ledger} />
    </Segment.Group>
  );
}

export default EmployeeView;
