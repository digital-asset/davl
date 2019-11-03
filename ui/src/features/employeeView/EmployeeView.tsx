import React, { useEffect } from 'react';
import Ledger from '../../ledger/Ledger';
import RequestVacationController from './RequestContainer';
import Approved from './Approved';
import Pending from './Pending';
import { useDispatch, useSelector } from 'react-redux';
import { loadAll } from './reducer';
import { RootState } from '../../app/rootReducer';
import SummaryView from './SummaryView';

type Props = {
  ledger: Ledger;
}

const EmployeeView: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  useEffect(() => { dispatch(loadAll(ledger)); }, [dispatch, ledger]);

  const summary = useSelector((state: RootState) => state.employeeView.summary);

  return (
    <>
      {summary ? <SummaryView {...summary} /> : <p>Loading summary...</p>}
      <RequestVacationController ledger={ledger} />
      <Approved ledger={ledger} />
      <Pending ledger={ledger} />
    </>
  );
}

export default EmployeeView;
