import React from 'react';
import Summary from './SummaryView';
import Ledger from '../../ledger/Ledger';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import { loadSummary } from './reducer';

type Props = {
  ledger: Ledger;
}

/**
 * React component to control the `MainView`.
 */
const SummaryContainer: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const summary = useSelector((state: RootState) => state.employeeView.summary);

  React.useEffect(() => { dispatch(loadSummary(ledger)); }, [dispatch, ledger]);

  return summary
    ? <Summary {...summary} />
    : <p>Loading employee info...</p>;
}

export default SummaryContainer;
