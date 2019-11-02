import React from 'react';
import EmployeeInfoView from './EmployeeInfoView';
import Ledger from '../../ledger/Ledger';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import { load as loadEmployeeInfo } from './reducer';

type Props = {
  ledger: Ledger;
}

/**
 * React component to control the `MainView`.
 */
const EmployeeInfoController: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const employeeInfo = useSelector((state: RootState) => state.employeeInfo);

  React.useEffect(() => { dispatch(loadEmployeeInfo(ledger)); }, [dispatch, ledger]);

  return employeeInfo
    ? <EmployeeInfoView {...employeeInfo} />
    : <p>Loading employee info...</p>;
}

export default EmployeeInfoController;
