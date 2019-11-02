import React from 'react';
import EmployeeInfoView, { Props as ViewProps } from './EmployeeInfoView';
import Ledger from '../ledger/Ledger';
import { EmployeeVacationAllocation } from '../daml/DAVL';

type Props = {
  ledger: Ledger;
}

/**
 * React component to control the `MainView`.
 */
const EmployeeInfoController: React.FC<Props> = ({ledger}) => {
  const [viewProps, setViewProps] = React.useState<ViewProps | undefined>(undefined);

  const loadViewProps = React.useCallback(async () => {
    try {
      const employeeVacationAllocation = await ledger.pseudoFetchByKey(EmployeeVacationAllocation, {employeeRole: {employee: ledger.party()}});
      setViewProps({
        employeeVacationAllocation: employeeVacationAllocation.data,
      });
    } catch (error) {
      alert("Unknown error:\n" + error);
    }
  }, [ledger]);

  React.useEffect(() => { loadViewProps(); }, [loadViewProps]);
  // React.useEffect(() => {
  //   const interval = setInterval(loadAllUsers, 5000);
  //   return () => clearInterval(interval);
  // }, [loadAllUsers]);

  return viewProps ? <EmployeeInfoView {...viewProps} /> : <p>Loading employee info...</p>;
}

export default EmployeeInfoController;
