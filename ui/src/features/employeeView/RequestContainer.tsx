/* eslint-disable @typescript-eslint/camelcase */
import React from 'react';
import Ledger from '../../ledger/Ledger';
import RequestView from './RequestView';
import { EmployeeRole_RequestVacation, EmployeeRole } from '../../daml/DAVL';

type Props = {
  ledger: Ledger;
}

/**
 * React component to control the `MainView`.
 */
const RequestVacationController: React.FC<Props> = ({ledger}) => {
  const [loading, setLoading] = React.useState<boolean>(false);

  const handleSubmit = async (requestVacation: EmployeeRole_RequestVacation) => {
    try {
      setLoading(true);
      await ledger.pseudoExerciseByKey(EmployeeRole.RequestVacation, {employee: ledger.party()}, requestVacation);
      alert('Request successfully submitted.');
    } catch (error) {
      alert(`Unknown error:\n${error}`);
    } finally {
      setLoading(false);
    }
  }

  return <RequestView onSubmit={handleSubmit} loading={loading} />
}

export default RequestVacationController;
