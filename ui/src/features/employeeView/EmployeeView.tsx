import React from 'react';
import Ledger from '../../ledger/Ledger';
import SummaryContainer from './SummaryContainer';
import RequestVacationController from './RequestContainer';
import Approved from './Approved';
import Pending from './Pending';

type Props = {
  ledger: Ledger;
}

const EmployeeView: React.FC<Props> = ({ledger}) => {
  return (
    <>
      <SummaryContainer ledger={ledger} />
      <RequestVacationController ledger={ledger} />
      <Approved ledger={ledger} />
      <Pending ledger={ledger} />
    </>
  );
}

export default EmployeeView;
