import React from 'react';
import Ledger from '../../ledger/Ledger';
import VacationListSegment from '../../components/VacationListSegment';
import { useDispatch, useSelector } from 'react-redux';
import * as reducer from './reducer';
import { RootState } from '../../app/rootReducer';
import { Vacation } from '../../utils/vacation';
import { ContractId } from '../../ledger/Types';
import { VacationRequest } from '../../daml/DAVL';

type Props = {
  ledger: Ledger;
}

const BossView: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  React.useEffect(() => { dispatch(reducer.loadRequests(ledger)); }, [dispatch, ledger])

  const vacations = useSelector((state: RootState) => state.bossView.requests);

  const handleApproveRequest = (vacation: Vacation) =>
    dispatch(reducer.approveRequest(ledger, new ContractId<VacationRequest>(vacation.contractId)));

  return (
    <VacationListSegment
      header='Pending Approvals'
      viewer='boss'
      vacations={vacations}
      onClickVacation={handleApproveRequest}
      icon='check'
    />
  );
}

export default BossView;
