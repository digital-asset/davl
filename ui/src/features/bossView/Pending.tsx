import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList from '../../components/VacationList';
import { VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { useDispatch, useSelector } from 'react-redux';
import * as reducer from './reducer';
import { RootState } from '../../app/rootReducer';
import { Vacation } from '../../utils/vacation';

type Props = {
  ledger: Ledger;
}

const Pending: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const vacations = useSelector((state: RootState) => state.pendingApprovals);

  React.useEffect(() => { dispatch(reducer.load(ledger)); }, [dispatch, ledger])

  const handleApproveRequest = (vacation: Vacation) =>
    dispatch(reducer.approveRequest(ledger, new ContractId<VacationRequest>(vacation.contractId)));

  return (
    <Segment>
      <Header as='h2'>
        Pending Approvals
      </Header>
      <VacationList
        vacations={vacations}
        onClickVacation={handleApproveRequest}
        icon='check'
      />
    </Segment>
  );
}

export default Pending;
