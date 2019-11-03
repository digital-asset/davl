import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Item as VacationListItem } from '../../components/VacationList';
import { VacationRequest } from '../../daml/DAVL';
import { ContractId } from '../../ledger/Types';
import { useDispatch, useSelector } from 'react-redux';
import * as reducer from './reducer';
import { RootState } from '../../app/rootReducer';

type Props = {
  ledger: Ledger;
}

const PendingApprovals: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const items = useSelector((state: RootState) => state.pendingApprovals);

  React.useEffect(() => { dispatch(reducer.load(ledger)); }, [dispatch, ledger])

  const handleApproveRequest = (item: VacationListItem) =>
    dispatch(reducer.approveRequest(ledger, new ContractId<VacationRequest>(item.contractId)));

  return (
    <Segment>
      <Header as='h2'>
        Pending Approvals
      </Header>
      <VacationList
        items={items}
        onClickVacation={handleApproveRequest}
        icon='check'
      />
    </Segment>
  );
}

export default PendingApprovals;
