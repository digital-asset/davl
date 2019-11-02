import React from 'react';
import Ledger from "../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Item as VacationListItem } from './VacationList';
import { VacationRequest } from '../daml/DAVL';
import { ContractId } from '../ledger/Types';

type Props = {
  ledger: Ledger;
}

const PendingApprovals: React.FC<Props> = ({ledger}) => {
  const [items, setItems] = React.useState<VacationListItem[]>([]);

  const loadRequests = React.useCallback(async () => {
    try {
      const requests = await ledger.query(VacationRequest, {vacation: {employeeRole: {boss: ledger.party()}}});
      const items: VacationListItem[] = requests.map((request) => {
        return {
          contractId: request.contractId.contractId,
          vacation: request.data.vacation,
        };
      });
      setItems(items);
    } catch (error) {
      alert(`Unknown error:\n${error}`);
    }
  }, [ledger]);

  React.useEffect(() => {
    loadRequests();
    const interval = setInterval(loadRequests, 1000);
    return () => clearInterval(interval);
  }, [loadRequests]);

  const handleApproveRequest = async ({contractId}: VacationListItem) => {
    try {
      await ledger.exercise(VacationRequest.Accept, new ContractId<VacationRequest>(contractId), {});
      alert('Request successfully approved.');
    } catch (error) {
      alert(`Unknown error:\n${error}`);
    }
  }

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
