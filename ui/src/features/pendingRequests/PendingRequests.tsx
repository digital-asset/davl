import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Item as VacationListItem, makeItem } from '../../components/VacationList';
import { VacationRequest } from '../../daml/DAVL';

type Props = {
  ledger: Ledger;
}

const PendingRequests: React.FC<Props> = ({ledger}) => {
  const [items, setItems] = React.useState<VacationListItem[]>([]);

  const loadRequests = React.useCallback(async () => {
    try {
      const requests = await ledger.query(VacationRequest, {vacation: {employeeRole: {employee: ledger.party()}}});
      const items: VacationListItem[] = requests.map((request) => makeItem(request.contractId, request.data.vacation));
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

  const handleCancelRequest = () => alert('Canceling vacation requests is not yet implemented.');

  return (
    <Segment>
      <Header as='h2'>
        Pending Requests
      </Header>
      <VacationList
        items={items}
        onClickVacation={handleCancelRequest}
        icon='cancel'
      />
    </Segment>
  );
}

export default PendingRequests;
