import React from 'react';
import Ledger from "../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Item as VacationListItem } from './VacationList';
import { Vacation } from '../daml/DAVL';

type Props = {
  ledger: Ledger;
}

const ApprovedVacations: React.FC<Props> = ({ledger}) => {
  const [items, setItems] = React.useState<VacationListItem[]>([]);

  const loadVacations = React.useCallback(async () => {
    try {
      const vacations = await ledger.query(Vacation, {employeeRole: {employee: ledger.party()}});
      const items: VacationListItem[] = vacations.map((vacation) => {
        return {
          contractId: vacation.contractId.contractId,
          vacation: vacation.data,
        };
      });
      setItems(items);
    } catch (error) {
      alert(`Unknown error:\n${error}`);
    }
  }, [ledger]);

  React.useEffect(() => {
    loadVacations();
    const interval = setInterval(loadVacations, 1000);
    return () => clearInterval(interval);
  }, [loadVacations]);

  const handleVacationInfo = () => {};

  return (
    <Segment>
      <Header as='h2'>
        Approved Vacations
      </Header>
      <VacationList
        items={items}
        onClickVacation={handleVacationInfo}
        icon='info'
      />
    </Segment>
  );
}

export default ApprovedVacations;
