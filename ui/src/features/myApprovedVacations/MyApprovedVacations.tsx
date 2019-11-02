import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import Ledger from '../../ledger/Ledger';
import { Segment, Header } from 'semantic-ui-react';
import VacationList, { Item as VacationListItem, makeItem } from '../../components/VacationList';
import { Vacation } from '../../daml/DAVL';
import { RootState } from '../../app/rootReducer';
import { set as setMyApprovedVacations } from './reducer';

type Props = {
  ledger: Ledger;
}

const MyApprovedVacations: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const items = useSelector((state: RootState) => state.approvedVacations);

  const loadVacations = React.useCallback(async () => {
    try {
      const vacations = await ledger.query(Vacation, {employeeRole: {employee: ledger.party()}});
      const items: VacationListItem[] = vacations.map((vacation) => makeItem(vacation.contractId, vacation.data));
      dispatch(setMyApprovedVacations(items));
    } catch (error) {
      alert(`Unknown error:\n${error}`);
    }
  }, [ledger, dispatch]);

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

export default MyApprovedVacations;
