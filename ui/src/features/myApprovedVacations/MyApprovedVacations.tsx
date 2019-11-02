import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import Ledger from '../../ledger/Ledger';
import { Segment, Header } from 'semantic-ui-react';
import VacationList from '../../components/VacationList';
import { RootState } from '../../app/rootReducer';
import { load as loadMyApprovedVacations } from './reducer';

type Props = {
  ledger: Ledger;
}

const MyApprovedVacations: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const items = useSelector((state: RootState) => state.approvedVacations);

  React.useEffect(() => { dispatch(loadMyApprovedVacations(ledger)); }, [dispatch, ledger]);

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
