import React from 'react';
import { useSelector } from 'react-redux';
import Ledger from '../../ledger/Ledger';
import { Segment, Header } from 'semantic-ui-react';
import VacationList from '../../components/VacationList';
import { RootState } from '../../app/rootReducer';

type Props = {
  ledger: Ledger;
}

const Approved: React.FC<Props> = () => {
  const vacations = useSelector((state: RootState) => state.employeeView.approved);

  const handleVacationInfo = () => {};

  return (
    <Segment>
      <Header as='h2'>
        Approved Vacations
      </Header>
      <VacationList
        vacations={vacations}
        onClickVacation={handleVacationInfo}
        icon='info'
      />
    </Segment>
  );
}

export default Approved;
