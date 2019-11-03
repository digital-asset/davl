import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList from '../../components/VacationList';
import { useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';

type Props = {
  ledger: Ledger;
}

const Pending: React.FC<Props> = () => {
  const vacations = useSelector((state: RootState) => state.employeeView.pending);

  const handleCancelRequest = () => alert('Canceling vacation requests is not yet implemented.');

  return (
    <Segment>
      <Header as='h2'>
        Pending Requests
      </Header>
      <VacationList
        vacations={vacations}
        onClickVacation={handleCancelRequest}
        icon='cancel'
      />
    </Segment>
  );
}

export default Pending;
