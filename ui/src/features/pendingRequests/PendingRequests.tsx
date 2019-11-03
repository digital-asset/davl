import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header } from 'semantic-ui-react';
import VacationList from '../../components/VacationList';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import * as reducer from './reducer';

type Props = {
  ledger: Ledger;
}

const PendingRequests: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const vacations = useSelector((state: RootState) => state.pendingRquests);

  React.useEffect(() => { dispatch(reducer.load(ledger)); }, [dispatch, ledger]);

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

export default PendingRequests;
