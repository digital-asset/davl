import React from 'react';
import Ledger from "../../ledger/Ledger";
import { Segment, Header, List, Form, SemanticTRANSITIONS } from 'semantic-ui-react';
import { useSelector, useDispatch } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import ListActionItem from '../../components/ListActionItem';
import { DatesRangeInput } from 'semantic-ui-calendar-react';
import * as reducer from './reducer';

type Props = {
  ledger: Ledger;
}

const Requests: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const boss = useSelector((state: RootState) => (state.employeeView.summary || {boss: ''}).boss);
  const vacations = useSelector((state: RootState) => state.employeeView.pending);
  const currentRequest = useSelector((state: RootState) => state.employeeView.currentRequest);
  const addingRequest = useSelector((state: RootState) => state.employeeView.addingRequest);

  const handleCancelRequest = () => alert('Canceling vacation requests is not yet implemented.');

  const handleAddRequest = (event?: React.FormEvent) => {
    if (event) {
      event.preventDefault();
    }
    if (currentRequest.length !== 23) {
      alert('No date range set for the vacation request.');
      return;
    }
    const fromDate = currentRequest.substr(0, 10);
    const toDate = currentRequest.substr(13, 10);
    dispatch(reducer.addRequest(ledger, fromDate, toDate));
  }

  const handleCurrentRequestChange = (event: React.SyntheticEvent, data: {value: string}) =>
    dispatch(reducer.setCurrentRequest(data.value));

  return (
    <Segment>
      <Header as='h3'>
        Pending Vacation Requests
      </Header>
      <List relaxed>
        {vacations.map((vacation) => {
          const {boss, fromDate, toDate} = vacation;
          return (
            <ListActionItem
              key={vacation.contractId}
              icon='calendar'
              action={{
                icon: 'cancel',
                onClick: () => handleCancelRequest(),
              }}
            >
              <List.Header>{}{fromDate} - {toDate}</List.Header>
              <List.Content>Approver: {boss}</List.Content>
            </ListActionItem>
          );
          }
        )}
        <ListActionItem
          icon='calendar'
          action={{
            icon: 'add',
            onClick: handleAddRequest,
          }}
        >
          <List.Header>
          <Form onSubmit={handleAddRequest}>
            <DatesRangeInput
              placeholder='Request New Vacation'
              value={currentRequest}
              onChange={handleCurrentRequestChange}
              animation={'none' as SemanticTRANSITIONS}
              size='small'
              dateFormat='YYYY-MM-DD'
              transparent
              closable
              icon={false}
              allowSameEndDate
              loading={addingRequest}
            />
          </Form>
          </List.Header>
          <List.Content>Approver: {boss}</List.Content>
        </ListActionItem>
      </List>
    </Segment>
  );
}

export default Requests;
