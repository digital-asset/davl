import React from 'react';
import { Segment, Header, List, Form, SemanticTRANSITIONS } from 'semantic-ui-react';
import { useSelector, useDispatch } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import ListActionItem from '../../components/ListActionItem';
import { DatesRangeInput } from 'semantic-ui-calendar-react';
import { addRequest, setCurrentRequest } from './employeeViewReducer';
import { VacationListItem } from '../../components/VacationListItem';
import { vacationLength, prettyRequests } from '../../utils/vacation';
import { useQuery } from '../../app/damlReducer';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { getLedger } from '../../app/store';

const Requests: React.FC = () => {
  const dispatch = useDispatch();
  const boss = useSelector((state: RootState) => (state.employeeView.summary || {boss: ''}).boss);
  const currentRequest = useSelector((state: RootState) => state.employeeView.currentRequest);
  const addingRequest = useSelector((state: RootState) => state.employeeView.addingRequest);

  const party = useSelector(getLedger).party;
  const {loading: loadingRequests, contracts: requestContracts} =
    useQuery(v3.VacationRequest, () => ({vacation: {employeeRole: {employee: party}}}), [party]);
  const requests = prettyRequests(requestContracts);

  const handleCancelRequest = () => alert('Canceling vacation requests is not yet implemented.');

  const handleAddRequest = (event?: React.FormEvent) => {
    if (event) {
      event.preventDefault();
    }
    if (currentRequest.length !== 23) {
      alert('No date range set for the vacation request.');
      return;
    }
    const fromDate = currentRequest.slice(0, 10);
    const toDate = currentRequest.slice(-10);
    dispatch(addRequest(fromDate, toDate));
  }

  const handleCurrentRequestChange = (event: React.SyntheticEvent, data: {value: string}) =>
    dispatch(setCurrentRequest(data.value));

  let days = '';
  if (currentRequest.length === 23) {
    const fromDate = currentRequest.slice(0, 10);
    const toDate = currentRequest.slice(-10);
    days = `, Days: ${vacationLength({fromDate, toDate})}`
  }

  return (
    <Segment loading={loadingRequests}>
      <Header as='h3'>
        Pending Vacation Requests
      </Header>
      <List relaxed>
        {requests.map((vacation) =>
            <VacationListItem
              key={vacation.contractId}
              vacation={vacation}
              viewer='employee'
              icon='cancel'
              onClickIcon={() => handleCancelRequest}
            />
        )}
        <ListActionItem
          icon='calendar plus outline'
          action={{
            icon: 'add',
            onClick: handleAddRequest,
          }}
        >
          <List.Header>
          <Form onSubmit={handleAddRequest}>
            <DatesRangeInput
              id='date-range-picker'
              placeholder='Add Vacation Request'
              value={currentRequest}
              onChange={handleCurrentRequestChange}
              animation={'none' as SemanticTRANSITIONS}
              dateFormat='YYYY-MM-DD'
              transparent
              closable
              icon={false}
              allowSameEndDate
              loading={addingRequest}
            />
          </Form>
          </List.Header>
          <List.Content>Approver: {boss}{days}</List.Content>
        </ListActionItem>
      </List>
    </Segment>
  );
}

export default Requests;
