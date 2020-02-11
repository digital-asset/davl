import React, { useState } from 'react';
import { Segment, Header, List, Form, SemanticTRANSITIONS } from 'semantic-ui-react';
import ListActionItem from '../../components/ListActionItem';
import { DatesRangeInput } from 'semantic-ui-calendar-react';
import { VacationListItem } from '../../components/VacationListItem';
import { vacationLength, prettyRequests } from '../../utils/vacation';
import { useStreamQuery, useExerciseByKey, useParty } from '../../daml-react-hooks';
import * as v3 from '@daml2ts/davl-v3/lib/davl-v3/DAVL';
import { EmployeeSummary } from '../../utils/employee';
import { toast } from 'react-semantic-toasts';

type Props = {
  employeeSummary: EmployeeSummary | null;
}

const Requests: React.FC<Props> = (props: Props) => {
  const [currentRequest, setCurrentRequest] = useState('');
  const boss = props.employeeSummary ? props.employeeSummary.boss : '';

  const party = useParty();
  const {loading: loadingRequests, contracts: requestContracts} =
    useStreamQuery(v3.VacationRequest, () => ({vacation: {employeeRole: {employee: party}}}), [party]);
  const requests = prettyRequests(requestContracts);

  const [exerciseRequestVacation, loadingRequestVacation] =
    useExerciseByKey(v3.EmployeeRole.EmployeeRole_RequestVacation);

  const handleCancelRequest = () => alert('Canceling vacation requests is not yet implemented.');

  const handleAddRequest = async (event?: React.FormEvent) => {
    if (event) {
      event.preventDefault();
    }
    if (currentRequest.length !== 23) {
      alert('No date range set for the vacation request.');
      return;
    }
    const fromDate = currentRequest.slice(0, 10);
    const toDate = currentRequest.slice(-10);
    await exerciseRequestVacation(party, {fromDate, toDate});
    setCurrentRequest('');
    toast({
      title: 'Success',
      type: 'success',
      description: 'Request successfully submitted.',
      time: 3000,
    });
  }

  const handleCurrentRequestChange = (event: React.SyntheticEvent, data: {value: string}) =>
    setCurrentRequest(data.value);

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
              loading={loadingRequestVacation}
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
