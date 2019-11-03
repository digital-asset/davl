/* eslint-disable @typescript-eslint/camelcase */
import React from 'react';
import { Header, Segment, Form, SemanticTRANSITIONS } from 'semantic-ui-react';
import { DatesRangeInput } from 'semantic-ui-calendar-react';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';
import * as reducer from './reducer';
import Ledger from '../../ledger/Ledger';

export type Props = {
  ledger: Ledger;
}

/**
 * React component for the view of the `MainScreen`.
 */
const RequestView: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const currentRequest = useSelector((state: RootState) => state.employeeView.currentRequest);
  const addingRequest = useSelector((state: RootState) => state.employeeView.addingRequest);

  const handleSubmit = (event: React.FormEvent) => {
    event.preventDefault();
    if (currentRequest.length !== 23) {
      alert('No date range set for the vacation request.');
      return;
    }
    const from = currentRequest.substr(0, 10);
    const to = currentRequest.substr(13, 10);
    dispatch(reducer.addRequest(ledger, from, to));
  }

  const handleRangeChange = (event: React.SyntheticEvent, data: {value: string}) => {
    dispatch(reducer.setCurrentRequest(data.value));
    console.log(data);
  }

  return (
    <Segment>
      <Header as='h2'>
        Request Vacation
      </Header>
      <Form onSubmit={handleSubmit}>
          <DatesRangeInput
            placeholder='Request New Vacation'
            value={currentRequest}
            onChange={handleRangeChange}
            animation={'none' as SemanticTRANSITIONS}
            size='small'
            dateFormat='YYYY-MM-DD'
            transparent
            closable
            icon={false}
            allowSameEndDate
          />
          <Form.Button
            type='submit'
            content={addingRequest ? 'Loading...' : 'Request'}
          />
      </Form>
    </Segment>
  );
}

export default RequestView;
