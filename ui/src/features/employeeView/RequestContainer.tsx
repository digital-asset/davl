/* eslint-disable @typescript-eslint/camelcase */
import React from 'react';
import Ledger from '../../ledger/Ledger';
import RequestView from './RequestView';
import { addRequest } from './reducer';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';

type Props = {
  ledger: Ledger;
}

/**
 * React component to control the `MainView`.
 */
const RequestVacationController: React.FC<Props> = ({ledger}) => {
  const dispatch = useDispatch();
  const loading = useSelector((state: RootState) => state.employeeView.addingRequest);

  const handleSubmit = (fromDate: string, toDate: string) =>
    dispatch(addRequest(ledger, fromDate, toDate));

  return <RequestView onSubmit={handleSubmit} loading={loading} />
}

export default RequestVacationController;
