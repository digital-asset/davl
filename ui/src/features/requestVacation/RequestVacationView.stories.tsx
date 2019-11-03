/* eslint-disable @typescript-eslint/camelcase */
import { storiesOf } from '@storybook/react';
import React from 'react';
import RequestVacationView from './RequestVacationView';
import { EmployeeRole_RequestVacation } from '../../daml/DAVL';

const onSubmit = ({fromDate, toDate}: EmployeeRole_RequestVacation) => {
  alert(`first day: ${fromDate}\nlast day: ${toDate}`);
}

const loading = false;


storiesOf("RequestVacation", module)
  .add("default", () => (
    <RequestVacationView {...{onSubmit, loading}} />
  ))
  ;
