/* eslint-disable @typescript-eslint/camelcase */
import { storiesOf } from '@storybook/react';
import React from 'react';
import RequestView from './RequestView';
import { EmployeeRole_RequestVacation } from '../../daml/DAVL';

const onSubmit = ({fromDate, toDate}: EmployeeRole_RequestVacation) => {
  alert(`first day: ${fromDate}\nlast day: ${toDate}`);
}

const loading = false;


storiesOf("RequestView", module)
  .add("default", () => (
    <RequestView {...{onSubmit, loading}} />
  ))
  ;
