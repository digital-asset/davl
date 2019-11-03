/* eslint-disable @typescript-eslint/camelcase */
import { storiesOf } from '@storybook/react';
import React from 'react';
import RequestView from './RequestView';

const onSubmit = (fromDate: string, toDate: string) => {
  alert(`first day: ${fromDate}\nlast day: ${toDate}`);
}

const loading = false;


storiesOf("RequestView", module)
  .add("default", () => (
    <RequestView {...{onSubmit, loading}} />
  ))
  ;
