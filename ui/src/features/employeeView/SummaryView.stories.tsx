import { storiesOf } from '@storybook/react';
import React from 'react';
import SummaryView from './SummaryView';
import { Summary } from './types';

const summary: Summary = {
  employee: 'Bob',
  boss: 'Alice',
  remainingVacationDays: '25',
}

storiesOf("SummaryView", module)
  .add("default", () => (
    <SummaryView {...summary} />
  ))
  ;
