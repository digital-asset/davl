import { storiesOf } from '@storybook/react';
import React from 'react';
import VacationList, { Item, Props } from './VacationList';

const employeeRole = {
  employee: 'Bob',
  boss: 'Alice',
}

const items: Item[] = [
  {...employeeRole, contractId: '1', fromDate: '2019-12-24', toDate: '2019-12-26'},
  {...employeeRole, contractId: '2', fromDate: '2019-12-30', toDate: '2020-01-02'},
]

const props: Props = {
  items,
  onClickVacation: (item) => alert(JSON.stringify(item)),
  icon: 'check',
}

storiesOf("VacationList", module)
  .add("default", () => (
    <VacationList {...props} />
  ))
  ;
