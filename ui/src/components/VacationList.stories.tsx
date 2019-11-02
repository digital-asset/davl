import { storiesOf } from '@storybook/react';
import React from 'react';
import { EmployeeRole } from '../daml/DAVL';
import VacationList, { Item, Props } from './VacationList';

const employeeRole: EmployeeRole = {
  employee: 'Bob',
  company: 'Acme',
  boss: 'Alice',
}

const items: Item[] = [
  {contractId: '1', vacation: {employeeRole, fromDate: '2019-12-24', toDate: '2019-12-26'}},
  {contractId: '2', vacation: {employeeRole, fromDate: '2019-12-30', toDate: '2020-01-02'}},
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
