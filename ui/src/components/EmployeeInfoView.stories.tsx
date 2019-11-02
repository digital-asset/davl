import { storiesOf } from '@storybook/react';
import React from 'react';
import EmployeeInfoView from './EmployeeInfoView';
import { EmployeeRole, EmployeeVacationAllocation } from '../daml/DAVL';

const employeeRole: EmployeeRole = {
  employee: 'Bob',
  company: 'ACME',
  boss: 'Alice',
}

const employeeVacationAllocation: EmployeeVacationAllocation = {
  employeeRole,
  remainingDays: '25',
}

storiesOf("EmployeeInfo", module)
  .add("default", () => (
    <EmployeeInfoView {...{employeeVacationAllocation}}/>
  ))
  ;
