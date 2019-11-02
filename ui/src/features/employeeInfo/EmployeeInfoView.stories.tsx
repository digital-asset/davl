import { storiesOf } from '@storybook/react';
import React from 'react';
import EmployeeInfoView from './EmployeeInfoView';
import { EmployeeInfo } from './reducer';

const employeeInfo: EmployeeInfo = {
  employee: 'Bob',
  boss: 'Alice',
  remainingVacationDays: '25',
}

storiesOf("EmployeeInfo", module)
  .add("default", () => (
    <EmployeeInfoView {...employeeInfo}/>
  ))
  ;
