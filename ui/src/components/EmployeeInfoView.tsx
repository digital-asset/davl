import React from 'react';
import { Header, Segment } from 'semantic-ui-react';
import { EmployeeVacationAllocation } from '../daml/DAVL';

export type Props = {
  employeeVacationAllocation: EmployeeVacationAllocation;
}

/**
 * React component for the view of the `MainScreen`.
 */
const EmployeeInfoView: React.FC<Props> = ({employeeVacationAllocation}) => {
  const employeeRole = employeeVacationAllocation.employeeRole;
  return (
    <Segment>
      <Header as='h2'>
        {employeeRole.employee}
      </Header>
      <p><strong>Boss:</strong> {employeeRole.boss}</p>
      <p><strong>Remaining vacation days:</strong> {employeeVacationAllocation.remainingDays}</p>
    </Segment>
  );
}

export default EmployeeInfoView;
