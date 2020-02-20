import React from 'react';
import { Segment, Header, List } from 'semantic-ui-react';
import { useStreamQuery, useParty } from '@daml/react';
import * as v4 from '@daml2ts/davl/lib/davl-0.0.4/DAVL';
import { prettyEmployeeSummaries } from '../../utils/employee';

const Staff: React.FC = () => {
  const party = useParty();

  const {loading, contracts} =
    useStreamQuery(v4.EmployeeVacationAllocation, () => ({employeeRole: {boss: party}}), [party]);
  const staff = prettyEmployeeSummaries(contracts);

  return (
    <Segment loading={loading}>
      <Header as='h1'>
        Team
      </Header>
      <List>
        {staff.map(({employee, remainingVacationDays}) =>
          <List.Item key={employee}>
            <List.Icon name='user outline' />
            <List.Content>
              <List.Header>{employee}</List.Header>
              <List.Description>Remaining vacation days: {remainingVacationDays}</List.Description>
            </List.Content>
          </List.Item>
        )}
      </List>
    </Segment>
  );
}

export default Staff;
