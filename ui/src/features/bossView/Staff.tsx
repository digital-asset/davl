import React from 'react';
import { Segment, Header, List } from 'semantic-ui-react';
import { useQuery, useParty } from '../../daml-react-hooks';
import * as v3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { prettyEmployeeSummaries } from '../../utils/employee';

const Staff: React.FC = () => {
  const party = useParty();

  const {loading, contracts} =
    useQuery(v3.EmployeeVacationAllocation, () => ({employeeRole: {boss: party}}), [party]);
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
