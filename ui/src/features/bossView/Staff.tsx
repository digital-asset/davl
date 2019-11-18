import React from 'react';
import { Segment, Header, List } from 'semantic-ui-react';
import { useSelector } from 'react-redux';
import { getLedger } from '../../app/store';
import { useQuery } from '../../app/damlReducer';
import * as v3 from '../../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import { prettyEmployeeSummaries } from '../../utils/employee';

const Staff: React.FC = () => {
  const party = useSelector(getLedger).party;

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
