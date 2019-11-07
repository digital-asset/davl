import React from 'react';
import { Segment, Header, List } from 'semantic-ui-react';
import { useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';

const Staff: React.FC = () => {
  const staff = useSelector((state: RootState) => state.bossView.staff);
  const loadingStaff = useSelector((state: RootState) => state.bossView.loadingStaff);

  return (
    <Segment loading={loadingStaff}>
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
