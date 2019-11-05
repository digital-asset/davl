import React from 'react';
import { Segment, Header } from 'semantic-ui-react';
import { useSelector } from 'react-redux';
import { RootState } from '../../app/rootReducer';

const Staff: React.FC = () => {
  const staff = useSelector((state: RootState) => state.bossView.staff);

  return (
    <Segment>
      <Header as='h1'>
        Team
      </Header>
      {staff.map(({employee, remainingVacationDays}) =>
        <p key={employee}><strong>{employee}:</strong> {remainingVacationDays}</p>
      )}
    </Segment>
  );
}

export default Staff;
