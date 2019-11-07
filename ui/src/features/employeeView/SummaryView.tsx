import React from 'react';
import { Header, Segment } from 'semantic-ui-react';
import { EmployeeSummary } from '../../utils/employee';

type Props = EmployeeSummary & {loading?: boolean};

/**
 * React component for the view of the `MainScreen`.
 */
const SummaryView: React.FC<Props> = ({employee, boss, remainingVacationDays, loading}) => {
  return (
    <Segment loading={loading}>
      <Header as='h1'>
        {employee}
      </Header>
      <p><strong>Boss:</strong> {boss}</p>
      <p><strong>Remaining vacation days:</strong> {remainingVacationDays}</p>
    </Segment>
  );
}

export default SummaryView;
