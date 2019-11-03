import React from 'react';
import { Header, Segment } from 'semantic-ui-react';
import { Summary } from './types';

type Props = Summary;

/**
 * React component for the view of the `MainScreen`.
 */
const SummaryView: React.FC<Props> = ({employee, boss, remainingVacationDays}) => {
  return (
    <Segment>
      <Header as='h2'>
        {employee}
      </Header>
      <p><strong>Boss:</strong> {boss}</p>
      <p><strong>Remaining vacation days:</strong> {remainingVacationDays}</p>
    </Segment>
  );
}

export default SummaryView;
