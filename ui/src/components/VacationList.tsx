import React from 'react'
import { List } from 'semantic-ui-react';
import ListActionItem from './ListActionItem';
import { SemanticICONS } from 'semantic-ui-react/dist/commonjs/generic';
import { Vacation } from '../utils/vacation';

export type Props = {
  vacations: Vacation[];
  onClickVacation: (vacation: Vacation) => void;
  icon: SemanticICONS;
}

/**
 * React component to display a list of `Vacation`s.
 */
const VacationList: React.FC<Props> = ({vacations, onClickVacation, icon}) => {
  return (
    <List relaxed>
      {vacations.map((vacation) => {
        const {employee, boss, fromDate, toDate} = vacation;
        return (
          <ListActionItem
            key={vacation.contractId}
            icon='plane'
            action={{
              icon,
              onClick: () => onClickVacation(vacation),
            }}
          >
            <List.Header>{}{fromDate} - {toDate}</List.Header>
            <List.Content>Employee: {employee}, Boss: {boss}</List.Content>
          </ListActionItem>
        );
        }
      )}
    </List>
  );
};

export default VacationList;
