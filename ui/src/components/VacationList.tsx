import React from 'react'
import { List } from 'semantic-ui-react';
import ListActionItem from './ListActionItem';
import { AnyContractId } from '../ledger/Types';
import { Vacation } from '../daml/DAVL';
import { SemanticICONS } from 'semantic-ui-react/dist/commonjs/generic';

export type Item = {
  contractId: AnyContractId,
  vacation: Vacation,
}

export type Props = {
  items: Item[];
  onClickVacation: (item: Item) => void;
  icon: SemanticICONS;
}

/**
 * React component to display a list of `Vacation`s.
 */
const VacationList: React.FC<Props> = ({items, onClickVacation, icon}) => {
  return (
    <List relaxed>
      {items.map((item) => {
        const {employeeRole: {employee, boss}, fromDate, toDate} = item.vacation;
        return (
          <ListActionItem
            key={item.contractId}
            icon='plane'
            action={{
              icon,
              onClick: () => onClickVacation(item),
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
