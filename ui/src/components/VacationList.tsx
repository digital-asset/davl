import React from 'react'
import { List } from 'semantic-ui-react';
import ListActionItem from './ListActionItem';
import { AnyContractId, Party, ContractId } from '../ledger/Types';
import { SemanticICONS } from 'semantic-ui-react/dist/commonjs/generic';
import { Vacation } from '../daml/DAVL';

export type Item = {
  contractId: AnyContractId,
  employee: Party;
  boss: Party;
  fromDate: string;
  toDate: string;
}

export const makeItem = <T extends {}>({contractId}: ContractId<T>, {employeeRole: {employee, boss}, fromDate, toDate}: Vacation) =>
  ({contractId, employee, boss, fromDate, toDate})

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
        const {employee, boss, fromDate, toDate} = item;
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
