import React from 'react';
import { Vacation, vacationLength } from '../utils/vacation';
import { SemanticICONS, List } from 'semantic-ui-react';
import ListActionItem from './ListActionItem';

export type Props = {
  vacation: Vacation;
  viewer: 'employee' | 'boss';
  onClickIcon: (vacation: Vacation) => void;
  icon: SemanticICONS;
}

export const VacationListItem: React.FC<Props> = ({vacation, viewer, onClickIcon, icon}) => {
  const {employee, boss, fromDate, toDate} = vacation;
  let content: string;
  switch (viewer) {
    case 'employee':
      content = `Approver: ${boss}`;
      break;
    case 'boss':
      content = `Requester: ${employee}`;
      break;
    default:
      content = '';
  }
  return (
    <ListActionItem
      key={vacation.contractId}
      icon='calendar outline'
      action={{
        icon,
        onClick: () => onClickIcon(vacation),
      }}
    >
      <List.Header>{fromDate} - {toDate}</List.Header>
      <List.Content>{content}, Days: {vacationLength(vacation)}</List.Content>
    </ListActionItem>
  );
}
