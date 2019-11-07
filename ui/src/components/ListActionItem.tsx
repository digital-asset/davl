import React from 'react'
import { Icon, List, SemanticICONS } from 'semantic-ui-react'

type Props = {
  icon: SemanticICONS;
  action: {
    icon: SemanticICONS;
    onClick: () => void;
  };
  outer?: boolean;
}

/**
 * Higher-order React component for a `List.Item` with an additional clickable
 * icon to the right.
 */
const ListActionItem: React.FC<Props> = ({icon, action, outer, children}) => {
  const actionIcon =
    <List.Content floated='right'>
      <Icon link name={action.icon} onClick={action.onClick} />
    </List.Content>;
  return (
    <List.Item>
      {outer ? null : actionIcon}
      <List.Icon name={icon} />
      <List.Content>
        {outer ? actionIcon : null}
        {children}
      </List.Content>
    </List.Item>
  );
  }

export default ListActionItem;
