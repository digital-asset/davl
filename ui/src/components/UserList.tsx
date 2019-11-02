import React from 'react'
import { List, SemanticICONS } from 'semantic-ui-react'
import ListActionItem from './ListActionItem';
import { Party } from '../ledger/Types';
import { User } from '../daml/User';

type Props = {
  users: User[];
  action: {
    icon: SemanticICONS;
    onClick: (party: Party) => void;
  };
}

/**
 * React component to display a list of `User`s. The `action` can be
 * performed on every party displayed.
 */
const UserList: React.FC<Props> = ({users, action: {icon, onClick}}) => {
  return (
    <List divided relaxed>
      {users.map((user) =>
        <ListActionItem
          key={user.party}
          icon='user'
          action={{
            icon,
            onClick: () => onClick(user.party),
          }}
          outer
        >
          <List.Header>{user.party}</List.Header>
          <List.List>
            {user.friends.map((friend) =>
              <ListActionItem
                key={friend}
                icon='user outline'
                action={{
                  icon,
                  onClick: () => onClick(friend),
                }}
              >
                <List.Header>{friend}</List.Header>
              </ListActionItem>
            )}
          </List.List>
        </ListActionItem>
      )}
    </List>
  );
};

export default UserList;
