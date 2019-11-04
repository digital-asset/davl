import React from 'react'
import { Image, Menu, Container, Grid } from 'semantic-ui-react'
import EmployeeView from '../features/employeeView/EmployeeView';
import BossView from '../features/bossView/BossView';
import { useDispatch, useSelector } from 'react-redux';
import { logOut } from '../app/authReducer';
import { RootState, reload } from '../app/rootReducer';

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC = () => {
  const dispatch = useDispatch();
  const credentials = useSelector((state: RootState) => state.auth.credentials);

  return (
    <>
      <Menu icon borderless>
        <Menu.Item>
          <Image
            as='a'
            href='https://www.daml.com/'
            target='_blank'
            src='/daml.svg'
            alt='DAML Logo'
            size='mini'
          />
        </Menu.Item>
        <Menu.Menu position='right'>
          <Menu.Item position='right'>
            You are logged in as {credentials ? credentials.party : '???'}.
          </Menu.Item>
          <Menu.Item
            position='right'
            active={false}
            onClick={() => dispatch(reload())}
            icon='refresh'
          />
          <Menu.Item
            position='right'
            active={false}
            onClick={() => dispatch(logOut())}
            icon='log out'
          />
        </Menu.Menu>
      </Menu>

      <Container>
        <Grid columns={2}>
          <Grid.Column>
            <EmployeeView />
          </Grid.Column>
          <Grid.Column>
            <BossView />
          </Grid.Column>
        </Grid>
      </Container>
    </>
  );
};

export default MainScreen;
