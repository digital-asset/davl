import React from 'react'
import { Image, Menu, Container, Grid } from 'semantic-ui-react'
import EmployeeView from '../features/employeeView/EmployeeView';
import BossView from '../features/bossView/BossView';
import { useDispatch } from 'react-redux';
import { logOut } from '../app/authReducer';
import * as daml from '../app/damlReducer';

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC = () => {
  const dispatch = useDispatch();
  const party = daml.useParty();
  const reload = daml.useReload();

  const handleLogout = () => {
    dispatch(logOut());
  }

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
            You are logged in as {party}.
          </Menu.Item>
          <Menu.Item
            position='right'
            active={false}
            onClick={() => reload()}
            icon='refresh'
          />
          <Menu.Item
            position='right'
            active={false}
            onClick={handleLogout}
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
