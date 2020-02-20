import React from 'react'
import { Image, Menu, Container, Grid } from 'semantic-ui-react'
import EmployeeView from '../features/employeeView/EmployeeView';
import BossView from '../features/bossView/BossView';
import { useParty, useReload } from '@daml/react';

type Props = {
  onLogout: () => void;
}

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC<Props> = (props) => {
  const party = useParty();
  const reload = useReload();

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
            onClick={props.onLogout}
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
