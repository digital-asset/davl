import React from 'react'
import { Image, Menu, Container, Grid } from 'semantic-ui-react'
import Ledger from '../ledger/Ledger';
import EmployeeView from '../features/employeeView/EmployeeView';
import BossView from '../features/bossView/BossView';

type Props = {
  ledger: Ledger;
  onLogout: () => void;
  onReload: () => void;
}

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC<Props> = ({ledger, onLogout, onReload}) => {
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
            You are logged in as {ledger.party()}.
          </Menu.Item>
          <Menu.Item
            position='right'
            active={false}
            onClick={onReload}
            icon='refresh'
          />
          <Menu.Item
            position='right'
            active={false}
            onClick={onLogout}
            icon='log out'
          />
        </Menu.Menu>
      </Menu>

      <Container>
        <Grid columns={2}>
          <Grid.Column>
            <EmployeeView ledger={ledger} />
          </Grid.Column>
          <Grid.Column>
            <BossView ledger={ledger} />
          </Grid.Column>
        </Grid>
      </Container>
    </>
  );
};

export default MainScreen;
