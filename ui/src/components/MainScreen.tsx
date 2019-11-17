import React from 'react'
import { Image, Menu, Container, Grid } from 'semantic-ui-react'
import EmployeeView from '../features/employeeView/EmployeeView';
import BossView from '../features/bossView/BossView';
import { useDispatch, useSelector } from 'react-redux';
import { logOut } from '../app/authReducer';
import { RootState } from '../app/rootReducer';
import { getLedger } from '../app/store';
import * as daml from '../app/damlReducer';
import * as v3 from '../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import * as employeeView from '../features/employeeView/employeeViewReducer';

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC = () => {
  const dispatch = useDispatch();
  const party = useSelector((state: RootState) => getLedger(state).party);

  const handleLogout = () => {
    dispatch(logOut());
    dispatch(daml.stop());
  }

  const handleReload = () => {
    dispatch(employeeView.loadSummary());
    dispatch(daml.reloadTemplate(v3.Vacation));
    dispatch(daml.reloadTemplate(v3.VacationRequest));
    dispatch(daml.reloadTemplate(v3.EmployeeVacationAllocation));
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
            onClick={handleReload}
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
