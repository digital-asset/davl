import React from 'react'
import { Image, Menu, Container } from 'semantic-ui-react'
import Ledger from '../ledger/Ledger';
import SummaryContainer from '../features/employeeView/SummaryContainer';
import RequestVacationController from '../features/employeeView/RequestContainer';
import EmployeePending from '../features/employeeView/Pending';
import BossPending from '../features/bossView/Pending';
import Approved from '../features/employeeView/Approved';

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
        <SummaryContainer ledger={ledger} />
        <RequestVacationController ledger={ledger} />
        <Approved ledger={ledger} />
        <EmployeePending ledger={ledger} />
        <BossPending ledger={ledger} />
      </Container>
    </>
  );
};

export default MainScreen;
