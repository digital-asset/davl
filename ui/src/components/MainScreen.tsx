import React from 'react'
import { Image, Menu, Container } from 'semantic-ui-react'
import Ledger from '../ledger/Ledger';
import EmployeeInfoController from './EmployeeInfoController';
import RequestVacationController from './RequestVacationController';
import PendingRequests from './PendingRequests';
import PendingApprovals from './PendingApprovals';
import ApprovedVacations from './ApprovedVacations';

type Props = {
  ledger: Ledger;
  onLogout: () => void;
}

/**
 * React component for the main screen of the `App`.
 */
const MainScreen: React.FC<Props> = ({ledger, onLogout}) => {
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
            onClick={onLogout}
            icon='log out'
          />
        </Menu.Menu>
      </Menu>

      <Container>
        <EmployeeInfoController ledger={ledger} />
        <RequestVacationController ledger={ledger} />
        <ApprovedVacations ledger={ledger} />
        <PendingRequests ledger={ledger} />
        <PendingApprovals ledger={ledger} />
      </Container>
    </>
  );
};

export default MainScreen;
