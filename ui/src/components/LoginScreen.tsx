import React from 'react'
import { Button, Form, Grid, Header, Segment } from 'semantic-ui-react'
import Credentials, { computeToken } from '../ledger/Credentials';
import Ledger from '../ledger/Ledger';
import { EmployeeProposal, EmployeeRole } from '../daml/DAVL';

type Props = {
  onLogin: (ledger: Ledger) => void;
}

/**
 * React component for the login screen of the `App`.
 */
const LoginScreen: React.FC<Props> = ({onLogin}) => {
  const [username, setUsername] = React.useState('Martin');

  const handleLogin = async (event?: React.FormEvent) => {
    try {
      if (event) {
        event.preventDefault();
      }
      const token = computeToken(username);
      const credentials: Credentials = {party: username, token};
      const ledger = new Ledger(credentials);
      const employeeRole = await ledger.pseudoLookupByKey(EmployeeRole, {employee: username});
      if (employeeRole === undefined) {
        alert("You have not yet signed up.");
        return;
      }
      onLogin(ledger);
    } catch(error) {
      alert(`Unknown error:\n${error}`);
    }
  }

  const handleSignup = async (event: React.FormEvent) => {
    try {
      event.preventDefault();
      const token = computeToken(username);
      const credentials: Credentials = {party: username, token};
      const ledger = new Ledger(credentials);
      const employeeProposals = await ledger.query(EmployeeProposal, {employeeRole: {employee: username}});
      switch (employeeProposals.length) {
        case 0:
          alert("There is not invitation for you.");
          return;
        case 1:
          break;
        default:
          alert("There are multiple invitations for you.");
          return;
      }
      const employeeProposalFull = employeeProposals[0];
      const employeeProposal = employeeProposalFull.data;
      const employeeRole = employeeProposal.employeeRole;
      const accept = window.confirm(`You have been invited to work for ${employeeRole.company}.\nBoss: ${employeeRole.boss}\nVacation days: ${employeeProposal.vacationDays}\nDo you accept?`);
      if (!accept) {
        return;
      }
      await ledger.exercise(EmployeeProposal.Accept, employeeProposalFull.contractId, {});
      await handleLogin();
    } catch(error) {
      alert("Unknown error:\n" + error);
    }
  }


  return (
    <Grid textAlign='center' style={{ height: '100vh' }} verticalAlign='middle'>
      <Grid.Column style={{ maxWidth: 450 }}>
        <Header as='h1' textAlign='center' size='huge' style={{color: '#223668'}}>
          <Header.Content>
            Digital Asset Vacation Ledger
          </Header.Content>
        </Header>
        <Form size='large'>
          <Segment>
            <Form.Input
              fluid
              icon='user'
              iconPosition='left'
              placeholder='Username'
              value={username}
              onChange={e => setUsername(e.currentTarget.value)}
            />
            <Button.Group fluid size='large'>
              <Button
                primary
                onClick={handleLogin}
              >
                Log in
              </Button>
              <Button
                secondary
                onClick={handleSignup}
              >
                Sign up
              </Button>
            </Button.Group>
          </Segment>
        </Form>
      </Grid.Column>
    </Grid>
  );
};

export default LoginScreen;
