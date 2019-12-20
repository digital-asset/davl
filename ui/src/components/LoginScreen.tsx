import React from 'react'
import { Button, Form, Grid, Header, Segment } from 'semantic-ui-react'
import Credentials, { preCheckCredentials } from '../daml-react-hooks/credentials';
import Ledger from '@digitalasset/daml-ledger-fetch';
import * as v3 from '../daml/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';

type Props = {
  onLogin: (credentials: Credentials) => void;
}

enum Status { Normal, LoggingIn, SigningUp }

/**
 * React component for the login screen of the `App`.
 */
const LoginScreen: React.FC<Props> = (props) => {
  const [status, setStatus] = React.useState(Status.Normal);
  const [username, setUsername] = React.useState('');
  const [password, setPassword] = React.useState('');

  const withCredentials = async (cont: (credentials: Credentials) => Promise<void>) => {
    const credentials = {
      party: username,
      token: password,
    }
    const error = preCheckCredentials(credentials);
    if (error !== null) {
      alert(error);
      return;
    }
    await cont(credentials);
  }

  const handleLogin = async (event?: React.FormEvent) => {
    if (event) {
      event.preventDefault();
    }
    await withCredentials(async (credentials) => {
      try {
        setStatus(Status.LoggingIn);
        const ledger = new Ledger(credentials.token);
        const employeeRole = await ledger.pseudoLookupByKey(v3.EmployeeRole, {employee: credentials.party});
        if (employeeRole) {
          setStatus(Status.Normal);
          props.onLogin(credentials);
        } else {
          alert("You have not yet signed up.");
        }
      } finally {
        setStatus(Status.Normal);
      }
    });
  }

  const handleSignup = async (event: React.FormEvent) => {
    event.preventDefault();
    await withCredentials(async (credentials) => {
      try {
        setStatus(Status.SigningUp)
        const ledger = new Ledger(credentials.token);
        const employeeProposals =
          await ledger.query(v3.EmployeeProposal, {employeeRole: {employee: credentials.party}});
        if (employeeProposals.length === 0) {
          alert('There is no invitation for you.');
        } else if(employeeProposals.length > 1) {
          alert('There are multiple invitations for you.');
        } else {
          const {contractId, argument: employeeProposal} = employeeProposals[0];
          const employeeRole = employeeProposal.employeeRole;
          const accept = window.confirm(`You have been invited to work for ${employeeRole.company}.\nBoss: ${employeeRole.boss}\nVacation days: ${employeeProposal.vacationDays}\nDo you accept?`);
          if (accept) {
            await ledger.exercise(v3.EmployeeProposal.EmployeeProposal_Accept, contractId, {});
            await handleLogin();
          }
        }
      } finally {
        setStatus(Status.Normal);
      }
    });
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
            <Form.Input
              fluid
              icon='lock'
              iconPosition='left'
              placeholder='Password'
              type='password'
              value={password}
              onChange={e => setPassword(e.currentTarget.value)}
            />
            <Button.Group fluid size='large'>
              <Button
                primary
                loading={status === Status.LoggingIn}
                onClick={handleLogin}
              >
                Log in
              </Button>
              <Button
                secondary
                loading={status === Status.SigningUp}
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
