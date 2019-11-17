import React from 'react'
import { Button, Form, Grid, Header, Segment } from 'semantic-ui-react'
import Credentials, { preCheckCredentials } from '../ledger/credentials';
import { useDispatch, useSelector } from 'react-redux';
import { logIn, signUp } from '../app/authReducer';
import { RootState } from '../app/rootReducer';
import * as daml from '../app/damlReducer';

/**
 * React component for the login screen of the `App`.
 */
const LoginScreen: React.FC = () => {
  const dispatch = useDispatch();
  const loggingIn = useSelector((state: RootState) => state.auth.loggingIn);
  const signingUp = useSelector((state: RootState) => state.auth.signingUp);
  const [username, setUsername] = React.useState('');
  const [password, setPassword] = React.useState('');

  const withCredentials = (cont: (credentials: Credentials) => void) => {
    const credentials = {
      party: username,
      token: password,
    }
    const error = preCheckCredentials(credentials);
    if (error !== null) {
      alert(error);
      return;
    }
    cont(credentials);
  }

  const handleLogin = (event: React.FormEvent) => {
    event.preventDefault();
    withCredentials((credentials) => {
      dispatch(logIn(credentials));
      dispatch(daml.start(credentials));
    });
  }

  const handleSignup = (event: React.FormEvent) => {
    event.preventDefault();
    withCredentials((credentials) => dispatch(signUp(credentials)));
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
                loading={loggingIn}
                onClick={handleLogin}
              >
                Log in
              </Button>
              <Button
                secondary
                loading={signingUp}
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
