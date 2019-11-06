import React from 'react'
import { Button, Form, Grid, Header, Segment } from 'semantic-ui-react'
import Credentials, { computeToken } from '../ledger/credentials';
import { useDispatch, useSelector } from 'react-redux';
import { logIn, signUp } from '../app/authReducer';
import { RootState } from '../app/rootReducer';

/**
 * React component for the login screen of the `App`.
 */
const LoginScreen: React.FC = () => {
  const dispatch = useDispatch();
  const loggingIn = useSelector((state: RootState) => state.auth.loggingIn);
  const signingUp = useSelector((state: RootState) => state.auth.signingUp);
  const [username, setUsername] = React.useState('Martin');

  const handleLogin = (event: React.FormEvent) => {
    event.preventDefault();
    const token = computeToken(username);
    const credentials: Credentials = {party: username, token};
    dispatch(logIn(credentials));
  }

  const handleSignup = (event: React.FormEvent) => {
    event.preventDefault();
    const token = computeToken(username);
    const credentials: Credentials = {party: username, token};
    dispatch(signUp(credentials));
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
