import React from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import { DamlLedger } from './DamlLedger';
import { useState } from 'react';
import Credentials from '../ledger/credentials';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const [credentials, setCredentials] = useState<Credentials | undefined>();

  return credentials
    ? <DamlLedger credentials={credentials}>
        <MainScreen onLogout={() => setCredentials(undefined)}/>
      </DamlLedger>
    : <LoginScreen onLogin={setCredentials}/>;
}

export default App;
