import React, { useState } from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import DamlLedger from '../daml-react-hooks';
import Credentials from '../daml-react-hooks/credentials';

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
