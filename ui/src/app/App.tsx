import React from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import { useSelector } from 'react-redux';
import { RootState } from './rootReducer';
import { DamlLedger } from './DamlLedger';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const credentials = useSelector((state: RootState) => state.auth.credentials);

  return credentials ? <DamlLedger credentials={credentials}><MainScreen /></DamlLedger> : <LoginScreen />;
}

export default App;
