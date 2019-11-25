import React from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import { useSelector } from 'react-redux';
import { RootState } from './rootReducer';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const daml = useSelector((state: RootState) => state.daml);

  return daml ? <MainScreen /> : <LoginScreen />;
}

export default App;
