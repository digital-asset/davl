import React from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import Ledger from '../ledger/Ledger';
import { useDispatch, useSelector } from 'react-redux';
import { reload, RootState } from './rootReducer';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const dispatch = useDispatch();
  const credentials = useSelector((state: RootState) => state.auth.credentials);

  if (credentials === undefined) {
    return (
      <LoginScreen />
    );
  } else {
    const ledger = new Ledger(credentials);
    return (
      <MainScreen
        ledger={ledger}
        onReload={() => dispatch(reload(ledger))}
      />
    );
  }
}

export default App;
