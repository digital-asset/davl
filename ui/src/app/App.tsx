import React from 'react';
import LoginScreen from '../components/LoginScreen';
import MainScreen from '../components/MainScreen';
import Ledger from '../ledger/Ledger';
import { useDispatch } from 'react-redux';
import { reload } from './rootReducer';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const dispatch = useDispatch();
  const [ledger, setLedger] = React.useState<Ledger | undefined>(undefined);

  if (ledger === undefined) {
    return (
      <LoginScreen
        onLogin={(ledger) => setLedger(ledger)}
      />
    );
  } else {
    return (
      <MainScreen
        ledger={ledger}
        onLogout={() => setLedger(undefined)}
        onReload={() => {dispatch(reload(ledger))}}
      />
    );
  }
}

export default App;
