import React from 'react';
import LoginScreen from './LoginScreen';
import MainScreen from './MainScreen';
import Ledger from '../ledger/Ledger';

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
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
      />
    );
  }
}

export default App;
