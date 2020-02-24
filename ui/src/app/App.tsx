import React, { useState } from "react";
import LoginScreen, { Credentials } from "../components/LoginScreen";
import MainScreen from "../components/MainScreen";
import DamlLedger from "@daml/react";

// NOTE(MH): Unfortunately, the development server of `create-react-app` does
// not proxy websockets propert. Thus, we need to bypass it and talk to the
// JSON API directly in development mode.
const wsBaseUrl =
  process.env.NODE_ENV === "development" ? "ws://localhost:7575/" : undefined;

/**
 * React component for the entry point into the application.
 */
const App: React.FC = () => {
  const [credentials, setCredentials] = useState<Credentials | undefined>();

  return credentials ? (
    <DamlLedger
      token={credentials.token}
      wsBaseUrl={wsBaseUrl}
      party={credentials.party}
    >
      <MainScreen onLogout={() => setCredentials(undefined)} />
    </DamlLedger>
  ) : (
    <LoginScreen onLogin={setCredentials} />
  );
};

export default App;
