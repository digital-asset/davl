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
  const party0 = sessionStorage.getItem("party");
  const token0 = sessionStorage.getItem("token");
  const credentials0 =
    party0 !== null && token0 !== null
      ? { party: party0, token: token0 }
      : undefined;
  const [credentials, setCredentials] = useState(credentials0);

  const handleLogin = (credentials: Credentials) => {
    sessionStorage.setItem("party", credentials.party);
    sessionStorage.setItem("token", credentials.token);
    setCredentials(credentials);
  };

  const handleLogout = () => {
    sessionStorage.removeItem("party");
    sessionStorage.removeItem("token");
    setCredentials(undefined);
  };

  return credentials ? (
    <DamlLedger
      token={credentials.token}
      wsBaseUrl={wsBaseUrl}
      party={credentials.party}
    >
      <MainScreen onLogout={handleLogout} />
    </DamlLedger>
  ) : (
    <LoginScreen onLogin={handleLogin} />
  );
};

export default App;
