import React from "react";
import { Button, Form, Grid, Header, Segment } from "semantic-ui-react";
import Ledger from "@daml/ledger";
import { Party } from "@daml/types";
import v4 from "@daml.js/davl-0.0.4";
import v5 from "@daml.js/davl-0.0.5";
import upgrade from "@daml.js/davl-upgrade-v4-v5-0.0.5";
import { decode } from "jwt-simple";

const LEDGER_ID = "DAVL";

export type Credentials = {
  party: Party;
  token: string;
};

type Props = {
  onLogin: (credentials: Credentials) => void;
};

enum Status {
  Normal,
  LoggingIn,
  SigningUp,
}

/**
 * Check that the party in the token matches the party of the credentials and
 * that the ledger ID in the token matches the given ledger id.
 */
export const preCheckCredentials = ({
  party,
  token,
}: Credentials): string | null => {
  const decoded = decode(token, "", true);
  if (!decoded.ledgerId || decoded.ledgerId !== LEDGER_ID) {
    return "The password is not valid for the given ledger id.";
  }
  if (!decoded.party || decoded.party !== party) {
    return "The password is not valid for this user.";
  }
  return null;
};

/**
 * React component for the login screen of the `App`.
 */
const LoginScreen: React.FC<Props> = props => {
  const [status, setStatus] = React.useState(Status.Normal);
  const [username, setUsername] = React.useState("");
  const [password, setPassword] = React.useState("");

  const withCredentials = async (
    cont: (credentials: Credentials) => Promise<void>,
  ) => {
    const credentials = {
      party: username,
      token: password,
    };
    const error = preCheckCredentials(credentials);
    if (error !== null) {
      alert(error);
      return;
    }
    await cont(credentials);
  };

  const canLogin = async (credentials: Credentials): Promise<boolean> => {
    const ledger = new Ledger({ token: credentials.token });
    const employeeRoleV5 = await ledger.fetchByKey(
      v5.DAVL.V5.EmployeeRole,
      credentials.party,
    );
    if (employeeRoleV5) {
      return true;
    }

    const employeeRoleV4 = await ledger.fetchByKey(
      v4.DAVL.EmployeeRole,
      credentials.party,
    );
    if (!employeeRoleV4) {
      alert("You have not yet signed up.");
      return false;
    }

    const upkradeProposalKey = {
      _1: credentials.party,
      _2: employeeRoleV4.payload.company,
    };
    const upgradeProposal = await ledger.fetchByKey(
      upgrade.Upgrade.UpgradeProposal,
      upkradeProposalKey,
    );
    if (!upgradeProposal) {
      alert("You have not been invited to upgrade to DAVL v5.");
      return false;
    }

    const accept = window.confirm(
      [
        "Do you agree to upgrade to DAVL v5? The only change compared to",
        "DAVL v4 is that you can cancel your pending vacation requests now.",
        "If you do not accept, you cannot use DAVL anymore.",
      ].join(" "),
    );
    if (!accept) {
      return false;
    }

    await ledger.exercise(
      upgrade.Upgrade.UpgradeProposal.UpgradeProposal_Accept,
      upgradeProposal.contractId,
      {},
    );
    return true;
  };

  const handleLogin = async (event?: React.FormEvent) => {
    if (event) {
      event.preventDefault();
    }
    await withCredentials(async credentials => {
      try {
        setStatus(Status.LoggingIn);
        const login = await canLogin(credentials);
        if (!login) {
          return;
        }
      } finally {
        setStatus(Status.Normal);
      }
      props.onLogin(credentials);
    });
  };

  const handleSignup = async (event: React.FormEvent) => {
    event.preventDefault();
    await withCredentials(async credentials => {
      // NOTE(MH): See `handleLogin` for an explanation of what this is about.
      let login = false;
      try {
        setStatus(Status.SigningUp);
        const ledger = new Ledger({ token: credentials.token });
        const employeeProposals = await ledger.query(
          v5.DAVL.V5.EmployeeProposal,
          {
            employeeRole: { employee: credentials.party },
          },
        );
        if (employeeProposals.length === 0) {
          alert("There is no invitation for you.");
        } else if (employeeProposals.length > 1) {
          alert("There are multiple invitations for you.");
        } else {
          const {
            contractId,
            payload: employeeProposal,
          } = employeeProposals[0];
          const employeeRole = employeeProposal.employeeRole;
          const accept = window.confirm(
            `You have been invited to work for ${employeeRole.company}.\nBoss: ${employeeRole.boss}\nVacation days: ${employeeProposal.vacationDays}\nDo you accept?`,
          );
          if (accept) {
            await ledger.exercise(
              v5.DAVL.V5.EmployeeProposal.EmployeeProposal_Accept,
              contractId,
              {},
            );
            login = true;
          }
        }
      } finally {
        setStatus(Status.Normal);
      }
      if (login) {
        await handleLogin();
      }
    });
  };

  return (
    <Grid textAlign="center" style={{ height: "100vh" }} verticalAlign="middle">
      <Grid.Column style={{ maxWidth: 450 }}>
        <Header
          as="h1"
          textAlign="center"
          size="huge"
          style={{ color: "#223668" }}
        >
          <Header.Content>Digital Asset Vacation Ledger v5</Header.Content>
        </Header>
        <Form size="large">
          <Segment>
            <Form.Input
              fluid
              icon="user"
              iconPosition="left"
              placeholder="Username"
              autoComplete="username"
              value={username}
              onChange={e => setUsername(e.currentTarget.value)}
            />
            <Form.Input
              fluid
              icon="lock"
              iconPosition="left"
              placeholder="Password"
              type="password"
              autoComplete="current-password"
              value={password}
              onChange={e => setPassword(e.currentTarget.value)}
            />
            <Button.Group fluid size="large">
              <Button
                primary
                loading={status === Status.LoggingIn}
                onClick={handleLogin}
              >
                Log in
              </Button>
              <Button
                secondary
                loading={status === Status.SigningUp}
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
