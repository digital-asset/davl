import { encode } from 'jwt-simple';

export const LEDGER_ID: string = 'davl';

export const APPLICATION_ID: string = 'davl';

export const SECRECT_KEY: string = 'secret';

export function computeToken(party: string): string {
  const payload = {
    ledgerId: LEDGER_ID,
    applicationId: APPLICATION_ID,
    party,
  };
  return encode(payload, SECRECT_KEY, 'HS256');
}

export type Credentials = {
  party: string;
  token: string;
}

export const computeCredentials = (party: string): Credentials => {
  const token = computeToken(party);
  return {party, token};
}

export default Credentials;
