import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import * as davl3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import WebSocket from 'ws';

type EmployeeInfo = {
  boss: string;
  vacationDays: number;
  acceptProposal: boolean;
}

type Config = {
  ledgerUrl: string;
  ledgerId: string;
  applicationId: string;
  secret: string;
  company: string;
  employees: { [party: string]: EmployeeInfo };
}

function computeToken(config: Config, party: string): string {
  const payload = {
    ledgerId: config.ledgerId,
    applicationId: config.applicationId,
    party,
  };
  return encode(payload, config.secret, 'HS256');
}

async function main() {
  const [file] = process.argv.slice(2);
  const json = await fs.readFile(file, {encoding: 'utf8'});
  const config = JSON.parse(json) as Config;

  const companyToken = computeToken(config, config.company);
  const address = `ws${config.ledgerUrl.slice(4)}contracts/searchForever`;
  console.log(address);
  const protocols = `jwt.token.${companyToken},daml.ws.auth`;
  console.log(protocols);
  const ws = new WebSocket(address, protocols);
  ws
    .on('error', error => {
      console.error(error);
    })
    .on('open', () => {
      console.log('open');
      ws.send(JSON.stringify({templateIds: [davl3.EmployeeRole.templateId]}));
    })
    .on('close', (code, reason) => {
      console.log(`close (${code}, ${reason})`);
    })
    .on('message', data => {
      console.log(data);
    });
}

// TODO(MH): Use top level await when upgrading to TypeScript 3.8.
// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
