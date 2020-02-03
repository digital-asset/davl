import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@daml/ledger'
import * as davl3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
// Check we can import the v3-v4 Upgrade module.
import { } from '@daml2ts/davl-upgrade-v3-v4/lib/b31fe1021c80fcd4e0adc3437d24a328f3b721e81c0a158f6c4a94b89cb8ab32/Upgrade';

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

function connect(config: Config, party: string): Ledger {
  const payload = {
    ledgerId: config.ledgerId,
    applicationId: config.applicationId,
    party,
  };
  const token = encode(payload, config.secret, 'HS256');
  return new Ledger(token, config.ledgerUrl);
}

async function main() {
  const argv = require('yargs').argv
  if (argv.ships > 3 && argv.distance < 53.5) {
    console.log('Plunder more riffiwobbles!')
  } else {
    console.log('Retreat from the xupptumblers!')
  }

  const [file] = process.argv.slice(2);
  const json = await fs.readFile(file, {encoding: 'utf8'});
  const config = JSON.parse(json) as Config;

  const companyLedger = connect(config, config.company);
  for (const employee in config.employees) {
    const employeeInfo = config.employees[employee];
    const employeeRole: davl3.EmployeeRole = {
      company: config.company,
      employee: employee,
      boss: employeeInfo.boss,
    };
    const employeeProposal: davl3.EmployeeProposal = {
      employeeRole,
      vacationDays: employeeInfo.vacationDays.toString(),
    };
    const employeProposalContract =
      await companyLedger.create(davl3.EmployeeProposal, employeeProposal);
    console.log(`Created EmployeeProposal for ${employee}.`);
    if (employeeInfo.acceptProposal) {
      const employeeLedger = connect(config, employee);
      await employeeLedger.exercise(
        davl3.EmployeeProposal.EmployeeProposal_Accept,
        employeProposalContract.contractId,
        {},
      );
      console.log(`Accepted EmployeeProposal for ${employee}.`);
    }
  }
}

// TODO(MH): Use top level await when upgrading to TypeScript 3.8.
// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
