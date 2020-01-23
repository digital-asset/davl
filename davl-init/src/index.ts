import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@digitalasset/daml-ledger-fetch'
import * as davl from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';

type Employee = {
  party: string;
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
  employees: Employee[];
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
  const [file] = process.argv.slice(2);
  const json = await fs.readFile(file, {encoding: 'utf8'});
  const config = JSON.parse(json) as Config;

  const companyLedger = connect(config, config.company);
  for (const employee of config.employees) {
    const employeeRole: davl.EmployeeRole = {
      company: config.company,
      employee: employee.party,
      boss: employee.boss,
    };
    const employeeProposal: davl.EmployeeProposal = {
      employeeRole,
      vacationDays: employee.vacationDays.toString(),
    };
    const employeProposalContract =
      await companyLedger.create(davl.EmployeeProposal, employeeProposal);
    console.log(`Created EmployeeProposal for ${employee.party}.`);
    if (employee.acceptProposal) {
      const employeeLedger = connect(config, employee.party);
      await employeeLedger.exercise(
        davl.EmployeeProposal.EmployeeProposal_Accept,
        employeProposalContract.contractId,
        {},
      );
      console.log(`Accepted EmployeeProposal for ${employee.party}.`);
    }
  }
}

// TODO(MH): Use top level await when upgrading to TypeScript 3.8.
// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
