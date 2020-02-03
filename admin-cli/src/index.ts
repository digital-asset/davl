import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@daml/ledger';
import * as davl3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import * as davlUpgradev3v4 from '@daml2ts/davl-upgrade-v3-v4/lib/b31fe1021c80fcd4e0adc3437d24a328f3b721e81c0a158f6c4a94b89cb8ab32/Upgrade';

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

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3 {
  // Initialize the ledger.
  export const init = async (config: Config) => {
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
      const employeeProposalContract =
        await companyLedger.create(davl3.EmployeeProposal, employeeProposal);
      console.log(`Created EmployeeProposal for ${employee}.`);
      if (employeeInfo.acceptProposal) {
        const employeeLedger = connect(config, employee);
        await employeeLedger.exercise(
          davl3.EmployeeProposal.EmployeeProposal_Accept,
          employeeProposalContract.contractId,
          {},
        );
        console.log(`Accepted EmployeeProposal for ${employee}.`);
      }
    }
  }
}//namespace v3

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3v4 {
  // Create upgrade proposals.
  export const upgrade = async (config: Config) => {
    const companyLedger = connect(config, config.company);
    const employees = await companyLedger.query(davl3.EmployeeRole, {});
    for (const employeeRole of employees) {
      const employee = employeeRole.key;
      const employeeProposal: davlUpgradev3v4.UpgradeProposal = {
        employee: employee,
        company: config.company,
      };
      await companyLedger.create(davlUpgradev3v4.UpgradeProposal, employeeProposal);
      console.log(`Created UpgradeProposal for ${employee}.`);
    }
  }

  // "Auto-" accept upgrade proposals.
  export const accept = async (config: Config) => {
    // Note : For now, we are using the 'config' parameter for ledger
    // connectivity. This is temporary until we improve command line
    // parsing.
    const companyLedger = connect(config, config.company);
    const employees = await companyLedger.query(davl3.EmployeeRole, {});
    for (const employeeRole of employees) {
      const employee = employeeRole.key;
      const employeeLedger = connect(config, employee);
      const [upgradeProposal] =
        await employeeLedger.query(davlUpgradev3v4.UpgradeProposal, {employee: employee});
      await employeeLedger.exercise(
          davlUpgradev3v4.UpgradeProposal.UpgradeProposal_Accept,
          upgradeProposal.contractId,
          {},
        );
      console.log(`Accepted UpgradeProposal for ${employee}.`);
    }
  }
} //namespace v3v4

async function main() {
  function usage (): void {
    console.log (
      'Usage : davl-admin-cli ' +
        '[v3-init|v3-v4-upgrade|v3-v4-upgrade-accept] FILE'
    );
  }

  if (process.argv.length != 4) {
    usage();
    process.exit(1);
  }

  const [file] = process.argv.slice(3);
  const json = await fs.readFile(file, {encoding: 'utf8'});
  const config = JSON.parse(json) as Config;

  switch (process.argv[2]) {
    case 'v3-init': {
      await v3.init(config);
      break;
    }
    case 'v3-v4-upgrade': {
      await v3v4.upgrade(config);
      break;
    }
    case 'v3-v4-upgrade-accept': {
      await v3v4.accept(config);
      break;
    }
    default: {
      console.log ('Unrecognized command "' + process.argv[2] + '"');
      usage();
      break;
    }
  }
}

// TODO(MH): Use top level await when upgrading to TypeScript 3.8.
// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
