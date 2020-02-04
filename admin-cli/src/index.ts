import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@daml/ledger';
import * as davl3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import * as davlUpgradev3v4 from '@daml2ts/davl-upgrade-v3-v4/lib/b31fe1021c80fcd4e0adc3437d24a328f3b721e81c0a158f6c4a94b89cb8ab32/Upgrade';
import { Argv } from 'yargs'; // Nice docs : http://yargs.js.org/

type EmployeeInfo = {
  boss: string;
  vacationDays: number;
  acceptProposal: boolean;
}

type LedgerParams = {
  ledgerUrl: string;
  ledgerId: string;
  applicationId: string;
  secret: string;
}

type Config = LedgerParams & {
  company: string;
  employees: { [party: string]: EmployeeInfo };
}

function connectByConfig(config: Config, party: string): Ledger {
  const payload = {
    ledgerId: config.ledgerId,
    applicationId: config.applicationId,
    party,
  };
  const token = encode(payload, config.secret, 'HS256');
  return new Ledger(token, config.ledgerUrl);
}

function connectByParams(
  { ledgerUrl, ledgerId, applicationId, secret }: LedgerParams,
  party: string
): Ledger {
  const payload = { ledgerId, applicationId, party };
  const token = encode(payload, secret, 'HS256');
  return new Ledger(token, ledgerUrl);
}

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3 {
  // Initialize the ledger.
  export const init = async (config: Config) => {
    const companyLedger = connectByConfig(config, config.company);
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
        const employeeLedger = connectByConfig(config, employee);
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
    const companyLedger = connectByConfig(config, config.company);
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
  export const accept = async (ledgerParams: LedgerParams) => {
    const companyLedger = connectByParams(ledgerParams, "Digital Asset");
    const employees = await companyLedger.query(davl3.EmployeeRole, {});
    for (const employeeRole of employees) {
      const employee = employeeRole.key;
      const employeeLedger = connectByParams(ledgerParams, employee);
      const [upgradeProposal] =
        await employeeLedger.query(davlUpgradev3v4.UpgradeProposal, { employee: employee });
      await employeeLedger.exercise(
          davlUpgradev3v4.UpgradeProposal.UpgradeProposal_Accept,
          upgradeProposal.contractId,
          {},
        );
      console.log(`Accepted UpgradeProposal for ${employee}.`);
    }
  }
} //namespace v3v4

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace cli {

  type Cli =
    | { command: 'v3-init'; file: string }
    | { command: 'v3-v4-upgrade'; file: string }
    | { command: 'v3-v4-upgrade-accept'; ledgerParams: LedgerParams }

  // The shape of the object returned from yargs.
  type Arguments = {
    [_: string]: unknown;
    file: string  | null;
    ledgerUrl: string | null;
    ledgerId: string | null;
    applicationId: string | null;
    secret: string| null;
  }

  // eslint-disable-next-line no-inner-declarations
  function argsToCli(args: Arguments): Cli {
    const [cmd] = args["_"] as string[]
    switch (cmd) {
      case 'v3-init': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, file: args.file! };
      }
      case 'v3-v4-upgrade': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, file: args.file! };
      }
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      case 'v3-v4-upgrade-accept': {
        return { command: cmd,
                 ledgerParams: {
                   // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                   ledgerUrl: args.ledgerUrl!,
                   // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                   ledgerId: args.ledgerId!,
                   // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                   applicationId: args.applicationId!,
                   // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                   secret: args.secret!,
                 }
               };
      }
      default: {
        console.error('Unrecognized command "' + cmd + '"');
        process.exit(1);
      }
    }
  }

  // eslint-disable-next-line no-inner-declarations
  export function parse (): Cli {
    const argv: Arguments = require('yargs')
      .usage('Usage $0 <command> [options]')
      .version(false)
      .help('h')
      .alias('h', 'help')
      .command({
        command: 'v3-init',
        desc: 'Initialze a test ledger',
        builder: (yargs: Argv) => {
          yargs.option('f', {
            alias: 'file',
            demandOption: true,
            describe: 'Configuration file',
            type: 'string',
          })
        },
      })
      .example('$0 v3-init -f test-setup.json', 'Ledger initialization from configuration file.')
      .command({
        command: 'v3-v4-upgrade',
        desc: 'Create v3/v4 upgrade proposals',
        builder: (yargs: Argv) => {
          yargs.option('f', {
            alias: 'file',
            demandOption: true,
            describe: 'Configuration file',
            type: 'string',
          })
        }
      })
      .example('$0 v3-v4-upgrade test-setup.json', 'Creation of v3/v4 upgrade proposal contracts')
      .command({
        command: 'v3-v4-upgrade-accept',
        desc: 'Exercise accepts on v3/v4 upgrade proposals.',
        builder: (yargs: Argv) => {
          yargs
            .option('ledger-url', { demandOption: true, describe: 'Ledger URL', type: 'string' })
            .option('ledger-id', { demandOption: true, describe: 'Ledger ID', type: 'string' })
            .option('application-id', { demandOption: true, describe: 'Application ID', type: 'string' })
            .option('secret', { demandOption: true, describe: 'Secret', type: 'string' })
        }
      })
      .example('$0 v3-v4-upgrade-accept --ledger-url ...', 'Exercising accept choices on v3/v4 upgrade proposals')
      .demandCommand(1, 'Missing command')
      .epilogue('$0 is an administration tool - use with caution!')
      .argv;

    return argsToCli(argv);
  }

  // eslint-disable-next-line no-inner-declarations
  async function config(file: string): Promise<Config> {
    const json = await fs.readFile(file, { encoding: 'utf8' });
    return JSON.parse(json) as Config;
  }

  // eslint-disable-next-line no-inner-declarations
  export async function run (cli: Cli): Promise<void> {
    switch (cli.command) {
      case 'v3-init': {
        await v3.init(await config(cli.file));
        break;
      }
      case 'v3-v4-upgrade': {
        await v3v4.upgrade(await config(cli.file));
        break;
      }
      case 'v3-v4-upgrade-accept': {
        await v3v4.accept(cli.ledgerParams);
        break;
      }
    }
  }
}//namespace cli

async function main() {
  await cli.run(cli.parse());
}

// TODO(MH): Use top level await when upgrading to TypeScript 3.8.
// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
