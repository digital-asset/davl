import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@daml/ledger';
import * as davl3 from '@daml2ts/davl-v3/lib/edb5e54da44bc80782890de3fc58edb5cc227a6b7e8c467536f8674b0bf4deb7/DAVL';
import * as davl4 from '@daml2ts/davl-v4/lib/77a41b679a3280df8685e5ef4db2a1f94d6d12db6117a669511e47e938feb207/DAVL';
import * as davlUpgradev3v4 from '@daml2ts/davl-upgrade-v3-v4/lib/b31fe1021c80fcd4e0adc3437d24a328f3b721e81c0a158f6c4a94b89cb8ab32/Upgrade';
import { Argv } from 'yargs'; // Nice docs : http://yargs.js.org/

type Vacation = {
  fromDate: string;
  toDate: string;
  approved: boolean;
}

type EmployeeInfo = {
  boss: string;
  vacationDays: number;
  acceptProposal: boolean;
  vacationRequests?: Vacation[];
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

function connect(
  { ledgerUrl, ledgerId, applicationId, secret }: LedgerParams,
  party: string
): Ledger {
  const payload = { ledgerId, applicationId, party };
  const token = encode(payload, secret, 'HS256');
  return new Ledger(token, ledgerUrl);
}

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3 {

  export const init = async (config: Config) => {
    const companyLedger = connect(config, config.company);
    // Create/accept employee proposals.
    for (const employee in config.employees) {
      const employeeInfo = config.employees[employee];
      const employeeRole: davl3.EmployeeRole = {
        company: config.company,
        employee,
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
    // Create/approve vacation requests.
    for (const employee in config.employees) {
      const employeeInfo = config.employees[employee];
      for (const vacationRequest of employeeInfo.vacationRequests ?? []) {
        const { fromDate, toDate } = vacationRequest;
        const employeeLedger = connect(config, employee);
        const employeeRoleContract = await employeeLedger.lookupByKey(davl3.EmployeeRole, employee);
        if (employeeRoleContract) {
          const [vacationRequestContractId] =
            await employeeLedger.exercise(
              davl3.EmployeeRole.EmployeeRole_RequestVacation,
              employeeRoleContract.contractId,
              { fromDate, toDate },
            );
          console.log(`Created VacationRequest [${fromDate} , ${toDate}] for ${employee}.`);
          if (vacationRequest.approved) {
            const boss = employeeInfo.boss
            const bossLedger = connect(config, boss);
            await bossLedger.exercise(
              davl3.VacationRequest.VacationRequest_Accept,
              vacationRequestContractId,
              {},
            );
            console.log(`${boss} approved VacationRequest [${fromDate}, ${toDate}] for ${employee}.`);
          }
        }
      }
    }
  }

}//namespace v3

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3v4 {

  export const upgradeInit = async (ledgerParams: LedgerParams, company: string) => {
    const companyLedger = connect(ledgerParams, company);
    const employees = await companyLedger.query(davl3.EmployeeRole, {});
    for (const employeeRole of employees) {
      const employee = employeeRole.key;
      const employeeProposal: davlUpgradev3v4.UpgradeProposal = {
        employee: employee,
        company,
      };
      await companyLedger.create(davlUpgradev3v4.UpgradeProposal, employeeProposal);
      console.log(`Created UpgradeProposal for ${employee}.`);
    }
  }

  export const upgradeAccept = async (ledgerParams: LedgerParams, company: string) => {
    const companyLedger = connect(ledgerParams, company);
    const employeeRoleContracts = await companyLedger.query(davl3.EmployeeRole, {});
    // Create update agreements.
    for (const employeeRoleContract of employeeRoleContracts) {
      const employee = employeeRoleContract.key;
      const employeeLedger = connect(ledgerParams, employee);
      const [upgradeProposalContract] =
        await employeeLedger.query(davlUpgradev3v4.UpgradeProposal, { employee });
      await employeeLedger.exercise(
        davlUpgradev3v4.UpgradeProposal.UpgradeProposal_Accept,
        upgradeProposalContract.contractId,
        {},
      );
      console.log(`Accepted UpgradeProposal for ${employee}.`);
    }
  }

  export const upgradeFinish = async (ledgerParams: LedgerParams, company: string) => {
    const companyLedger = connect(ledgerParams, company);
    const employeeUpgradeAgreementContracts =
      await companyLedger.query(davlUpgradev3v4.UpgradeAgreement, { company });
    for (const employeeUpgradeAgreementContract of employeeUpgradeAgreementContracts) {
      const employee = employeeUpgradeAgreementContract.payload.employee;
      const employeeRoleContract = await companyLedger.lookupByKey(davl4.EmployeeRole, employee);
      if (employeeRoleContract) {
        const employeeRole = employeeRoleContract.payload;
        const vacationRequestContracts = await companyLedger.query(davl3.VacationRequest, { vacation: { employeeRole } });
        if (vacationRequestContracts.length > 0) {
          for (const vacationRequestContract of vacationRequestContracts) {
            const { fromDate, toDate } = vacationRequestContract.payload.vacation;
            await companyLedger.exercise(
              davlUpgradev3v4.UpgradeAgreement.UpgradeAgreement_UpgradeVacationRequest,
              employeeUpgradeAgreementContract.contractId,
              { requestId: vacationRequestContract.contractId });
            console.log(`Upgraded VacationRequest [${fromDate}, ${toDate}] for ${employee}.`);
          }
        }
        const boss = employeeRole.boss;
        const [bossUpgradeAgreementContract] =
          await companyLedger.query(davlUpgradev3v4.UpgradeAgreement, { employee: boss, company });
        if (bossUpgradeAgreementContract) {
          const vacationContracts = await companyLedger.query(davl3.Vacation, { employeeRole });
          if (vacationContracts.length > 0) {
            for (const vacationContract of vacationContracts) {
              const { fromDate, toDate } = vacationContract.payload;
              await companyLedger.exercise(
                davlUpgradev3v4.UpgradeAgreement.UpgradeAgreement_UpgradeVacation,
                bossUpgradeAgreementContract.contractId,
                { employeeAgreementId: employeeUpgradeAgreementContract.contractId, vacationId: vacationContract.contractId });
              console.log(`Upgraded Vaction [${fromDate}, ${toDate}] for ${employee}.`);
            }
          }
        }
      }
    }
  }

} //namespace v3v4

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace cli {

  type Cli =
    | { command: 'v3-init'; file: string }
    | { command: 'v3-v4-upgrade-init'; ledgerParams: LedgerParams; company: string }
    | { command: 'v3-v4-upgrade-accept'; ledgerParams: LedgerParams; company: string }
    | { command: 'v3-v4-upgrade-finish'; ledgerParams: LedgerParams; company: string }

  // The shape of the object returned from yargs.
  type Arguments = {
    [_: string]: unknown;
    file: string  | null;
    ledgerUrl: string | null;
    ledgerId: string | null;
    applicationId: string | null;
    secret: string | null;
    company: string | null;
  }

  const argsToCli = (args: Arguments): Cli => {

    const mkLedgerParams = (args: Arguments): LedgerParams => (
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      { ledgerUrl: args.ledgerUrl!, ledgerId: args.ledgerId!, applicationId: args.applicationId!, secret: args.secret! }
    );

    const [cmd] = args["_"] as string[];
    switch (cmd) {
      case 'v3-init': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, file: args.file! };
      }
      case 'v3-v4-upgrade-init': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, ledgerParams: mkLedgerParams(args), company: args.company! };
      }
      case 'v3-v4-upgrade-accept': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, ledgerParams: mkLedgerParams(args), company: args.company! };
      }
      case 'v3-v4-upgrade-finish': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, ledgerParams: mkLedgerParams(args), company: args.company! };
      }
      default: {
        console.error('Unrecognized command "' + cmd + '"');
        process.exit(1);
      }
    }
  }

  export const parse = (): Cli => {

    const ledgerParamsBuilder = (yargs: Argv) => yargs
      .option('ledger-url', { demandOption: true, describe: 'Ledger URL', type: 'string' })
      .option('ledger-id', { demandOption: true, describe: 'Ledger ID', type: 'string' })
      .option('application-id', { demandOption: true, describe: 'Application ID', type: 'string' })
      .option('secret', { demandOption: true, describe: 'Secret', type: 'string' })
    ;

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
            alias: 'file', demandOption: true, describe: 'Configuration file', type: 'string'
          }) },
      })
      .example('$0 v3-init -f test-setup.json',
               'Ledger initialization from configuration file.')
      .command({
        command: 'v3-v4-upgrade-init',
        desc: 'Create v3/v4 upgrade proposals',
        builder: (yargs: Argv) =>
          ledgerParamsBuilder(yargs)
          .option('company', { demandOption: true, describe: 'Company', type: 'string' })
      })
      .example('$0 v3-v4-upgrade-init --ledger-url ...',
               'Creation of v3/v4 upgrade proposal contracts.')
      .command({
        command: 'v3-v4-upgrade-accept',
        desc: 'Exercise accepts on v3/v4 upgrade proposals',
        builder: (yargs: Argv) =>
          ledgerParamsBuilder(yargs)
          .option('company', { demandOption: true, describe: 'Company', type: 'string' })
      })
      .example('$0 v3-v4-upgrade-accept --ledger-url ...',
               'Exercising accept choices on v3/v4 upgrade proposals.')
      .command({
        command: 'v3-v4-upgrade-finish',
        desc: 'Use approved upgrade agreements to complete v3/v4 contract upgrades',
        builder: (yargs: Argv) =>
          ledgerParamsBuilder(yargs)
          .option('company', { demandOption: true, describe: 'Company', type: 'string' })
      })
      .example('$0 v3-v4-upgrade-finish --ledger-url ...',
               'Upgrade v3 vacation and request contracts to v4.')
      .demandCommand(1, 'Missing command')
      .epilogue('$0 is an administration tool - use with caution!')
      .argv;

    return argsToCli(argv);
  }

  const config = async (file: string): Promise<Config> => {
    const json = await fs.readFile(file, { encoding: 'utf8' });
    return JSON.parse(json) as Config;
  }

  export const run = async (cli: Cli): Promise<void> => {
    switch (cli.command) {
      case 'v3-init': {
        const cfg = await config(cli.file);
        await v3.init(cfg);
        break;
      }
      case 'v3-v4-upgrade-init': {
        await v3v4.upgradeInit(cli.ledgerParams, cli.company);
        break;
      }
      case 'v3-v4-upgrade-accept': {
        await v3v4.upgradeAccept(cli.ledgerParams, cli.company);
        break;
      }
      case 'v3-v4-upgrade-finish': {
        await v3v4.upgradeFinish(cli.ledgerParams, cli.company);

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
