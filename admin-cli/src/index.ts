import { promises as fs } from 'fs';
import { encode } from 'jwt-simple';
import Ledger from '@daml/ledger';
import { CreateEvent } from '@daml/ledger';
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

  export const dump = async (ledgerParams: LedgerParams, company: string, file: string) => {
    const companyLedger = connect(ledgerParams, company);

    const vacationOfVacationContract = (
      vacationContract: CreateEvent<davl3.Vacation>): Vacation => {
      const { fromDate, toDate} = vacationContract.payload;
      return ({fromDate, toDate, approved: true});
    };

    const vacationOfVacationRequestContract = (
      vacationRequestContract: CreateEvent<davl3.VacationRequest>): Vacation => {
      const { fromDate, toDate } = vacationRequestContract.payload.vacation;
      return ({fromDate, toDate, approved: false});
    }

    const employeeInfoOfEmployeeRoleContract = async (
      employeeRoleContract: CreateEvent<davl3.EmployeeRole>): Promise<[string, EmployeeInfo]> => {
      const employeeRole = employeeRoleContract.payload;
      const { employee, boss } = employeeRole;
      const vacationContracts = await companyLedger.query(davl3.Vacation, { employeeRole });
      const vacationRequestContracts = await companyLedger.query(davl3.VacationRequest, { vacation: { employeeRole } });
      const signedVacations = vacationContracts.map(vacationOfVacationContract);
      const unsignedVacations = vacationRequestContracts.map(vacationOfVacationRequestContract);
      const allVacations = signedVacations.concat(unsignedVacations);
      const employeeVacationAllocationContract = await companyLedger.lookupByKey(davl3.EmployeeVacationAllocation, employee);
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const { remainingDays } = employeeVacationAllocationContract!.payload;
      return [employee,
              { boss: boss,
                vacationDays: Number(remainingDays),
                acceptProposal: true,
                vacationRequests: allVacations.length == 0 ? undefined : allVacations
              }];
    }

    const employeeInfoOfEmployeeProposalContract = (
      employeeProposalContract: CreateEvent<davl3.EmployeeProposal>): [string, EmployeeInfo] => {
      const employeeProposal = employeeProposalContract.payload;
      const { employeeRole, vacationDays } = employeeProposal;
      const { employee, boss } = employeeRole;
      return [employee,
              { boss: boss,
                vacationDays: Number(vacationDays),
                acceptProposal: false,
                vacationRequests: undefined
              }];
    }

    const employeeRoleContracts = await companyLedger.query(davl3.EmployeeRole, {});
    const employeeProposalContracts = await companyLedger.query(davl3.EmployeeProposal, {});
    const signedEmployees = await Promise.all(employeeRoleContracts.map(employeeInfoOfEmployeeRoleContract));
    const unsignedEmployees = employeeProposalContracts.map(employeeInfoOfEmployeeProposalContract);
    const employees: { [party: string]: EmployeeInfo } = {}
    signedEmployees
      .concat(unsignedEmployees)
      .forEach(([name, info]) => employees[name] = info);
    const { ledgerUrl, ledgerId, applicationId, secret } = ledgerParams;
    const config: Config = { ledgerUrl, ledgerId, applicationId, secret, company, employees };
    await fs.writeFile(file, JSON.stringify(config, null, 2), { encoding: 'utf8' })
    console.log(`Dump written to '${file}'.`);
  }

}//namespace v3

// eslint-disable-next-line @typescript-eslint/no-namespace
namespace v3v4 {

  export const upgradeInit = async (ledgerParams: LedgerParams, company: string) => {
    const companyLedger = connect(ledgerParams, company);
    const employeeRoleContracts = await companyLedger.query(davl3.EmployeeRole, {});
    for (const employeeRole of employeeRoleContracts) {
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
    | { command: 'v3-dump'; ledgerParams: LedgerParams; company: string; file: string }
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
      case 'v3-dump': {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return { command: cmd, ledgerParams: mkLedgerParams(args), company: args.company!, file: args.file! };
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
        command: 'v3-dump',
        desc: 'Create a v3 ledger dump',
        builder: (yargs: Argv) => {
          ledgerParamsBuilder(yargs)
          .option('company', { demandOption: true, describe: 'Company', type: 'string' })
          .option('f', {
            alias: 'file', demandOption: true, describe: 'Filename to dump to', type: 'string'
          }) },
      })
      .example('$0 v3-dump --ledger-params ... -f dump.json',
               'Dumping the v3 state of a ledger to a file.')
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
      case 'v3-dump': {
        await v3.dump(cli.ledgerParams, cli.company, cli.file);
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
