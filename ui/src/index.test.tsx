// To run this test, from the root of the repository, execute '(cd
// ui&&yarn test)'.
import { exit } from 'process';
import * as tmp from 'tmp';
import { ChildProcess, spawn, spawnSync } from 'child_process';
import puppeteer, { Page, Browser } from "puppeteer";
import waitOn from 'wait-on';
import { encode } from 'jwt-simple';

////////////////////////////////////////////////////////////////////////////////
// Globals

const LEDGER_HOST = 'localhost';
const LEDGER_PORT = '6865';
const LEDGER_ID = "DAVL";
const APPLICATION_ID = "DAVL";
const SECRET = "secret";
//const LEDGER_URL = 'http://localhost:7575/';
const JSON_API_PORT = '7575';
const UI_PORT = '3000';
const TEST_SETUP = '../test-setup.json'
const INIT_CMD = 'v5-init'
const DARS = [
    '../released/davl-v4.dar',
    '../released/davl-v5.dar',
    '../released/davl-upgrade-v4-v5.dar'
]
let ui: ChildProcess | undefined = undefined;
let browser: Browser | undefined = undefined;
let api: ChildProcess | undefined = undefined;
let ledger: ChildProcess | undefined = undefined;

////////////////////////////////////////////////////////////////////////////////
// Setup/teardown

/* eslint-disable @typescript-eslint/require-await */
beforeAll(async () => {
  // Create log files.
  const [ledgerLog, apiLog, uiLog] =
    [ tmp.fileSync({ prefix: 'sandbox-', postfix: '.log' }),
      tmp.fileSync({ prefix: 'json-api-', postfix: '.log' }),
      tmp.fileSync({ prefix: 'davl-ui-', postfix: '.log' }),
    ];
  console.log(`ledger log : "${ledgerLog.name}"`);
  console.log(`json-api log : "${apiLog.name}"`);
  console.log(`ui log : "${uiLog.name}"`);

  // Spin up a ledger.
  ledger =
    spawn('daml',
          [ 'sandbox', '--port', LEDGER_PORT, '--ledgerid', LEDGER_ID, '--wall-clock-time' ],
          { detached: true, stdio: ['ignore', ledgerLog.fd, ledgerLog.fd ] }
         );
  ledger.unref();
  await waitOn({resources: [ `tcp:localhost:${LEDGER_PORT}` ]});

  // Upload dars (sync).
  for (const dar of DARS) {
    const {status} =
      spawnSync('daml',
        [ 'ledger', 'upload-dar', '--host', LEDGER_HOST, '--port', LEDGER_PORT, dar ],
        { stdio: [ 'ignore', 'inherit', 'inherit' ] });
    if(status !== undefined && status !== 0) {
      exit(1);
    }
  }

  // Spint up a json-api.
  api = spawn('daml',
              ['json-api', '--ledger-host', LEDGER_HOST, '--ledger-port', LEDGER_PORT, '--http-port', JSON_API_PORT],
              { detached: true, stdio: ['ignore', apiLog.fd, apiLog.fd ] });
  api.unref();
  await waitOn({resources: [ `tcp:localhost:${JSON_API_PORT}` ]});

  // Populate initial ledger state (sync).
  const init =
     spawnSync ('yarn',
            ['run', 'davl-admin-cli', INIT_CMD, '-f', TEST_SETUP],
            { stdio: [ 'ignore', 'inherit', 'inherit' ] }
           );
  if(init.status !== undefined && init.status !== 0) {
    exit(1);
  }

  // Spin up a UI.
  ui = spawn('yarn',
             ['start'],
             { detached: true, env: {...process.env, BROWSER: 'none'}, stdio: ['inherit', uiLog.fd, uiLog.fd ]});
  ui.unref();
  await waitOn({resources: [ `tcp:localhost:${UI_PORT}` ]});

  // Launch a browser.
  browser = await puppeteer.launch();

}, 40_000);

/* eslint-disable @typescript-eslint/require-await */
afterAll(async () => {
  if (ledger) {
    ledger.kill();
  }

  if (api) {
    api.kill();
  }

  if (ui) {
    process.kill(-ui.pid);
  }

  if (browser) {
    await browser.close();
  }
});

////////////////////////////////////////////////////////////////////////////////
// Utils

// Calculate an encoded JWT for the given user.
const passwordOfUser = (user: string): string =>
  encode({ ledgerId: LEDGER_ID, applicationId: APPLICATION_ID, party: user }, SECRET, 'HS256')
  ;

// Point a fresh browser page at DAVL.
const newPage = async (): Promise<Page> => {
  if (!browser) {
    throw Error('Browser uninitialized');
  }
  const page = await browser.newPage();
  await page.goto(`http://localhost:${UI_PORT}`);
  return page;
}

// Log in to DAVL and wait for the main screen.
const login = async (page: Page, user: string): Promise<void> => {
  const userInput = await page.waitForSelector('.test-select-username-field');
  const passwordInput = await page.waitForSelector('.test-select-password-field');
  await userInput.click();
  await userInput.type(user);
  await passwordInput.click();
  await passwordInput.type(passwordOfUser(user));
  await page.click('.test-select-login-button');
  await page.waitForSelector('.test-select-main-menu');
}

// Log out of DAVL and wait for the login screen.
const logout = async (page: Page): Promise<void> => {
  await page.click('.test-select-log-out');
  await page.waitForSelector('.test-select-login-screen');
}

////////////////////////////////////////////////////////////////////////////////
// Tests

test('all resources acquired', () => {
  expect(ledger).not.toBeUndefined();
  expect(api).not.toBeUndefined();
  expect(ui).not.toBeUndefined();
  expect(browser).not.toBeUndefined();
})

test('exisiting user can log in and log out', async () => {
  const page = await newPage();
  await login(page, 'Martin');
  await logout(page);
  await page.close();
}, 50_000);
