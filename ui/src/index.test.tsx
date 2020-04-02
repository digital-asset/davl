// To run this test, from the root of the repository, execute '(cd
// ui&&yarn test)'.
import { exit } from "process";
import { ChildProcess, spawn, spawnSync } from "child_process";
import puppeteer, { Page, Browser } from "puppeteer";
import waitOn from "wait-on";
import { encode } from "jwt-simple";

////////////////////////////////////////////////////////////////////////////////
// Globals

const LEDGER_PORT = "6865";
const LEDGER_ID = "DAVL";
const APPLICATION_ID = "DAVL";
const SECRET = "secret";
const JSON_API_PORT = "7575";
const UI_PORT = "3000";
const TEST_SETUP = "./test-setup.json";
const INIT_CMD = "v5-init";

let damlPlatform: ChildProcess | undefined = undefined;
let ui: ChildProcess | undefined = undefined;
let browser: Browser | undefined = undefined;

////////////////////////////////////////////////////////////////////////////////
// Setup/teardown

/* eslint-disable @typescript-eslint/require-await */
beforeAll(async () => {
  // Spin up daml platform.
  damlPlatform = spawn("./daml-platform.sh", [], {
    cwd: "..",
    stdio: "inherit",
  });
  await waitOn({
    resources: [
      `tcp:localhost:${LEDGER_PORT}`,
      `tcp:localhost:${JSON_API_PORT}`,
    ],
  });

  // Populate ledger.
  const init = spawnSync(
    "yarn",
    ["run", "davl-admin-cli", INIT_CMD, "-f", TEST_SETUP],
    { cwd: "..", stdio: "inherit" },
  );
  if (init.status !== undefined && init.status !== 0) {
    if (damlPlatform !== undefined) {
      damlPlatform.kill();
    }
    exit(1);
  }

  // Spin up a UI ("detached" is neccessary here).
  ui = spawn("yarn", ["start"], {
    detached: true,
    env: { ...process.env, BROWSER: "none" },
    stdio: "inherit",
  });
  await waitOn({ resources: [`tcp:localhost:${UI_PORT}`] });

  // Launch a browser.
  browser = await puppeteer.launch();
}, 40_000);

/* eslint-disable @typescript-eslint/require-await */
afterAll(async () => {
  if (browser !== undefined) {
    await browser.close();
  }

  if (damlPlatform !== undefined) {
    damlPlatform.kill();
  }

  if (ui !== undefined) {
    process.kill(-ui.pid);
  }
});

////////////////////////////////////////////////////////////////////////////////
// Utils

// Calculate an encoded JWT for the given user.
const passwordOfUser = (user: string): string =>
  encode(
    { ledgerId: LEDGER_ID, applicationId: APPLICATION_ID, party: user },
    SECRET,
    "HS256",
  );

// Point a fresh browser page at DAVL.
const newPage = async (): Promise<Page> => {
  if (!browser) {
    throw Error("Browser uninitialized");
  }
  const page = await browser.newPage();
  await page.goto(`http://localhost:${UI_PORT}`);
  return page;
};

// Log in to DAVL and wait for the main screen.
const login = async (page: Page, user: string): Promise<void> => {
  const userInput = await page.waitForSelector(".test-select-username-field");
  const passwordInput = await page.waitForSelector(
    ".test-select-password-field",
  );
  await userInput.click();
  await userInput.type(user);
  await passwordInput.click();
  await passwordInput.type(passwordOfUser(user));
  await page.click(".test-select-login-button");
  await page.waitForSelector(".test-select-main-menu");
};

// Log out of DAVL and wait for the login screen.
const logout = async (page: Page): Promise<void> => {
  await page.click(".test-select-log-out");
  await page.waitForSelector(".test-select-login-screen");
};

////////////////////////////////////////////////////////////////////////////////
// Tests

test("all resources acquired", () => {
  expect(damlPlatform).toBeDefined();
  expect(ui).toBeDefined();
  expect(browser).toBeDefined();
});

test("exisiting user can log in and log out", async () => {
  const page = await newPage();
  await login(page, "Martin");
  await logout(page);
  await page.close();
}, 50_000);
