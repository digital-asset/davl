# Sample code

**This repo contains sample code to help you get started with DAML. Please bear in mind that it is provided for illustrative purposes only, and as such may not be production quality and/or may not fit your use-cases. You may use the contents of this repo in parts or in whole according to the BSD0 license:**

> Copyright © 2020 Digital Asset (Switzerland) GmbH and/or its affiliates
>
> Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.
>
> THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# davl

Holiday tracking application, driven by DAML.

- Developer log [here](/notes/diary.md).
- Index of notes [here](/notes/index.md).
- Model V1 spec [here](/notes/v1-model.md).
- Notes on command-line-interface `davl-cli` [here](/cli/README.md).

## Dev setup

> **WARNING** These notes are incomplete as the full dev process is still under
> development.

To run the application, you need:
- A "DAML Platform", i.e., a Ledger API server and a JSON API server, and
- The frontend code.

> Note: If you are only working on the DAML models, you can skip all this and
> just run scenarios in the IDE as usual.

The frontend bit is still somewhat hazy at the moment, but it will surely
include some sort of auto-refreshing web server. That web server should be
configured to redirect `/contracts`, `/command` and `/parties` to the JSON API
server.

To start up the rest, you can run the following commands (each "box" in its own
terminal):

1. DAML Platform

To start a DAML Platform, run
```
./daml-platform.sh
```
This will start an in-memory sandbox, navigator and the JSON API. All DARs in
the `released` folder will be deployed to the sandbox as well.

To shut down the DAML Platform, press `Ctrl-C`.

2. Frontend dev server

When working with a fresh checkout of the repo or the first time after a,
the version in `SDK_VERSION` has been bumped, you need to (re)generate
the TypeScript bindings for the DARs and install the
dependencies of the TypeScript frontend by running
```
DAML_SDK_VERSION=$(cat SDK_VERSION) daml codegen ts -o daml2ts released/*.dar
yarn install
yarn workspaces run build
```

To start a development server for the frontend, run
```
cd ui
yarn start
```

3. Initialize the ledger (optional)

To create a few employees on the running DAVL ledger, run
```
yarn run davl-admin-cli v5-init --file test-setup.json
```

4. Use the app

In order to log into the initialized ledger, you can use the the
username/password combinations in the table below.

| Username | Password                                                                                                                                                         |
|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Martin   | eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6IkRBVkwiLCJhcHBsaWNhdGlvbklkIjoiREFWTCIsInBhcnR5IjoiTWFydGluIn0.iVNloMAzEYklKzxPNajGdiTTRAZkoLv0JPJg2hDXvac |
| Moritz   | eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6IkRBVkwiLCJhcHBsaWNhdGlvbklkIjoiREFWTCIsInBhcnR5IjoiTW9yaXR6In0.Bg7DvvQT8FOsAPQo7hejwyqAlb1lrxqq4cDm6rhIFLA |
| Robin    | eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6IkRBVkwiLCJhcHBsaWNhdGlvbklkIjoiREFWTCIsInBhcnR5IjoiUm9iaW4ifQ.SvH81YSQIKw0cl4dEyTYBemu-UANkJbYB1huqrIOL28  |

Once you're logged in, the app hopefully self-explanatory.

## Production setup

The production setup is very similar to the dev setup; the main difference is
that the frontend files are minified and served by a static web server (nginx),
and each bit is wrapped in a Docker container for easy distribution. See the
`azure-pipelines.yaml` file for how the Docker images are built and the
`infra/infra.tf` file for how they are deployed.
