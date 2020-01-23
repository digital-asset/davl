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

When working with a fresh checkout of the repo, you need to install the
dependencies of the TypeScript frontend by running
```
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
yarn run davl-init test-setup.json
```

## Production setup

The production setup is very similar to the dev setup; the main difference is
that the frontend files are minified and served by a static web server (nginx),
and each bit is wrapped in a Docker container for easy distribution. See the
`azure-pipelines.yaml` file for how the Docker images are built and the
`infra/infra.tf` file for how they are deployed.
