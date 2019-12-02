# davl

Holiday tracking application, driven by DAML.

- Developer log [here](/notes/diary.md).
- Index of notes [here](/notes/index.md).
- Model V1 spec [here](/notes/v1-model.md).
- Notes on command-line-interface `davl-cli` [here](/cli/README.md).

Regression testing

- `(cd project/V1; daml test)`
- `cli/test.sh`

## Dev setup

> **WARNING** These notes are incomplete as the full dev process is still under
> development.

To run the application, you need:
- A PostgreSQL database server,
- A DAML Ledger API server,
- A DAML JSON API server, and
- The frontend code.

> Note: If you are only working on the DAML models, you can skip all this and
> just run scenarios in the IDE as usual.

The frontend bit is still somewhat hazy at the moment, but it will surely
include some sort of auto-refreshing web server. That web server should be
configured to redirect `/contracts`, `/command` and `/parties` to the JSON API
server.

To start up the rest, you can run the following commands (each "box" in its own
terminal):

1. PostgreSQL

The eaiest way to run PostgreSQL locally is to use Docker:

```
docker run -e POSTGRES_USER=davl \
           -e POSTGRES_PASSWORD=s3cr3t \
           -e POSTGRES_DB=davl-db \
           -e PGDATA=/var/lib/postgresql/data/pgdata \
           -v $(pwd)/_db_:/var/lib/postgresql/data \
           -p 5432:5432 \
           postgres:11.5-alpine
```

This will create a folder `_db_` in the current cwd for the db's files. They may end up belonging to root. (FIXME)

2. Ledger API Server

Starting the Ledger API Server is relatively easy:

```
cd v3
daml build
daml sandbox --ledgerid DAVL --sql-backend-jdbcurl 'jdbc:postgresql://${DOCKER_IP:-127.0.0.1}/davl-db?user=davl&password=s3cr3t' .daml/dist/*.dar
```

3. DAML JSON API Server

```
cd v3
daml json-api --ledger-host localhost --ledger-port 6865 --http-port 7575 --application-id DAVL-JSON
```

4. Frontend dev server

If you haven't build the `daml-json-types` library yet, for instance when
you're on a fresh clone of the repo, you need to build it first:

```
cd daml-json-types
yarn build
```

During active development of `daml-json-types`, you might want to run
`yarn build:watch` instead of `yarn build`.

With the JSON API server up-and-running and `daml-json-types` built,
you should be able to start the frontend server:

```
cd ui
yarn start
```

## Production setup

The production setup is very similar to the dev setup; the main difference is
that the frontend files are minified and served by a static web server (nginx),
and each bit is wrapped in a Docker container for easy distribution. See the
`azure-pipelines.yaml` file for how the Docker images are built and the
`infra/infra.tf` file for how they are deployed.
