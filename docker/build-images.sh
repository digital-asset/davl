#!/usr/bin/env bash

set -euo pipefail

BUILD_NUMBER=${BUILD_NUMBER:-unset}
SLACK_URL=${SLACK_URL:-unset}
GCS_CREDS=${GOOGLE_APPLICATION_CREDENTIALS_CONTENT:-unset}

SDK_VERSION=$(cat SDK_VERSION)

if test daml 1>/dev/null 2>&1; then
    daml install $SDK_VERSION
else
    curl https://get.daml.com | sh -s $SDK_VERSION
fi

if [ "$BUILD_NUMBER" != "unset" ] && [ "$SLACK_URL" != "unset" ] && ["$GCS_CREDS" != "unset" ]; then
    CI="yes"
else
    CI="no"
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR/..

get_tag () {
    TZ=UTC git log -n1 --date=format-local:%Y%m%d%H%M --format=format:%cd-%h --abbrev=6 -- $@
}

tag_exists () {
    if [ "$CI" = "yes" ]; then
        gcloud container images list-tags gcr.io/da-dev-pinacolada/$1 | grep -q $2
    else
        [ -n "$(docker images -q -f reference=$1)" ]
    fi
}

tell_slack () {
    if [ "$CI" = "yes" ]; then
        local message=$(git log --pretty=format:%s -n1)
        curl -XPOST \
             -i \
             -H 'Content-type: application/json' \
             --data "{\"text\":\"<https://dev.azure.com/digitalasset/davl/_build/results?buildId=$BUILD_NUMBER|$message>: uploaded $1\n\"}" \
             $SLACK_URL
    fi
}

if [ "$CI" = "yes" ]; then
    GCS_KEY=$(mktemp)
    cleanup () {
        rm -f $GCS_KEY
    }
    trap cleanup EXIT
    echo "$GCS_CREDS" > $GCS_KEY
    gcloud auth activate-service-account --key-file=$GCS_KEY
    cleanup
    trap - exit
    gcloud auth configure-docker --quiet
fi

if [ "$CI" = "yes" ]; then
    prefix="gcr.io/da-dev-pinacolada/"
else
    prefix=""
fi

SANDBOX_TAG=$(get_tag released SDK_VERSION docker/sandbox.docker)
if tag_exists sandbox $SANDBOX_TAG; then
    echo "sandbox $SANDBOX_TAG already exists."
else
    echo "Building sandbox image version $SANDBOX_TAG..."
    SANDBOX_IMAGE=${prefix}sandbox:$SANDBOX_TAG
    DOCKER_DIR=$(mktemp -d)
    cp -r released $DOCKER_DIR/released
    cp -r $HOME/.daml/sdk/$SDK_VERSION/daml-sdk $DOCKER_DIR/daml-sdk
    docker build -t $SANDBOX_IMAGE -f docker/sandbox.docker --build-arg sdk_version=$SDK_VERSION $DOCKER_DIR
    if [ "$CI" = "yes" ]; then
        docker push $SANDBOX_IMAGE
    fi
    echo "Done building $SANDBOX_IMAGE."
    tell_slack sandbox:$SANDBOX_TAG
fi

JSON_API_TAG=$(get_tag SDK_VERSION docker/json-api.docker)
if tag_exists json-api $JSON_API_TAG; then
    echo "json-api $JSON_API_TAG already exists."
else
    echo "Building json-api image version $JSON_API_TAG..."
    JSON_API_IMAGE=${prefix}json-api:$JSON_API_TAG
    DOCKER_DIR=$(mktemp -d)
    cp -r $HOME/.daml/sdk/$SDK_VERSION/daml-sdk $DOCKER_DIR/daml-sdk
    docker build -t $JSON_API_IMAGE -f docker/json-api.docker $DOCKER_DIR
    if [ "$CI" = "yes" ]; then
        docker push $JSON_API_IMAGE
    fi
    echo "Done building $JSON_API_IMAGE."
    tell_slack json-api:$JSON_API_TAG
fi

UI_TAG=$(get_tag ui docker/nginx.docker docker/nginx.conf.sh)
if tag_exists ui $UI_TAG; then
    echo "ui $UI_TAG already exists."
else
    echo "Building ui image version $UI_TAG..."
    UI_IMAGE=${prefix}ui:$UI_TAG
    DOCKER_DIR=$(mktemp -d)
    (cd ui && yarn build)
    cp -r ui/build $DOCKER_DIR/ui
    cp docker/nginx.conf.sh $DOCKER_DIR/nginx.conf.sh
    docker build -t $UI_IMAGE -f docker/nginx.docker $DOCKER_DIR
    if [ "$CI" = "yes" ]; then
        docker push $UI_IMAGE
    fi
    echo "Done building $UI_IMAGE."
    tell_slack ui:$UI_TAG
fi

TRIGGER_TAG=$(get_tag upgrade-v4-v5-automation SDK_VERSION docker/trigger.docker)
if tag_exists trigger $TRIGGER_TAG; then
    echo "trigger $TRIGGER_TAG already exists."
else
    echo "Building trigger image version $TRIGGER_TAG..."
    TRIGGER_IMAGE=${prefix}trigger:$TRIGGER_TAG
    DOCKER_DIR=$(mktemp -d)
    echo '{"company": "Digital Asset"}' > $DOCKER_DIR/init.json
    cd upgrade-v4-v5-automation
    daml build -o $DOCKER_DIR/automation.dar
    cd -
    cp -r $HOME/.daml/sdk/$SDK_VERSION/daml-sdk $DOCKER_DIR/daml-sdk
    docker build -t $TRIGGER_IMAGE -f docker/trigger.docker $DOCKER_DIR
    if [ "$CI" = "yes" ]; then
        docker push $TRIGGER_IMAGE
    fi
    echo "Done building $TRIGGER_IMAGE."
    tell_slack trigger:$TRIGGER_TAG
fi
