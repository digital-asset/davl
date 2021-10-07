## Upgrading the SDK

In order to move DAVL to the next SDK version, you should:

1. Check that [the site](https://davl.da-ext.net) is still up and running. This
   requires a VPN connection.
1. Change the SDK version number in the following files: `SDK_VERSION`,
   `admin-cli/package.json`, `ui/package.json`.
1. Regenerate the `yarn.lock` file. This is usually also a good time to just
   bump our dependencies a bit, so you can do this by running:
   ```plaintext
   rm yarn.lock && yarn install
   ```
   `yarn` is provided by `dev-env`.
1. Make a PR with the above modifications, and get it merged. Once those
   changes are on `master`, new images will be built, but nothing will be
   deployed yet. The CI system should send a few messages on
   `#team-application-runtime` with the tags of the new images.
1. Once you have those tags, you can make a new PR that updates the
   `infra/deployed-versions.tfvars` file to use the new images. Once that one
   is merged, the CI run will deploy the new images.
1. CI will send a message to `#team-application-runtime` once the new machines
   are started. It may take a few more minutes after that before the website is
   again up and running.
