# Release Process

This document describes the general process for creating a new release of a
Marlowe component.

## Step 1: Increment Version Numbers

There are two processes to follow depending on what type of release the current
release (i.e. the most recent tag on `main`) is: a prerelease or a normal release.

### Process if the current release is a normal release

For the `runtime` component, increment the version numbers in
`marlowe-chain-sync.cabal`, `marlowe-runtime.cabal`, `marlowe-runtime-web.cabal`,
`marlowe-client.cabal`, and `marlowe-runtime-cli.cabal` as prescribed by semantic
versioning. Given a version number scheme `major.minor.patch`:

- For API-preserving changes that do not add functionality, increment the `patch` number by one.
- For API-preserving changes that add new functionality, increment the `minor` number by one.
- For API-breaking changes of any kind, increment the `major` number by one.

In all cases, when incrementing one of the version numbers, the lesser version
numbers are all reset to `0`.

How can you determine what type of changes are included in a release? Check the
diff of the protocol golden tests directories since the last release
(`marlowe-chain-sync/.golden` and `marlowe-runtime/.golden`).

- If there are no changes, only API-preserving changes have been made. You will
  have to determine from the change log if new functionality has been added or
  not.
- If any existing files have been changed, API-breaking changes have been made,
  and a major release is required.
- If new files have been added (i.e. new protocols have been added), the change
  is still API preserving (a client from the same major release family can
  connect and communicate successfully without needing to know about the new
  protocols). This indicates new functionality, and a minor release is
  required.
- If files have been deleted, API-breaking changes have been made (this is a
  special case of existing files being changed).

### Process if the current release is a prerelease

If the codebase is currently in a prerelease state, we don't want to repeat
increments of the same kind. E.g. If a previous prerelease has already
incremented the `patch` number, do not increment it again. If the `minor`
number has already been incremented, do not increment it or the `patch` version
again. If the `major` version has already been incremented, do not change
increment anything.

However, if a larger increment is required than has already been applied,
update the version number. For example, if the `minor` number has already been
incremented, and a breaking change is added, increment the `major` number.

## Step 2: Create a Pre-Release Tag

Tag the latest commit on `main` with a pre-release tag. A pre-release tag has
the following format:

```
<component>@v<major>.<minor>.<patch>-rc<n>
```

where `<component>` is the name of the component being released. Currently, the
release process is defined for the `runtime` component, but other components
could be added, such as the validator or the language. These releases will always
be accompanied by a release of the runtime however.

The `major`, `minor`, and `patch` numbers should match what is in the `cabal`
files, and `n` is a monotonically increasing number that gets bumped between
successive prerelease tags.

Push this tag to github with `git push --tags`. Do this directly on `main`.

## Step 3: Ensure Nightly Tests Have Run

Run the nightly tests on the tagged commit if they have not already been run on
that commit. If they fail, file a bug report and abort the current release.
After the failures have been addressed, start again at step 1 (following the
"current prerelease" process).

## Step 4: Manual QA

Schedule or perform manual QA of the changes listed in the change log since the
last release. Also perform basic smoke tests to check for regressions. If
manual QA finds any issues, file a bug report and abort the current release.
After the issues have been addressed, start again at step 1 (following the
"current prerelease" process).

## Step 5: Create a Release Tag

Create and push a release tag *on the same commit as the prerelease tag*.
A release tag is the same as a prerelease tag with the `-prerelease<n>` suffix
dropped.

## Step 6: Publish Haskell Libraries to CHaP

See [here](https://input-output-hk.github.io/cardano-haskell-packages/#how-to-add-a-new-package-version).

## Step 7: Publish a Release on GitHub

Create a new release on GitHub that points to the release tag. Include a
high-level summary description and a detailed change log (from the main
changelog).

Attach the binary files for the CLI tools (`marlowe-cli` and `marlowe-runtime-cli`) to the
release for Mac and Linux.

## Step 8: Update documentation

TODO

## Step 9: Send notifications on public channels

TODO
