# Release process

This directory contains scripts that is used in release.

This project uses [standard-version](https://github.com/conventional-changelog/standard-version) to release an update.
It create a commit with a tag version, update version in `alire.toml` and `dotenv.gpr` and update the changelog according to commit messages.