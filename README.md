# Tryhard
Quick and easy tool to get and aggregate DotA 2 hero stats and matchups.

## Usage

This could all be achieved by the awesome folks in [OpenDota](https://www.opendota.com/)

## Development
This program is built with [`stack`](docs.haskellstack.org).
I have this repo setup with a [`.devcontainer` for VSCode](https://code.visualstudio.com/docs/remote/containers) which I find useful to work on a windows machine.

In order to get you up and running, be sure to have `GHC` (check the `.devcontainer/Dockerfile` for the version) and `stack`.
Most used commands are written and documented in the `Makefile`, so you can run and peruse `make help`.

### Basic usage
Run the tests and watch file changes. 
```bash
$ stack test --file-watch
```

If you want to do manual testing, `$ stack run` is nice helper to build quickly and execute.