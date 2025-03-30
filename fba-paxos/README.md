# XDR definitions

XDR definitions are in `fba-paxos/xdr/`, which is a git submodule.
After cloning this repository, setup the submodule with `git submodule init && git submodule update`.

To compile the XDR definitions and use them, the Makefile uses the `xdrpp` package, which must be known to `pkg-config`, and the `xdrc` command.

Here's a way to set up all that on Ubuntu:
* Clone https://github.com/xdrpp/xdrpp
* In the `xdrpp` directory, `./autogen.sh && ./configure --prefix=$HOME/.local/`
* `make && make install`
* Then, make sure that `$HOME/.local/lib/pkgconfig` is in `PKG_CONFIG_PATH` and that `$HOME/.local/bin` is in your `PATH`.
* Finally, `make clean && make` should work.

# TODO: Quorum test

Have a look at `isQuorum` in `LocalNode.ccp` in `stellar-core`.
