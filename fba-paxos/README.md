# An implementation of single-shot Paxos using SCP Quorums

*Note: This readme is quite long and has a lot of sections. If you're reading
this on github, it's recommended to use the table-of-contents button, located
in the top right of the readme:*

![](README-toc.png)



## Project description

`paxos.ivy` contains three descriptions of a Paxos consensus algorithm that
uses with SCP-style quorums.
Each description (called an "isolate") is a full specification of the consensus
algorithm and some of its safety properties, and each can be independently
verified.
Each isolate refines the previous isolate (earlier isolates are more abstract,
and later isolates are more concrete); i.e. each isolate describes fewer
possible behaviors than the previous isolate, and comes with a proof of this
fact.

### Level 1 description (most abstract)

Highest level descirption. Uses a global view of state in all nodes.
After following [Install IVy](#Install-IVy), verify this isolate with:
```bash
ivy_check isolate=level_1 paxos.ivy
```

### Level 2 description

Also uses a global view of state in all nodes, but makes more messages explicit
than in Level 1.
After following [Install IVy](#Install-IVy), verify this isolate with:
```bash
ivy_check isolate=level_2 paxos.ivy
```

### Implementation (most concrete)

Runnable implementation. Uses a local view of state in one node.
After following [Install IVy](#Install-IVy), verify this isolate with:
```bash
ivy_check isolate=impl complete=fo paxos.ivy
```
The argument `complete=fo` tells Ivy not to check that the verification
conditions are decidable, and indeed there are some problematic quantifier
alternations. Removing them is left as an exercise. Because of this `ivy_check`
may get stuck. If so, restart it to try your luck again; it should use a
different random seed, but if not, do `ivy_check seed=$RANDOM complete=fo paxos.ivy`.

**TODO: What does the proof guarantee? What part of it is TCB?**



## Dependencies

### Install a C++ compiler

On macOS this may be initiated by the terminal command:
```bash
xcode-select --install
```

### Install IVy

Follow the instructions on the
[IVy website](https://kenmcmil.github.io/ivy/install.html), but prefer to use a
python virtual environment instead of installing things globally.
Keep in mind that you may need a specific version of some dependencies, such as
Z3. We used the following versions:

* IVy 1.8, python 3.10.15, z3 4.12.6

### Install `xdrpp`

To compile XDR definitions and use them, you will need `xdrpp`, and it must be
known to both `pkg-config` and `xdrc`.

*On Ubuntu:* To install `xdrpp` clone <https://github.com/xdrpp/xdrpp>, and run
the following commands in the `xdrpp` directory:
```bash
./autogen.sh && ./configure --prefix=$HOME/.local/
make && make install
```

*On macOS (as of Sequoia):* Use the instructions for Ubuntu, but first install
these dependencies.
```bash
brew install autoconf automake # dependencies for running ./autogen.sh
brew install pandoc # dependencies for running make
```


## Build this project

SCP's network communication structures are defined in XDR specifications.
The XDR definitions are in `fba-paxos/xdr/`, which is a git submodule.
After cloning this repository, obtain the submodule with:
```bash
git submodule init && git submodule update
```

Update the following colon-delimited environment variables to ensure that
`pkg-config` and `xdrc` can locate `xdrpp` and its libraries:
Ensure `PKG_CONFIG_PATH` includes `$HOME/.local/lib/pkgconfig`.
Ensure `PATH` includes `$HOME/.local/bin`.
This can usually be accomplished with the following commands:
```bash
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$HOME/.local/lib/pkgconfig"
export PATH="$PATH:$HOME/.local/bin"
```
Now build with the following command.
```bash
make clean && make
```
This will build all of the executables described [Build individual executables](#Build-individual-executables).

### Build individual executables

* `make TestCompilation.out`

  This executable comes from `app/TestCompilation.cpp` and exists for two reasons:
  1. It instantiates some C++ templates to sanity check their compilation.
  2. It serves to [Test some of the C++ code](#Test-some-of-the-C-code).

* `make GenTestData.out`

  This executable comes from `app/GenTestData.cpp` and is used to generate test
  data in XDR format to [Test some of the C++ code](#Test-some-of-the-C-code).

  `GenTestData.out` splits a node configuration (a mapping of
  nodes to QSets) from a single json file to a set of XDR files.
  Specifically, it reads stdin for a JSON *array* of
  `{publicKey:PUBKEYSTR, quorumSet:QSET}` objects and outputs each such `QSET`
  as XDR-formatted `stellar::SCPQuorumSet` data to a file at the path
  `data/<PUBKEYSTR>.xdr`.
  Since the `PUBKEYSTR` field is used as a filename, it cannot be
  arbitrary binary data.
  In practice this is not an issue, though, since
  JSON files do not contain arbitrary binary data.

  See `data/conflicted.json` for an example of the input format (note that
  `conflicted.json` is copied from the
  [python-fbas](https://github.com/nano-o/python-fbas/blob/83c7c7c7b5c8e4fecbefa88cbafac6ad78ba5e33/tests/test_data/conflicted.json#L1)
  repo).
  The function which parses this format is `load_jnodeslices` in
  `libscp/StellarJsonXdr.hpp`.

* `make paxos.out`

  This executable comes from `paxos.ivy`, which is first compiled to C++ by an
  explicit makefile target.

  `paxos.out` implements a node that performs a single multi-round
  paxos-consensus process with other nodes over UDP.
  It is described more completely in [Demo the Paxos-with-QSets implementation](#Demo-the-Paxos-with-QSets-implementation).


## Run/demo this project

### Verify the isolates

After installing all [Dependencies](#Dependencies) you can verify all three isolates in
[Project description](#Project-description) with the command `make check`.

### Test some of the C++ code

The command `make test` will build the project and then
(1) generate XDR test data in the `data/` directory and
(2) run some self tests of the quorum-checking algorithm.

### Demo the Paxos-with-QSets implementation

`paxos.out` implements a node that performs a single multi-round
paxos-consensus process with other nodes over UDP.
Without arguments, the executable produces the following usage message:
```
usage: paxos node.max n
```
This usage message indicates that you should provide two command-line
arguments, one for the maximum zero-indexed node identifier, and the other for
the current node's zero-indexed node identifier.
For example, if you have three terminals open then you could run a three node
consensus process by issuing these three commands (one in each terminal):
```bash
./paxos.out 2 0
./paxos.out 2 1
./paxos.out 2 2
```
However, this will fail without first creating an appropriate QSet
configuration file (`qset_config.json`) located in the working directory of
each node-process.
This file contains a single JSON QSet; the function that parses this format is
`load_jqset` in `libscp/StellarJsonXdr.hpp`.
We've provided appropriate QSet configuration files in the `data/node*/`
directories.

#### A four-node demo

In four separate terminals, run these commands (one in each terminal):
```bash
(cd data/node0 && ../../paxos.out 3 0)
(cd data/node1 && ../../paxos.out 3 1)
(cd data/node2 && ../../paxos.out 3 2)
(cd data/node3 && ../../paxos.out 3 3)
```
In any of the repls, now execute the command `interface.propose(121)`.
After a moment, each of the four nodes will print `interface.decide(121)`
indicating that consensus was reached.
Since this is single-shot paxos, you must kill all four processes (with
control+c) before the next part of the demo.

Start the four nodes as above.
After starting them all, kill either node 0 or node 1, and then execute
`interface.propose(234)` in any of nodes 0, 1, or 2 (whichever was not killed).
After a moment, the nodes among 0, 1, and 2 which are running will print
`interface.decide(234)` to indicate that consensus was reached, however, node 3
will be unable to do so.

The QSet configurations in the `data/node*/` directories describe four nodes.
The first three nodes require agreement with any pair of nodes 0, 1, and 2.
The last node requires agreement with both nodes 0 and 1.
Whereas the first three nodes may suffer a failure of any one among them, the
last is more limited.

**TODO: Describe a demo that proves to the user that a node received another node's QSet over the network and used it as part of checking for quorum.**






## Maintain this project

### Project/build structure

**Executables** are defined in `app/*.cpp` and built by the implicit makefile
target, `%.out`.
*Exception:* `paxos.out` comes from `paxos.ivy` and is first compiled to
`app/paxos.cpp` by its own explicit makefile target.

**C++ Libraries** are defined in `libscp/{vendor/,}*.hpp`.
They are included in the build only through explicit `#include <...>` lines.
There are no makefile targets for separate compilation of object files.
*Exception:* The XDR definitions are compiled to headers under `xdr/` and
cannot be rehomed under `libscp/` because of a use of `#include "..."` syntax
(instead of `#include <...>`) in their generated code.

There is also a directory, `hs/`, containing a haskell project that explores some
high level approaches to quorum checking/finding. It is unused and can be
deleted if desired.

### Source code organization

**`libscp/QuorumChecker.hpp`** --- `QuorumChecker` is an abstract class that defines
the interface for finding or checking quorums.
It is parameterized over a type of node-ids and a type of slice-collection.
It has two implementations:

a. **`libscp/NaiveQuorumChecker.hpp`** --- `NaiveQuorumChecker` implements
   `QuorumChecker` for a slice-collection which is a (naive) vector-of-sets of
   node-ids. The node-id type is still a parameter.

b. **`libscp/QSetQuorumChecker.hpp`** --- `QSetQuorumChecker` implements
   `QuorumChecker` for a slice-collection which is a `stellar::SCPQuorumSet`.
   The node-id type is fixed to `stellar::NodeID`.
   There are tests for this implementation in `app/TestCompilation.cpp`.

<details>
<summary><b><code>libscp/StellarJsonXdr.hpp</code></b> --- These functions are
used to convert between JSON data or files and XDR data or files. They make
use of a vendored copy of <a href="https://github.com/nlohmann/json">nlohmann's
json library</a> in <code>libscp/vendor/json.hpp</code>.</summary>

* Working with `stellar::NodeID`:
  * Naive introduction from ASCII: `jstr_to_nid`, `str_to_nid`
  * Naive elimination to ASCII: `nid_to_str`
  * Equality: `nid_eq`
* Working with `stellar::SCPQuorumSet`:
  * Introduction from JSON data in memory: `jqset_to_qset`
  * Introduction from a JSON-formatted file: `load_jqset`
  * Introduction from a JSON-formatted file mapping nodes to QSets: `load_jnodesslices`
  * Equality: `qset_eq`
* Working with any XDR data:
  * Introduction from an XDR-formatted file: `load_xdr`
  * Elimination to an XDR-formatted file: `dump_xdr`

</details>

**`libscp/PaxosAdapter.hpp`** --- Items in the `paxos_adapter` namespace define
important behavior in the IVy and C++ interface.

* **`paxos_adapter::AdaptedQSet`** wraps `stellar::SCPQuorumSet` to provide it with
  implementations of methods that IVy requires: `__hash`, `operator==`, and
  `operator<<`.

* **`paxos_adapter::is_quorum`** is the IVy interface to `QSetQuorumChecker`.
  This defines how a node checks whether a set of nodes has reached
  quorum-threshold.

**`array_set.ivy`** --- Defines a structure tracking a set of nodes and their
QSets. Provides an interface to the C++ code to check if quorum has been
reached; the IVy callsite for `paxos_adapter::is_quorum` is here.

**`stellar_data.ivy`** --- IVy wrappers for XDR data,
`stellar::SCPQuorumSet` and (eventually) `stellar::NodeID`.

* **`scp_qset`** --- Wrapper for `stellar::SCPQuorumSet`.
  Includes a static `load` method to load a node's own QSet config.
  Defines methods that IVy requires: `__ser<…>`, `__deser<…>`, and `_arg<…>`.
  The type parameter for these methods is the class where `scp_qset` is instantiated.

* **`scp_nodeid`** --- Stub intended to wrap `stellar::NodeID`.

#### How is `stellar::SCPQuorumSet` wrapped for use in IVy?

#### How is `stellar::SCPQuorumSet` serialized for IVy's UDP library?

### Issues, small to large

#### (Small) Rename `array_set.ivy`

The `array_map_with_qset_quorum_check` module in `array_set.ivy` is named
poorly, as is the file. Pick a better name for both.

#### (Small) Are we checking `is_quorum` correctly?

There is some question of how `is_quorum` in `libscp/PaxosAdapter.hpp` should work:

* Use `findQuorum` to obtain a quorum set from the candidate set, and then return
  whether *that quorum set* `containsQuorumSlice` for the current node's QSet.

* Use `findQuorum` to obtain a quorum set from the candidate set. Return
  *whether that quorum set is non-empty* (meaning a quorum set was found), *and*
  whether *the candidate set* `containsQuorumSlice` for the current node's QSet.

These two are subtly different, but amount to only a few lines of code
difference. The current version is the second bulletpoint, but the first
bulletpoint can be obtained by reverting
[`4b43404`](https://github.com/plredmond/ivy-proofs/commit/4b43404b1d57e0b6a1152f733cd8128f37a6f3b1)

#### (Small) Give namespaces for the libraries in `libscp/`

The libraries in `libscp/` aren't all wrapped in namespaces, so they pollute a
big happy global namespace.
It should be pretty easy to wrap them one at a time in a sensible name, and
then track down the name resolution bugs.

#### (Small) Rename or remove common type aliases

`QSetQuorumChecker` in `libscp/QSetQuorumChecker.hpp` is the concrete
instantiation of `QuorumChecker` in `libscp/QuorumChecker.hpp`.
`QSetQuorumChecker` also defines some public type aliases (`using`
declarations) for our most common domain types.
These type aliases are used in other files (grep for `QSetQuorumChecker::`).
The problem with these type aliases is that, in an effort to make them short,
they're not meaningful, and quite confusing.
Rename or remove the type aliases for clarity.

* `QSetQuorumChecker::Node = stellar::NodeID`

  This can probably be removed, since
  it's clearer to use the canonical name of the domain type,
  `stellar::NodeID`.

* `QSetQuorumChecker::X = Slice<stellar::NodeID>`

  This should properly be named
  `QSetQuorumChecker::Slice`, but I don't know the rules for duplicate names.
  If the duplication is a problem, then also rename `Slice` in
  `libscp/QuorumChecker.hpp` to something more abstract.

* `QSetQuorumChecker::XS = stellar::SCPQuorumSet`

  This can probably be removed, since
  it's clearer to use the canonical name of the domain type,
  `stellar::SCPQuorumSet`.

* `QSetQuorumChecker::XS = std::map<Node, XS, decltype(nodeid_cmp)*>`

  This should be renamed to either `QSetQuorumChecker::NodesSlices` or
  `QSetQuorumChecker::QSetConfigs`. The difficulty here is making clear in the
  name that it contains configurations for multiple nodes.
  We already use the singular term "qset config" elsewhere for a single
  node's qset, and it would be good to avoid colliding.

#### (Medium) Audit use of sets & maps with explicit comparator

We use types like `std::set<E, decltype(nodeid_cmp)*>`
and `std::map<K, V, decltype(nodeid_cmp)*>`
to key on `stellar::NodeID` in quite a few places.
Values of these types must be constructed by explicitly passing in the
`nodeid_cmp` comparator function because `stellar::NodeID` doesn't have a
global comparison operator.
It's easy to construct these values incorrecly, leading to crashes at runtime.

Audit the use of these types and consider wrapping them to prevent incorrect
construction.
Alternatively, give `stellar::NodeID` and `stellar::SCPQuorumSet` global
operators (seem corresponding issue).

#### (Medium) Decide whether to have global `operator==` and `operator<` for `stellar::NodeID` and `stellar::SCPQuorumSet`

Right now much of the project is made complex because of the use of explicit
local implementations of `operator==` and `operator<` for `stellar::NodeID` and
`stellar::SCPQuorumSet`.
These operators are (respectively) a required part of IVy integration and use
of the types in C++ sets and maps.
This situation creates unsafety because, throughout the project it is necessary
to construct sets and maps by explicitly passing in a comparator function for
`stellar::NodeID`; if it is forgotten, runtime failures will occur.

Since this project might eventually be a part of stellar core, I was hesitant
to just define my own global `operator==` and `operator<` for the domain types
`stellar::NodeID` and `stellar::SCPQuorumSet`.
We'd need to have a discussion with the stellar core team about how they use
these XDR types and whether they want to have such operators defined for them.

#### (Medium) Refactor the (de)serialization of `scp_qset` and/or `stellar::SCPQuorumSet`
#### (Medium) Reorganize the repo as a C++ project

This project and makefile are set up as an IVy project with some C++ code that
has grown around it.
The problem is that I've certainly done a few things wrong, e.g., the C++
libraries are all `*.hpp` files complete with preprocessor macros to prevent
double inclusion.
Presumably we'd prefer to have a more traditional build-the-object-files and
link-the-object-files approach that allows separate compilation.
The current approach requires all the source to be available for a single `g++`
call.
To do this we'd need to also separate out some headers for the cpp libraries.

#### (Medium) Implement `scp_node` as a wrapper for `stellar::NodeID`

We will need to complete the wrapper for `stellar::NodeID` which has been
started as `scp_node` in `stellar_data.ivy`.
While it is possible to follow the example of `scp_qset` in `stellar_data.ivy`
(which wraps `stellar::SCPQuorumSet`), it's not clear that example is a correct
usage of IVy's functionality for wrapping C++ classes.
Start by researching how IVy's designers intended for C++ classes to be
accessed and used from IVy.

#### (Medium) Move `paxos_adapter::AdaptedQSet` into `scp_qset`

`paxos_adapter::AdaptedQSet` in `libscp/PaxosAdapter.hpp` is tightly coupled
with `scp_qset` in `stellar_data.ivy` because they are effectively the one class.
However, they live in two separate files and are two distinct classes (an
`instance` of `scp_qset` generates a child class of `paxos_adapter::AdaptedQSet`).

Possible resolutions:

* It should be quite easy to move the definition of `paxos_adapter::AdaptedQSet` to
  a C++ block within `scp_qset`.
* A more complete resolition of this issue would be to merge them into one class,
  but that might not be possible because IVy requires methods on the parent of an
  `scp_qset` instance (namely, `_hash`, `operator==`, and `operator<<`) which are
  not defined on `stellar::SCPQuorumSet`.
* Another option would be to eliminate `paxos_adapter::AdaptedQSet` by moving
  the three methods to its parent, `stellar::SCPQuorumSet`. It's not clear if
  adding those implementations to `stellar::SCPQuorumSet` would be a hindrance
  to possible integration into stellar-core.

#### (Large) Refactor to use `stellar::NodeID`.

Currently the whole project identifies nodes with an `instance node : iterable` in
`paxos.ivy` (a sequential numeric identifier).
This is a problem because an SCP implementation identifies nodes with public keys.
In particular, `is_quorum` in `libscp/PaxosAdapter.hpp` should identify nodes
with `stellar::NodeID`.

The current version works because of a hack. We have a consistent way
to munge *both* the IVy `node` numbers and also JSON strings into `stellar::NodeID`:
We convert the `node` number to its ASCII representation and copy the first 32
`char` of the string into the an ed25519-constructed `stellar::NodeID`.
These two paths originate at:

* For IVy `node` numbers: `int_to_nodeid` in `libscp/PaxosAdapter.hpp`
* For JSON strings: `jstr_to_nid` in `libscp/StellarJsonXdr.hpp`

One way to fix this problem is as follows:

1. Change the `impl` isolate in `paxos.ivy` to track a 1:1 mapping between `node`
   numbers and `stellar:NodeID` values (learned from network messages).
   The IVy level will continue to identify nodes by their `node` number, and
   the C++ level will continue to use `stellar::NodeID` values.
2. Whenever information is received from the network, use the `stellar::NodeID`
   to obtain the `node` number. Whenever information is passed from the IVy
   level to the C++ level, convert from `node` number to `stellar::NodeID`.

It's not clear whether the C++ level needs to be aware of the 1:1 mapping. It would
be cleaner to keep it on one side of the boundary, if possible, likely the IVy
side.

One thing that makes this confusing is that it means different nodes will learn
a different mapping of `node` numbers to `stellar::NodeID`, because the `node`
numbers will be assigned on a first-come-first-served basis.
