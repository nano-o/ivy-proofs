# An implementation of single-shot Paxos

Check the proof with `ivy_check complete=fo voting.ivy`.
`complete=fo` tells Ivy not to check that the verification conditions are decidable, and indeed there are some problematic quantifier alternations. Removing them is left as an exercise. Because of this `ivy_check` may get stuck. If so, restart it to try your luck again; it should use a different random seed I think, but if not do `ivy_check seed=$RANDOM complete=fo voting.ivy`.

Note that we cheat a bit: the majority check is implemented in C++ and assumed correct (see `array_set.ivy`).

Compile it to an executable using `ivyc paxos.ivy`.
For example, you can start 3 nodes 0, 1, and 2 by running `./paxos 2 0`, `./paxos 2 1`, `./paxos 2 2` in 3 different terminals (first arguments is max node ID and second argument is current node ID).
You will get 3 REPLs. Call e.g. `interface.propose(42)` in one of them, and watch them all decide 42.
You can also e.g. kill node 1 before calling propose and see if it still works (it should because there is still a quorum left).

# Installing Ivy

Follow the instructions on the [Ivy website](https://kenmcmil.github.io/ivy/install.html) (but I would use a python virtual environment and not run stuff as root)
