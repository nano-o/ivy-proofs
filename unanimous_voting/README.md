# A trivial consensus protocol

`voting.ivy` specifies a trivial voting protocol that is safe but not live.

Check the proof with `ivy_check voting.ivy`. Note that verifying what's usually called the validity property is left as an excercise.

Compile it to an executable using `ivyc voting.ivy`.
For example, you can start 3 nodes 0, 1, and 2 by running `./voting 2 0`, `./voting 2 1`, `./voting 2 2` in 3 different terminals (first arguments is max node ID and second argument is current node ID).
You will get 3 REPLs. Call `interface.propose(1)` in one of them, and watch them all decide 1.

# Installing Ivy

Follow the instructions on the [Ivy website](https://kenmcmil.github.io/ivy/install.html) (but I would use a python virtual environment and not run stuff as root)
