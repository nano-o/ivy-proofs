The file `multi_paxos_chand.ivy` contains a formalization of multi-paxos following the presentation of Chand et al. found at  [https://arxiv.org/abs/1606.01387]. You will find an Ivy specification closely following the TLA+ specficiation of Chand et al. in isolate `protocol`. The isolate `abstract_protocol` specifies a more abstract version of multi-paxos, which we prove satisfies the agreement property of (multi-)consensus. We then prove by refienement that the `protocol` isolate satisfies agreement.

The file `multi_paxos_chand.ivy` has been successfully checked with Ivy 1.7,
installed from [https://github.com/Microsoft/ivy], commit `108385d`, with the
command `ivy_check complete=fo seed=$RANDOM multi_paxos_chand.ivy`. As far as
I know, `complete=fo` disables the fragment checker (indeed, not all the VCs
are in EPR); with `seed=$RANDOM`, you can rerun Ivy and hope for better results
if it gets stuck.

