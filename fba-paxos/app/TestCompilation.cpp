#include <xdrpp/types.h>
#include <xdr/Stellar-SCP.h>
#include <iostream>
#include <libscp/StellarJsonXdr.hpp>
#include <libscp/QuorumChecker.hpp>
#include <libscp/QSetQuorumChecker.hpp>
#include <libscp/NaiveQuorumChecker.hpp>


// test that we can do some things with the QC interface
template<class NID, class QuorumSlices>
int local_node_example(
        QuorumChecker<NID, QuorumSlices>& checker,
        std::map<NID, QuorumSlices, decltype(nodeid_cmp)*> const& nodeStates,
        QuorumSlices const& localNodeSlices)
{
    Slice<NID> quorum = checker.findQuorum(nodeStates);
    if (quorum.empty())
    {
        return -1; // No quorum set in the given node states.
    }
    int qsize = quorum.size();
    if (checker.containsQuorumSlice(quorum, localNodeSlices))
    {
        return qsize; // The local node recognizes this quorum.
    }
    return -qsize; // The local node doesn't recognize this quorum.
}

int main() {
    test_QSetQuorumChecker();

    {
        // XXX test that these templates can be instantiated
        NaiveQuorumChecker<stellar::NodeID> nqc;
        assert(-1 == local_node_example(nqc,
                std::map<stellar::NodeID, std::vector<Slice<stellar::NodeID> >, decltype(nodeid_cmp)*>(),
                std::vector<Slice<stellar::NodeID> >()));
    }
}
