#include <libscp/QuorumChecker.hpp>

template<class NID, class QuorumSlices>
void local_node_example(
        QuorumChecker<NID, QuorumSlices> const& checker,
        std::map<NID, QuorumSlices> const& nodeStates,
        QuorumSlices const& localNodeSlices)
{
    Slice<NID> quorum = checker.findQuorum(nodeStates);
    if (quorum.empty())
    {
        std::cout << "No quorum set in the given node states." << std::endl;
        return;
    }
    std::cout << "Found a quorum of " << quorum.size() << " nodes." << std::endl;

    if (checker.isQuorumSlice(quorum, localNodeSlices))
    {
        std::cout << "The local node recognizes this quorum." << std::endl;
        return;
    }
    std::cout << "The local node doesn't recognize this quorum." << std::endl;
}


int main()
{
    NaiveQuorumChecker<stellar::NodeID> checker;
    //local_node_example(checker); // TODO some inputs

    // TODO: do another matching the types in xdr/Stellar-SCP.h
}
