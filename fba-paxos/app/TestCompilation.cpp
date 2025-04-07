#include <xdrpp/types.h>
#include <xdr/Stellar-SCP.h>
#include <iostream>
#include <libscp/QuorumChecker.hpp>
#include <libscp/QSetQuorumChecker.hpp>

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

int main() {
    stellar::SCPQuorumSet quorumSet;
    quorumSet.threshold = 2;

    stellar::NodeID validator1(stellar::PUBLIC_KEY_TYPE_ED25519);
    xdr::opaque_array<32> key1 = {};
    key1[0] = 1;
    validator1.type(stellar::PUBLIC_KEY_TYPE_ED25519);
    validator1.ed25519() = key1;
    quorumSet.validators.push_back(validator1);

    stellar::SCPQuorumSet innerSet;
    innerSet.threshold = 1;
    stellar::NodeID validator2(stellar::PUBLIC_KEY_TYPE_ED25519);
    xdr::opaque_array<32> key2 = {};
    key2[0] = 42;
    validator2.type(stellar::PUBLIC_KEY_TYPE_ED25519);
    validator2.ed25519() = key2;
    innerSet.validators.push_back(validator2);

    quorumSet.innerSets.push_back(innerSet);

    // Print the SCPQuorumSet
    std::cout << "SCPQuorumSet:" << std::endl;
    std::cout << "Threshold: " << quorumSet.threshold << std::endl;
    std::cout << "Validators: " << quorumSet.validators.size() << std::endl;
    std::cout << "InnerSets: " << quorumSet.innerSets.size() << std::endl;

    for (const auto& inner : quorumSet.innerSets) {
        std::cout << "  InnerSet Threshold: " << inner.threshold << std::endl;
        std::cout << "  InnerSet Validators: " << inner.validators.size() << std::endl;
    }

    NaiveQuorumChecker<stellar::NodeID> checker;
    QSetQuorumChecker checkerB;

    return 0;
}
