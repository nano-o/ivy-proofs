#include <xdrpp/types.h>
#include "xdr/Stellar-SCP.h"
#include <iostream>
#include <map>
#include <set>
#include <vector>


// (interface)

template<class NodeId>
using Slice = std::set<NodeId>;

template<class NodeId, class QuorumSlices> class QuorumChecker
{
    public:
        virtual ~QuorumChecker() {}

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual Slice<NodeId> findQuorum(std::map<NodeId, QuorumSlices>) = 0;

        // Is the candidate slice a quorum for the given quorum-slices? (I.e.
        // Is any one of the quorum-slices a subset of the candidate slice?)
        virtual bool isQuorumSlice(Slice<NodeId>, QuorumSlices) = 0;

        // TODO: is-blocking-set function?
};


// (a naive implementation)

// Quorum slices, stored naively.
template<class NodeId>
using NaiveQuorumSlices = std::vector< Slice<NodeId> >;

template<class NodeId>
class NaiveQuorumChecker : public QuorumChecker< NodeId, NaiveQuorumSlices<NodeId> >
{
    using X = Slice<NodeId>;
    using XS = NaiveQuorumSlices<NodeId>;

    public:

        virtual Slice<NodeId> findQuorum(std::map<NodeId, XS> m)
        {
            X x;
            // TODO
            return x;
        }

        virtual bool isQuorumSlice(X, XS)
        {
            return false; // TODO
        }

        // TODO: is-blocking-set function?
};


template<class NodeId, class QuorumSlices>
void local_node_example(
        QuorumChecker<NodeId, QuorumSlices> const& checker,
        std::map<NodeId, QuorumSlices> const& nodeStates,
        QuorumSlices const& localNodeSlices)
{
    Slice<NodeId> quorum = checker.findQuorum(nodeStates);
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
