#include <xdrpp/types.h>
#include "xdr/Stellar-SCP.h"
#include <iostream>
#include <map>
#include <set>
#include <vector>


// HACK: Ought to be some way to sort NodeID, but this is probably not it.
bool nodeid_cmp(stellar::NodeID const& a, stellar::NodeID const& b)
{
    return a.ed25519() < b.ed25519();
}


// (interface)

template<class NID>
using Slice = std::set<NID>;

template<class NID, class QuorumSlices> class QuorumChecker
{
    public:
        virtual ~QuorumChecker() {}

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual Slice<NID> findQuorum(std::map<NID, QuorumSlices>) = 0;

        // Is the candidate slice a quorum for the given quorum-slices? (I.e.
        // Is any one of the quorum-slices a subset of the candidate slice?)
        virtual bool isQuorumSlice(Slice<NID>, QuorumSlices) = 0;

        // TODO: is-blocking-set function?
};


// (a naive implementation)

// Quorum slices, stored naively.
template<class NID>
using NaiveQuorumSlices = std::vector< Slice<NID> >;

template<class NID>
class NaiveQuorumChecker : public QuorumChecker< NID, NaiveQuorumSlices<NID> >
{
    using X = Slice<NID>;
    using XS = NaiveQuorumSlices<NID>;

    public:

        virtual Slice<NID> findQuorum(std::map<NID, XS> m)
        {
            X x;
            // TODO
            return x;
        }

        virtual bool isQuorumSlice(X candidate, XS quorumSlices)
        {
            for (X& slice : quorumSlices)
            {
                // a⊆b ≅ (∅ ≡ a⁄b)
                std::set<stellar::NodeID, decltype(nodeid_cmp)*> extras(nodeid_cmp);
                //X extras; // gives cmp error
                std::set_difference(slice.begin(), slice.end(),
                                    candidate.begin(), candidate.end(),
                                    std::inserter(extras, extras.begin()),
                                    nodeid_cmp);
                if (extras.empty())
                {
                    return true;
                }
            }
            return false;
        }

        // TODO: is-blocking-set function?
};


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
