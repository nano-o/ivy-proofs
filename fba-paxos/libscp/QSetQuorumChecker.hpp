#ifndef INCLUDE_QSET_QUORUM_CHECKER_
#define INCLUDE_QSET_QUORUM_CHECKER_

#include <libscp/QuorumChecker.hpp>

class QSetQuorumChecker : public QuorumChecker< stellar::NodeID, stellar::SCPQuorumSet >
{
    using NID = stellar::NodeID;
    using X = Slice<stellar::NodeID>;
    using XS = stellar::SCPQuorumSet;

    public:

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual X findQuorum(std::map<NID, XS, decltype(nodeid_cmp)*> const&)
        {
            X x;
            return x;
        }

        // Is the candidate slice a quorum for the given quorum-slices? (I.e.
        // Is any one of the quorum-slices a subset of the candidate slice?)
        virtual bool isQuorumSlice(X const&, XS const&)
        {
            return false;
        }
};

#endif // INCLUDE_QSET_QUORUM_CHECKER_

