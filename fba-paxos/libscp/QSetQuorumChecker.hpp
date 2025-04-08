#ifndef INCLUDE_QSET_QUORUM_CHECKER_
#define INCLUDE_QSET_QUORUM_CHECKER_

#include <algorithm>
#include <libscp/QuorumChecker.hpp>

class QSetQuorumChecker : public QuorumChecker< stellar::NodeID, stellar::SCPQuorumSet >
{
    using NID = stellar::NodeID;
    using X = Slice<stellar::NodeID>;
    using XS = stellar::SCPQuorumSet;

    public:

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual X findQuorum(std::map<NID, XS, decltype(nodeid_cmp)*> const& m)
        {
            X x;
            return x;
        }

        virtual bool isQuorumSlice(X const& candidate, XS const& qset)
        {
            // This implementation allocates a lot more than
            // https://github.com/stellar/stellar-core/blob/c1746e764983020a725415b017ba0863e15292ec/src/scp/LocalNode.cpp#L93-L122
            // but hopefully still has some value for its simplicity.

            X validators(qset.validators.begin(), qset.validators.end(),
                         nodeid_cmp); // XXX nodeid_cmp doesn't seem to be required here, but it ought to be

            X cs_and_vs;
            std::set_intersection(validators.begin(), validators.end(),
                                  candidate.begin(), candidate.end(),
                                  std::inserter(cs_and_vs, cs_and_vs.begin()),
                                  nodeid_cmp);

            if (qset.innerSets.empty())
            {
                return qset.threshold <= cs_and_vs.size();
            }
            else
            {
                auto count_sat = std::count_if(qset.innerSets.begin(), qset.innerSets.end(),
                        [&](XS const& x){ return this->isQuorumSlice(candidate, x); });
                return qset.threshold <= cs_and_vs.size() + count_sat;
            }
        }
};

#endif // INCLUDE_QSET_QUORUM_CHECKER_

