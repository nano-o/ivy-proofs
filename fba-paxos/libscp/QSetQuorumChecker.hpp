#ifndef INCLUDE_QSET_QUORUM_CHECKER_
#define INCLUDE_QSET_QUORUM_CHECKER_

#include <algorithm>
#include <libscp/QuorumChecker.hpp>


class QSetQuorumChecker : public QuorumChecker< stellar::NodeID, stellar::SCPQuorumSet >
{
    public:
        using Node = stellar::NodeID;
        using X = Slice<stellar::NodeID>;
        using XS = stellar::SCPQuorumSet;
        using NodeXS = std::map<Node, XS, decltype(nodeid_cmp)*>;

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual X findQuorum(NodeXS const& m)
        {
            X candidate(nodeid_cmp);
            for (auto const& kv: m) { candidate.insert(kv.first); }
            // remove nodes that do not recognize the quorum candidate
            int size_before_filtering;
            do
            {
                size_before_filtering = candidate.size();
                for (auto const& node : X(candidate))
                {
                    if (!containsQuorumSlice(candidate, m.find(node)->second))
                    {
                        candidate.erase(node);
                    }
                }
            }
            while(size_before_filtering != candidate.size());
            return candidate;
        }

        virtual bool containsQuorumSlice(X const& candidate, XS const& qset)
        {
            // This implementation allocates a lot more than
            // https://github.com/stellar/stellar-core/blob/c1746e764983020a725415b017ba0863e15292ec/src/scp/LocalNode.cpp#L93-L122
            // but hopefully still has some value for its simplicity.

            X validators(qset.validators.begin(), qset.validators.end(),
                         nodeid_cmp);

            X cs_and_vs(nodeid_cmp);
            std::set_intersection(validators.begin(), validators.end(),
                                  candidate.begin(), candidate.end(),
                                  std::inserter(cs_and_vs, cs_and_vs.begin()),
                                  nodeid_cmp);

            auto count_sat = std::count_if(qset.innerSets.begin(), qset.innerSets.end(),
                    [&](XS const& x){ return containsQuorumSlice(candidate, x); });
            return qset.threshold <= cs_and_vs.size() + count_sat;
        }
};

#endif // INCLUDE_QSET_QUORUM_CHECKER_

