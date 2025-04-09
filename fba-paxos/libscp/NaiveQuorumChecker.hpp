#ifndef INCLUDE_NAIVE_QUORUM_CHECKER_HPP_
#define INCLUDE_NAIVE_QUORUM_CHECKER_HPP_

#include <libscp/QuorumChecker.hpp>
//#include <algorithm>
//-#include <xdrpp/types.h>
//-#include "xdr/Stellar-SCP.h"
//+#include <xdr/Stellar-SCP.h>
// #include <iostream>
//  #include <map>
//   #include <set>
//   -#include <vector>
//
//

template<class NID>
using NaiveQuorumSlices = std::vector< Slice<NID> >;

template<class NID>
class NaiveQuorumChecker : public QuorumChecker< NID, NaiveQuorumSlices<NID> >
{
    using X = Slice<NID>;
    using XS = NaiveQuorumSlices<NID>;

    public:

        virtual X findQuorum(std::map<NID, XS, decltype(nodeid_cmp)*> const& m)
        {
            X candidate(nodeid_cmp);
            for (auto const& kv : m)
            {
                candidate.insert(kv.first);
            }
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

        virtual bool containsQuorumSlice(X const& candidate, XS const& quorumSlices)
        {
            for (auto const& slice : quorumSlices)
            {
                // a⊆b ≅ (∅ ≡ a⁄b)
                X extras(nodeid_cmp);
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

#endif // INCLUDE_NAIVE_QUORUM_CHECKER_HPP_

