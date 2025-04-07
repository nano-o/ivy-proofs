#ifndef INCLUDE_QUORUM_CHECKER_HPP_
#define INCLUDE_QUORUM_CHECKER_HPP_

#include <xdrpp/types.h>
#include "xdr/Stellar-SCP.h"
#include <iostream>
#include <map>
#include <set>
#include <vector>

// HACK: Ought to be some way to sort NodeID, but this is probably not it.
//
// NOTE: the std::set and std::map types in this file all have
// <...,decltype(nodeid_cmp)*> because stellar::NodeID doesn't have operator<
// defined. It's pretty messy.
bool nodeid_cmp(stellar::NodeID const& a, stellar::NodeID const& b)
{
    return a.ed25519() < b.ed25519();
}

template<class NID>
using Slice = std::set<NID, decltype(nodeid_cmp)*>;

template<class NID, class QuorumSlices>
class QuorumChecker
{
    public:
        virtual ~QuorumChecker() {}

        // Search for a quorum among the given a map (of nodes and their
        // last-known quorum slices), otherwise return the empty set.
        virtual Slice<NID> findQuorum(std::map<NID, QuorumSlices, decltype(nodeid_cmp)*> const&) = 0;

        // Is the candidate slice a quorum for the given quorum-slices? (I.e.
        // Is any one of the quorum-slices a subset of the candidate slice?)
        virtual bool isQuorumSlice(Slice<NID> const&, QuorumSlices const&) = 0;

        // TODO: is-blocking-set function?
};

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
                for (auto const& node : candidate) // std::erase_if in c++20
                {
                    if (!isQuorumSlice(candidate, m.find(node)->second))
                    {
                        candidate.erase(node); // XXX possible iterator invalidation
                    }
                }
            }
            while(size_before_filtering != candidate.size());
            return candidate;
        }

        virtual bool isQuorumSlice(X const& candidate, XS const& quorumSlices)
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

#endif // INCLUDE_QUORUM_CHECKER_HPP_
