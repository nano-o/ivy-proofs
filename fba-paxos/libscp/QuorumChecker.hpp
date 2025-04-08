#ifndef INCLUDE_QUORUM_CHECKER_HPP_
#define INCLUDE_QUORUM_CHECKER_HPP_

#include <xdr/Stellar-SCP.h>
#include <iostream>
#include <map>
#include <set>

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
        virtual bool containsQuorumSlice(Slice<NID> const&, QuorumSlices const&) = 0;

        // TODO: is-blocking-set function?
};

template<class NID, class QuorumSlices>
bool isQuorum(QuorumChecker<NID, QuorumSlices>& qc, std::map<NID, QuorumSlices, decltype(nodeid_cmp)*> const& m)
{
    Slice<NID> q = qc.findQuorum(m);
    return q.size() == m.size();
}

#endif // INCLUDE_QUORUM_CHECKER_HPP_
