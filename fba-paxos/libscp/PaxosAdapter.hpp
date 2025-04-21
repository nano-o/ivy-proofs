#ifndef INCLUDE_PAXOS_ADAPTER_HPP_
#define INCLUDE_PAXOS_ADAPTER_HPP_

#include <fstream>
#include <iostream>
#include <libscp/StellarJsonXdr.hpp>
#include <libscp/QSetQuorumChecker.hpp>

namespace paxos_adapter
{
    class AdaptedQSet : public stellar::SCPQuorumSet {
        public:
            inline size_t __hash() const
            {
                // TODO
            }
    };

    inline bool operator ==(AdaptedQSet const& self, AdaptedQSet const& other)
    {
        return qset_eq(self, other);
    }

    std::ostream& operator<<(std::ostream& os, AdaptedQSet const& obj)
    {
        // TODO
    }

    QSetQuorumChecker::Node int_to_nodeid(int const& n)
    {
        return str_to_nid(std::to_string(n));
    }

    QSetQuorumChecker::X arrayset_to_slice(std::vector<int> const& ns)
    {
        QSetQuorumChecker::X x(nodeid_cmp);
        for (auto const& n: ns) { x.insert(int_to_nodeid(n)); }
        return x;
    }

    // call from ivy array_set with self.repr
    bool is_quorum(int const& n, std::vector<int> const& ns)
    {
        // TODO-1: instead of loading this static qset-config for every
        // is_quorum call, instantiate it once when array_set is created
        //
        // TODO-2: after instantiating this static qset-config once on start,
        // change to instantiating an empty one on start; then use information
        // received in messages to update it during runtime
        std::fstream node_qsets_fd("qset_config.json");
        QSetQuorumChecker::NodeXS node_qsets = load_jnodeslices(node_qsets_fd);

        //// Step 1. Convert the inputs to node identifiers.

        // TODO: the self-nid should be a key not munged int-data
        QSetQuorumChecker::Node self = int_to_nodeid(n);
        // TODO: the candidate set should be a set of keys, not munged int-data
        QSetQuorumChecker::X candidate = arrayset_to_slice(ns);

        //// Step 2. Fetch a QSet for the self-node and each candidate-node.

        // fetch the self-node's qset
        QSetQuorumChecker::XS self_qset;
        {
            auto const& found = node_qsets.find(self);
            if (found == node_qsets.end())
            {
                throw "self's qset not found in node_qsets"; // FIXME: use an exception
            }
            self_qset = found->second;
        }

        // fetch each qset for nodes in the candidate
        QSetQuorumChecker::NodeXS candidate_qsets(nodeid_cmp);
        for(auto const& nid: candidate)
        {
            auto const& found = node_qsets.find(nid);
            if (found == node_qsets.end())
            {
                throw "candidate node's qset not found in node_qsets"; // FIXME: use an exception
            }
            candidate_qsets.insert(*found);
        }

        // find a quorum within the candidate set
        QSetQuorumChecker qsqc;
        QSetQuorumChecker::X quorum = qsqc.findQuorum(candidate_qsets);

        // check whether the current node recognizes this quorum
        return qsqc.containsQuorumSlice(quorum, self_qset);
    }
}

#endif // INCLUDE_PAXOS_ADAPTER_HPP_
