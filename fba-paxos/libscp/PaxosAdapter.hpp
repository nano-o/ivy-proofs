#ifndef INCLUDE_PAXOS_ADAPTER_HPP_
#define INCLUDE_PAXOS_ADAPTER_HPP_

#include <iostream>
#include <libscp/StellarJsonXdr.hpp>
#include <libscp/QSetQuorumChecker.hpp>

namespace paxos_adapter
{
    using NID = stellar::NodeID;
    using X = Slice<stellar::NodeID>;
    using XS = stellar::SCPQuorumSet;

    NID int_to_nodeid(int const& n)
    {
        return conv_nid(std::to_string(n));
    }

    X arrayset_to_slice(std::vector<int> const& ns)
    {
        X x(nodeid_cmp);
        for (auto const& n: ns) { x.insert(int_to_nodeid(n)); }
        return x;
    }

    // call from ivy array_set with self.repr
    int ENTRY_POINT(std::vector<int> const& ns)
    {
        X candidate = arrayset_to_slice(ns);
        //std::cout << "{";
        //for (auto const& c: candidate) { std::cout << c.ed25519() << ' '; }
        //std::cout << "}" << std::endl;
        return 0;
    }
}

#endif // INCLUDE_PAXOS_ADAPTER_HPP_
