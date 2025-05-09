#ifndef INCLUDE_PAXOS_ADAPTER_HPP_
#define INCLUDE_PAXOS_ADAPTER_HPP_

#include <fstream>
#include <iostream>
#include <libscp/StellarJsonXdr.hpp>
#include <libscp/QSetQuorumChecker.hpp>

namespace paxos_adapter
{
    class AdaptedQSet : public stellar::SCPQuorumSet
    {

        public:
            AdaptedQSet() {}
            AdaptedQSet(stellar::SCPQuorumSet x) : stellar::SCPQuorumSet{x} {}

            size_t __hash() const
            {
                std::string x = xdr::xdr_to_string<stellar::SCPQuorumSet>(*this);
                return std::hash<std::string>{}(x);
            }
            std::string encode() const
            {
                xdr::msg_ptr m = xdr::xdr_to_msg<stellar::SCPQuorumSet>(*this);
                return std::string(m->raw_data(), m->raw_data() + m->raw_size());
            }
            static void decode(std::string s, AdaptedQSet & out)
            {
                xdr::msg_ptr m = xdr::message_t::alloc(s.length() - 4);
                std::copy(s.begin(), s.end(), m->raw_data());
                xdr::xdr_from_msg<stellar::SCPQuorumSet>(m, out);
            }
    };

    bool operator==(AdaptedQSet const& self, AdaptedQSet const& other)
    {
        return qset_eq(self, other);
    }

    std::ostream& operator<<(std::ostream& os, AdaptedQSet const& self)
    {
        os << xdr::xdr_to_string<stellar::SCPQuorumSet>(self);
        return os;
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



    std::vector<long long> chars_to_llongs(std::string s)
    {
        size_t len = (s.length() / sizeof(long long)) + (0 != s.length() % sizeof(long long));
        std::vector<long long> out(len);
        //DEBUG printf("lllen:%zu, strlen:%zu, outlen:%zu,%zu\n", sizeof(long long), s.length(), len, out.size());
        assert(s.length() <= out.size() * sizeof(long long)); // enough space?
        std::copy(s.begin(), s.end(), (char *) out.data()); // reinterpret the output vector as a char array
        //DEBUG for (auto ll: out) { printf("[%-16llx](", ll); for(int i=0; i<sizeof(long long); i += 1) { printf("%c,", *(((char *) &ll) + i)); }; printf(")\n"); }
        return out;
    }

    std::string llongs_to_chars(size_t sl, std::vector<long long> xs)
    {
        assert(sl <= xs.size() * sizeof(long long)); // enough space?
        std::string s(sl, '\0');
        std::copy((char *) xs.data(), ((char *) xs.data()) + sl, (char *) s.data()); // reinterpret the input vector as a char array
        //DEBUG for (auto c: s) { printf("%c,", c); } printf("\n");
        return s;
    }



    // call from ivy array_set with self.repr
    bool is_quorum(
                                            int const& n, // current node's ivy nodeid
                          stellar::SCPQuorumSet const& self_qset, // current node's qset
                              std::vector<int> const& ns, // candidate sets' ivy nodeids
            std::vector<stellar::SCPQuorumSet> const& qs  // candidate sets' qsets
            )
    {
        //// Step 1. Convert the inputs to node identifiers.

        // TODO: the self-nid should be a key, not munged int-data
        QSetQuorumChecker::Node self = int_to_nodeid(n);
        // TODO: the candidate set should be a set of keys, not munged int-data
        QSetQuorumChecker::X candidate = arrayset_to_slice(ns);

        //// Step 2. Fetch a QSet for the self-node and each candidate-node.

        // fetch each qset for nodes in the candidate
        QSetQuorumChecker::NodeXS candidate_qsets(nodeid_cmp);
        assert(ns.size() == qs.size()); // ns and qs are keys and values, so they must correspond
        for(int i=0; i<ns.size(); i+=1)
        {
            candidate_qsets.insert({int_to_nodeid(ns[i]), qs[i]});
        }

        // find a quorum within the candidate set
        QSetQuorumChecker qsqc;
        QSetQuorumChecker::X quorum = qsqc.findQuorum(candidate_qsets);

        // check whether the current node recognizes this quorum
        return qsqc.containsQuorumSlice(quorum, self_qset);
    }
}

#endif // INCLUDE_PAXOS_ADAPTER_HPP_
