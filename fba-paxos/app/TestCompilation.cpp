#include <xdrpp/types.h>
#include <xdr/Stellar-SCP.h>
#include <iostream>
#include <libscp/StellarJsonXdr.hpp>
#include <libscp/QuorumChecker.hpp>
#include <libscp/QSetQuorumChecker.hpp>
#include <libscp/NaiveQuorumChecker.hpp>

using NodeQSets = std::map< stellar::NodeID, stellar::SCPQuorumSet, decltype(nodeid_cmp)* >;

// constructor that ensures we add the nodeid_cmp (else segfaults occur on insert)
NodeQSets node_qsets_empty()
{
    NodeQSets m(nodeid_cmp);
    return m;
}

// restrict test data to specified keys
NodeQSets test_restrict(NodeQSets const& m1, std::vector<std::string> const& ks)
{
    auto m2 = node_qsets_empty();
    for (auto const& k: ks)
    {
        auto it = m1.find(str_to_nid(k));
        assert(it != m1.end()); // test_restrict should fail if a name isn't in the test data
        m2.insert(*it);
        std::cout << k << ' ';
    }
    std::cout << ';' << std::endl;
    return m2;
}

void test_QSetQuorumChecker()
{
    auto test_data = node_qsets_empty();
    for (std::string const& name : {"PK11", "PK12", "PK13", "PK21", "PK22", "PK23", "PKX"})
    {
        stellar::NodeID k = str_to_nid(name);
        stellar::SCPQuorumSet v;
        assert(0 < load_xdr("data/" + name + ".xdr", v)); // test should fail if an expected test file isn't found
        test_data.insert({k, v});
    }
    // XXX tests ported from https://github.com/nano-o/python-fbas/blob/83c7c7c7b5c8e4fecbefa88cbafac6ad78ba5e33/tests/fbas_graph_test.py#L24-L36
    QSetQuorumChecker qsqc;
    assert( isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PK13"})));
    assert( isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12"})));
    assert(!isQuorum(qsqc, test_restrict(test_data, {"PK11"})));
    assert(!isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PK13", "PK21"})));
    assert(!isQuorum(qsqc, test_restrict(test_data, {"PK13", "PK21"})));
    assert( isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PK13", "PK21", "PK22", "PK23"})));
    //fbas.is_quorum({"PK11", "PK12", "PK13", "NON_EXISTENT"}) // test not ported
    assert(!isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PK13", "PKX"})));
    assert( isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PKX", "PK22", "PK23"})));
    assert(!isQuorum(qsqc, test_restrict(test_data, {"PK11", "PK12", "PKX", "PK22"})));
}

// test that we can do some things with the QC interface
template<class NID, class QuorumSlices>
int local_node_example(
        QuorumChecker<NID, QuorumSlices>& checker,
        std::map<NID, QuorumSlices, decltype(nodeid_cmp)*> const& nodeStates,
        QuorumSlices const& localNodeSlices)
{
    Slice<NID> quorum = checker.findQuorum(nodeStates);
    if (quorum.empty())
    {
        return -1; // No quorum set in the given node states.
    }
    int qsize = quorum.size();
    if (checker.containsQuorumSlice(quorum, localNodeSlices))
    {
        return qsize; // The local node recognizes this quorum.
    }
    return -qsize; // The local node doesn't recognize this quorum.
}

int main() {
    test_QSetQuorumChecker();

    {
        // XXX test that these templates can be instantiated
        NaiveQuorumChecker<stellar::NodeID> nqc;
        assert(-1 == local_node_example(nqc,
                std::map<stellar::NodeID, std::vector<Slice<stellar::NodeID> >, decltype(nodeid_cmp)*>(),
                std::vector<Slice<stellar::NodeID> >()));
    }
}
