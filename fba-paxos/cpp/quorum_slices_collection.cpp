#include <map>
#include <set>
#include <vector>


// (interface)

template<class NodeId>
using Slice = std::set<NodeId>;

// FIXME: QuorumSlices should be an associated-type-alias, not a parameter
template<class NodeId, class QuorumSlices>
class QuorumSlicesCollection
{
    public:
        virtual ~QuorumSlicesCollection() {}

        // Add or update the node's set of quorum slices.
        virtual void Add(NodeId, QuorumSlices) = 0;

        // Is the set a quorum-set for the nodes in this collection?
        virtual bool IsQuorum(Slice<NodeId>) = 0;

        // Is the set a blocking-set for the nodes in this collection?
        virtual bool IsBlocking(Slice<NodeId>) = 0;
};


// (a naive implementation)

// Quorum slices, stored naively.
template<class NodeId>
using NaiveQuorumSlices = std::vector< Slice<NodeId> >;

// Quorum slices collection, implemented naively.
template<class NodeId>
class NaiveQuorumSlicesCollection
    : public QuorumSlicesCollection< NodeId, NaiveQuorumSlices<NodeId> >
{
    using Sl = Slice<NodeId>;
    using QSlices = NaiveQuorumSlices<NodeId>;

    std::map< NodeId, QSlices > storage;

    public:
        virtual void Add(NodeId nid, QSlices qs)
        {
            // TODO
        }
        virtual bool IsQuorum(Sl s)
        {
            return false; // TODO
        }
        virtual bool IsBlocking(Sl s)
        {
            return false; // TODO
        }
};

void usage_example(QuorumSlicesCollection< long long, NaiveQuorumSlices<long long> >& qsc)
{
}

int main()
{
    NaiveQuorumSlicesCollection<long long> x;
    usage_example(x);
}
