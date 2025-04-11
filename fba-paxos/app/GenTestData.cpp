#include <libscp/StellarJsonXdr.hpp>

int main (int argc, char ** argv)
{
    std::cout
        << "Parse a json array of {publicKey:JSON,quorumSet:QSET} on stdin."
        << std::endl
        << "Each is output to a separate xdr file in the data/ directory."
        << std::endl;

    for (auto const& kv : load_jnodeslices(std::cin))
    {
        std::string name = nid_to_str(kv.first);
        std::cout << xdr::xdr_to_string(kv.second, name.c_str());

        std::string path = "data/" + name + ".xdr";
        size_t w_count = dump_xdr(kv.second, path);
        std::cout << "Wrote: " << path << std::endl;

        stellar::SCPQuorumSet qset2 = {};
        size_t r_count = load_xdr(path, qset2);
        std::cout << "Read: " << path << std::endl;
        assert(w_count == r_count);
        assert(qset_eq(kv.second, qset2));
    }
}

