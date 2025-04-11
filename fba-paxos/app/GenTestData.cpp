#include <libscp/StellarJsonXdr.hpp>

int main (int argc, char ** argv)
{
    std::cout
        << "Parse a json array of {publicKey:JSON,quorumSet:QSET} on stdin."
        << ' '
        << "Each is output to a separate xdr file in the data/ directory."
        << std::endl;

    for (json const& element: json::parse(std::cin))
    {
        std::cout << "Input: " << element << std::endl;
        std::string pk = element["publicKey"];
        auto qset = jqset_to_qset(element["quorumSet"]);
        std::cout << "Output: " << xdr::xdr_to_string(qset, pk.c_str());

        std::string path = "data/" + pk + ".xdr";
        size_t w_count = dump_xdr(qset, path);
        std::cout << "Wrote: " << path << std::endl;

        stellar::SCPQuorumSet qset2 = {};
        size_t r_count = load_xdr(path, qset2);
        std::cout << "Read: " << path << std::endl;
        assert(w_count == r_count);
        assert(qset_eq(qset, qset2));
    }
}

