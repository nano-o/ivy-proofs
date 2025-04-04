#include <iostream>
#include <data/json.hpp>
#include <xdrpp/marshal.h>
#include <xdrpp/printer.h>
#include "xdr/Stellar-SCP.h"

using json = nlohmann::json;

stellar::SCPQuorumSet load_qset(json qset)
{
    stellar::SCPQuorumSet x;
    // TODO: actually populate the thing
    return x;
}

int main (int argc, char ** argv)
{
    std::cout << "Parses a json array of {publicKey:JSON,quorumSet:QSET} on stdin. Each is output to a separate xdr file." << std::endl;
    for (json const& element: json::parse(std::cin))
    {
        std::string pk = element["publicKey"];
        auto qset = load_qset(element["quorumSet"]);
        std::cout << xdr::xdr_to_string(qset, pk.c_str());

        xdr::msg_ptr m = xdr::xdr_to_msg(qset);
        std::FILE* fd = std::fopen((pk + ".xdr").c_str(), "wb");
        std::fwrite(m->raw_data(), sizeof(char), m->raw_size(), fd);
    }
}
