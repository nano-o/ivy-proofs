#include <iostream>
#include <data/json.hpp>
#include <xdrpp/marshal.h>
#include <xdrpp/printer.h>
#include "xdr/Stellar-SCP.h"

using json = nlohmann::json;

// JSON string to XDR NodeID.
stellar::NodeID load_jnid(json jnid)
{
    xdr::opaque_array<32> key = {};
    {
        std::string s = jnid;
        // FIXME: ensure that no more than 32 are copied
        std::copy(s.begin(), s.end(), key.data());
    }
    stellar::NodeID nid(stellar::PUBLIC_KEY_TYPE_ED25519);
    nid.type(stellar::PUBLIC_KEY_TYPE_ED25519);
    nid.ed25519() = key;
    return nid;
}

// JSON {theshold:number, validators:[string], innerQuorumSets:[qset]} to XDR SCPQuorumSet.
stellar::SCPQuorumSet load_jqset(json jqset)
{
    stellar::SCPQuorumSet qset;
    qset.threshold = jqset["threshold"]; // XXX coercion to uint32
    for (json const& jvalidator: jqset["validators"])
    {
        qset.validators.push_back(load_jnid(jvalidator));
    }
    for (json const& inner_jqset: jqset["innerQuorumSets"])
    {
        qset.innerSets.push_back(load_jqset(inner_jqset));
    }
    return qset;
}

int main (int argc, char ** argv)
{
    std::cout << "Parses a json array of {publicKey:JSON,quorumSet:QSET} on stdin. Each is output to a separate xdr file." << std::endl;
    for (json const& element: json::parse(std::cin))
    {
        std::cout << element << std::endl;
        std::string pk = element["publicKey"];
        auto qset = load_jqset(element["quorumSet"]);
        std::cout << xdr::xdr_to_string(qset, pk.c_str());

        size_t w_count = 0;
        { // write
            xdr::msg_ptr m = xdr::xdr_to_msg(qset);
            auto fd = std::fopen((pk + ".xdr").c_str(), "wb");
            w_count = std::fwrite(m->raw_data(), sizeof(char), m->raw_size(), fd);
            assert(m->raw_size() == w_count);
            std::fflush(fd);
            std::fclose(fd);
        }
        { // compare for equality
            stellar::SCPQuorumSet qset2;
            { // read
                assert(w_count <= 4096);
                std::array<char, 4096> buf = {};
                auto fd = std::fopen((pk + ".xdr").c_str(), "rb");
                size_t r_count = std::fread(buf.data(), sizeof(buf[0]), 4096, fd);
                assert(!std::ferror(fd));
                assert(std::feof(fd));
                assert(w_count == r_count);
                // copy buf into a msg of correct length (alloc function
                // excludes 4 byte length) because unmarshal requires that
                xdr::msg_ptr msg = xdr::message_t::alloc(r_count - 4);
                std::copy(buf.data(), buf.data() + r_count, msg->raw_data());
                xdr::xdr_from_msg(msg, qset2);
                // TODO: assert equality
            }
        }
    }
}
