#ifndef INCLUDE_STELLAR_JSON_XDR_HPP_
#define INCLUDE_STELLAR_JSON_XDR_HPP_

#include <sys/stat.h>
#include <iostream>
#include <fstream>
#include <libscp/vendor/json.hpp>
#include <xdrpp/marshal.h>
#include <xdrpp/printer.h>
#include <xdr/Stellar-SCP.h>
#include <libscp/QuorumChecker.hpp>

using json = nlohmann::json;

// Convert a std::string to an XDR NodeID.
stellar::NodeID str_to_nid(std::string const& s_)
{
    xdr::opaque_array<32> key = {};
    std::string s = s_.substr(0, 32);
    std::copy(s.begin(), s.end(), key.data());

    stellar::NodeID nid(stellar::PUBLIC_KEY_TYPE_ED25519);
    nid.type(stellar::PUBLIC_KEY_TYPE_ED25519);
    nid.ed25519() = key;

    return nid;
}

// Convert an XDR NodeID back to a std::string. Probably not useful for real keys.
//
// FIXME: Assumes the NodeID is PUBLIC_KEY_TYPE_ED25519
std::string nid_to_str(stellar::NodeID const& nid)
{
        xdr::opaque_array<32> key = nid.ed25519();
        std::string s;
        for (auto const& c: key)
        {
            if (c == '\0') { break; }
            s.push_back(c);
        }
        return s;
}

// Convert a JSON string-value to an XDR NodeID.
stellar::NodeID jstr_to_nid(json jnid)
{
    return str_to_nid(jnid);
}

// Convert a JSON {theshold:number, validators:[string],
// innerQuorumSets:[qset]} object to an XDR SCPQuorumSet.
stellar::SCPQuorumSet jqset_to_qset(json jqset)
{
    stellar::SCPQuorumSet qset;
    qset.threshold = jqset["threshold"]; // XXX coercion to uint32
    for (json const& jvalidator: jqset["validators"])
    {
        qset.validators.push_back(jstr_to_nid(jvalidator));
    }
    for (json const& inner_jqset: jqset["innerQuorumSets"])
    {
        qset.innerSets.push_back(jqset_to_qset(inner_jqset));
    }
    return qset;
}

bool nid_eq(stellar::NodeID const& a, stellar::NodeID const& b)
{
    // FIXME: distinguish the cases
    return a.ed25519() == b.ed25519();
}

bool qset_eq(stellar::SCPQuorumSet const& a, stellar::SCPQuorumSet const& b)
{
    if (a.threshold != b.threshold ||
        a.validators.size() != b.validators.size() ||
        a.innerSets.size() != b.innerSets.size())
        return false;

    for(int i=0; i<a.validators.size(); i+=1)
    {
        if (!nid_eq(a.validators[i], b.validators[i]))
            return false;
    }
    for(int i=0; i<a.innerSets.size(); i+=1)
    {
        if(!qset_eq(a.innerSets[i], b.innerSets[i]))
            return false;
    }
    return true;
}

// Write out xdr data to a path, in network-message format.
template<class XDR_TYPE>
size_t dump_xdr(XDR_TYPE const& xdr_value, std::string path)
{
    xdr::msg_ptr m = xdr::xdr_to_msg(xdr_value);
    auto fd = std::fopen(path.c_str(), "wb");
    size_t w_count = std::fwrite(m->raw_data(), sizeof(char), m->raw_size(), fd);
    if(std::ferror(fd))
    {
        std::perror((std::string("dump_xdr to '")+path+std::string("' failed")).c_str());
    }
    else if (m->raw_size() != w_count)
    {
        std::cerr << "wrote " << w_count << " to '" << path << "' but expected to write " << m->raw_size() << std::endl;
    }
    std::fflush(fd);
    std::fclose(fd);
    return w_count;
}

// Read xdr data (in network-message format) from a path. Return bytes read on
// success or -1 on failure.
template<class XDR_TYPE>
size_t load_xdr(std::string path, XDR_TYPE& xdr_value)
{
    struct stat st;
    if (0 != stat(path.c_str(), &st))
    {
        perror(("failed to stat '"+path+"'").c_str());
        return -1;
    }
    else if (st.st_size < 4)
    {
        std::cerr << "size of '" << path << "' is too small to be an xdr network message" << std::endl;
        return -1;
    }
    char buf[st.st_size];
    auto fd = std::fopen(path.c_str(), "rb");
    size_t r_count = std::fread(buf, sizeof(buf[0]), st.st_size, fd);
    if (std::ferror(fd))
    {
        perror(("read error for '"+path+"'").c_str());
        return -1;
    }
    else if (r_count != st.st_size)
    {
        perror(("didn't read all of '"+path+"'").c_str());
        return -1;
    }
    // copy buf into a msg of correct length (alloc function excludes 4 byte
    // length) because unmarshal requires that
    xdr::msg_ptr m = xdr::message_t::alloc(r_count - 4);
    std::copy(buf, buf + r_count, m->raw_data());
    xdr::xdr_from_msg(m, xdr_value);
    return r_count;
}

// Read a json file containing [{publicKey:JSON,quorumSet:QSET}] to a map.
std::map<stellar::NodeID, stellar::SCPQuorumSet, decltype(nodeid_cmp)*>
    load_jnodeslices(std::istream & stream)
{
    std::map<stellar::NodeID, stellar::SCPQuorumSet, decltype(nodeid_cmp)*>
            m(nodeid_cmp);
    for (json const& element: json::parse(stream))
    {
        auto pk = jstr_to_nid(element["publicKey"]);
        auto qset = jqset_to_qset(element["quorumSet"]);
        m.insert({pk, qset});
    }
    return m;
}

#endif // INCLUDE_STELLAR_JSON_XDR_HPP_
