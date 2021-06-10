#!/bin/bash

protobuf=$((readlink $0 || printf $0) | xargs dirname)
SED=$(which gsed || which sed)

if [[ -z "$protobuf" ]]; then
    echo "error: don't know where we are!" 1>&2
    exit 1
fi

cd "$protobuf"

hprotoc="$(which hprotoc)"

if [[ -z "$hprotoc" ]]; then
    echo "error: can't continue without hprotoc" 1>&2
    echo "to fix:" 1>&2
    echo 1>&2
    echo "cabal install hprotoc" 1>&2
    exit 1
fi

$SED -e 's/Rpb//g' -e 's/Req\>/Request/g' -e 's/Resp\>/Response/g' \
    -e 's/MapRedR/MapReduceR/g' -e 's/DelR/DeleteR/' -e 's/ClientId/ClientID/' \
    -e 's/GetServerInfoResponse/ServerInfo/g' \
    -e 's/MapReduceResponse/MapReduce/g' \
    -e '/java/Id' \
    -e 's/Yokozuna/Yz/' \
    -e '/import "riak.proto"/d' \
    src/riak.proto src/riak_kv.proto src/riak_dt.proto src/riak_yokozuna.proto src/riak_search.proto src/riakextra.proto > src/Protocol.proto

(cd src && hprotoc -p Network.Riak Protocol.proto)

rm src/Protocol.proto
