#!/bin/bash

# root="$(hg root)"

# if [[ -z "$root" ]]; then
#     echo "error: don't know where we are!" 1>&2
#     exit 1
# fi

# cd "$root/protobuf"

hprotoc="$(which hprotoc)"

if [[ -z "$hprotoc" ]]; then
    echo "error: can't continue without hprotoc" 1>&2
    echo "to fix:" 1>&2
    echo 1>&2
    echo "cabal install hprotoc" 1>&2
    exit 1
fi

sed -e 's/Rpb//g' -e 's/Req/Request/g' -e 's/Resp/Response/g' \
    -e 's/MapRedR/MapReduceR/g' -e 's/DelR/DeleteR/' -e 's/ClientId/ClientID/' \
    -e 's/GetServerInfoResponse/ServerInfo/g' \
    -e 's/MapReduceResponse/MapReduce/g' \
    src/riak.proto src/riak_kv.proto > src/Protocol.proto

(cd src && hprotoc -p Network.Riak Protocol.proto)
for i in $(find src/Network/Riak/Protocol -name '*.hs';
           echo src/Network/Riak/Protocol.hs); do
    cp /dev/null $i.$$
    echo '{-# OPTIONS_GHC -fno-warn-unused-imports #-}' >> $i.$$
    cat $i >> $i.$$
    mv $i.$$ $i
done

rm src/Protocol.proto
