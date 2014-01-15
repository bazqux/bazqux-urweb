bazqux-urweb
============

<a href="http://www.impredicative.com/ur/">Ur/Web</a> part of <a href="https://bazqux.com">BazQux Reader</a> sources.

This is NOT the complete sources.

BazQux Reader is written in Ur/Web (client side and a part of web server) and Haskell (major part of web server and feeds fetcher). Only Ur/Web part, Ur/Web<=>Haskell interface and Riak utility functions are presented here. You can even compile Ur/Web sources but it will fail trying to compile missing Haskell sources.

Code is ugly (quick and dirty hacks, experiments, legacy, Russian comments or no comments at all). But it's a working code used everyday by thousands of people.

If you like to make mixed Ur/Web+Haskell app look into `lib`, `gcc` and `crawler/Gen.hs`.

`lib` contains Ur/Web implementation of Haskell Binary package (Haskell function arguments and results are serialized using Binary). `gcc` is a substitute for the real GNU gcc which is called from `urweb` compiler and calls GHC internally. `crawler/Gen.hs` contains description of data types from which both Ur/Web and Haskell data types and serialization functions are generated. It also generates helper Riak funtions for data types stored in Riak and FFI bindings for Haskell functions called from Ur/Web part. It's not a ready to use library but rather the script you must edit.

`crawler/Riak.hs` is a library used for interfacing with Riak. It has caches, utility functions and few optimizations (like decoding Binary values right while reading many keys to minimize latency). `crawler/Lib/riak-0.7.0.1` is a slightly modified `riak` package. It works with latest riak and adds small random delays while resolving conflicting values (to fight race conditions when two threads modify same value simultaneously).
