bazqux-urweb
============

<a href="http://www.impredicative.com/ur/">Ur/Web</a> part of <a href="https://bazqux.com">BazQux Reader</a> sources.

This is NOT the full sources. Most server-side functionality is written in Haskell. Only Ur/Web sources, Ur/Web<=>Haskell interface and Riak utility functions are presented here. You can compile Ur/Web part but it will fail trying to compile missing Haskell sources.

Code is ugly with experiments, legacy and Russian comments.

If you like to make mixed Ur/Web+Haskell app look into `lib`, `gcc` and `crawler/Gen.hs`.
