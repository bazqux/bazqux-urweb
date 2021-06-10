bazqux-urweb
============

Major part of the <a href="https://bazqux.com">BazQux Reader</a> sources. Excludes configuration files (nginx, ElasticSearch, utilities), static assets (images and commercial fonts) and few source code files (Config.hs with API keys and few experimental files). It won't compile but it could give you an impression of what is going on.

BazQux Reader is written in [Ur/Web](http://www.impredicative.com/ur/) (client side and a part of web server) and [Haskell](https://www.haskell.org/) (major part of web server and feeds fetcher). JavaScript FFI is used for features missing in Ur/Web (or optimizations), [Stylus](https://stylus-lang.com/) for CSS preprocessing and [Webpack](https://webpack.js.org/) to build final JS/CSS assets.

Ur/Web part of sources is located in the root and `lib` folder. Haskell part is in `crawler` folder (historic name, frontend is using Haskell as well).

Code is quite ugly (quick and dirty hacks, experiments, legacy, Russian comments or no comments at all). But it's a working code used everyday by thousands of people. Some parts are left untouched from the crazy 2013 spring when Google decided to close Google Reader and I needed to do many-many things in 3 months. Some parts are newer and are more readable.

Ur/Web <=> Haskell interface
------------

If you like to make mixed Ur/Web+Haskell app look into `lib`, `copy_cc`, `crawler/Gen.hs` and `crawler/bazqux.cabal` (`Executable Reader` part).

`lib` contains Ur/Web implementation of Haskell Binary package (Haskell function arguments and results are serialized using Binary). `copy_cc` is a substitute for the real GNU gcc which is called from `urweb` compiler and copies `webapp.c` file so it will be compiled and linked later in GHC. `crawler/Gen.hs` contains description of data types from which both Ur/Web and Haskell data types and serialization functions are generated. It also generates helper Riak funtions for data types stored in Riak and FFI bindings for Haskell functions called from Ur/Web part. It's not a ready to use library but rather the script you must edit.
