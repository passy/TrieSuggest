TrieSuggest
===========

Programming exercise for [Coding for Interviews](http://codingforinterviews.com/)
implementing a trie-based autocompleter in Haskell. The dictionary is loaded
from `/usr/share/dict/words`. Also, I suck at Haskell, but that was the whole
point of making it.

Licensed under [WTFPL](http://www.wtfpl.net/).

Building
--------

    cabal configure
    cabal build
    dist/build/suggest/suggest
