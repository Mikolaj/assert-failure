assert-failure [![Build Status](https://secure.travis-ci.org/Mikolaj/assert-failure.png)](http://travis-ci.org/Mikolaj/assert-failure)[![Build Status](https://drone.io/github.com/Mikolaj/assert-failure/status.png)](https://drone.io/github.com/Mikolaj/assert-failure/latest)
==============

This library contains syntactic sugar that improves
the usability of 'assert' and 'error'.
This is actually a bunch of hacks wrapping the original 'assert' function,
which is, as of GHC 7.8, the only simple way of obtaining source positions.
The original 'assert' function is here re-exported for convenience.

Make sure to enable assertions for your cabal package, e.g., by setting

    ghc-options: -fno-ignore-asserts

in your .cabal file. Otherwise, some of the functions will have
no effect at all.

The library is available from [Hackage] [1] and it's homepage
and issue tracker is on [github] [2]. The library emerged from the tons
of assertions (augmented by comments and printouts) and 'error' calls
(marked by unique strings to overcome their lack of source position)
in the [LambdaHack] [3] game engine.

[1]: http://hackage.haskell.org/package/assert-failure
[2]: https://github.com/Mikolaj/assert-failure
[3]: http://hackage.haskell.org/package/LambdaHack
