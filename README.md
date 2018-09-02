# bytestring-encoding: ByteString ↔ Text converter based on GHC.IO.Encoding

[![Build Status (Travis CI)](https://travis-ci.org/msakai/bytestring-encoding.svg?branch=master)](https://travis-ci.org/msakai/bytestring-encoding)
[![Build status (AppVeyor)](https://ci.appveyor.com/api/projects/status/8pwtxsky05ge0ooc/branch/master?svg=true)](https://ci.appveyor.com/project/msakai/bytestring-encoding/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/msakai/bytestring-encoding/badge.svg?branch=master)](https://coveralls.io/github/msakai/bytestring-encoding?branch=master)

These library provides converter between `ByteString` and `Text` based
on `GHC.IO.Encoding`.
Compared to the [text-icu](http://hackage.haskell.org/package/text-icu)
package, it has only limited feature and platform dependent, but it is
light-weight and consistent with conversion by `System.IO`.

## Limitations and Known issues

* There are some cases that conversion can produce incomplete results due to the problem of `GHC.IO.Encoding` API.
  see https://ghc.haskell.org/trac/ghc/ticket/15553 for details.
  
