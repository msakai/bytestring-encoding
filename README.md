# bytestring-encoding: ByteString ↔ Text converter based on GHC.IO.Encoding

[![Build Status (Travis CI)](https://travis-ci.org/msakai/bytestring-encoding.svg?branch=master)](https://travis-ci.org/msakai/bytestring-encoding)
[![Build status (AppVeyor)](https://ci.appveyor.com/api/projects/status/8pwtxsky05ge0ooc/branch/master?svg=true)](https://ci.appveyor.com/project/msakai/bytestring-encoding/branch/master)
[![Build Status (GitHub Actions)](https://github.com/msakai/bytestring-encoding/workflows/build/badge.svg)](https://github.com/msakai/bytestring-encoding/actions)
[![Coverage Status](https://coveralls.io/repos/github/msakai/bytestring-encoding/badge.svg?branch=master)](https://coveralls.io/github/msakai/bytestring-encoding?branch=master)
[![Hackage](https://img.shields.io/hackage/v/bytestring-encoding.svg)](https://hackage.haskell.org/package/bytestring-encoding)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/bytestring-encoding.svg)](https://packdeps.haskellers.com/feed?needle=bytestring-encoding)
[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

These library provides converter between `ByteString` and `Text` based
on `GHC.IO.Encoding`.
Compared to the [text-icu](http://hackage.haskell.org/package/text-icu)
package, it has only limited feature and is platform dependent, but is
light-weight and consistent with conversion by `System.IO`.

## Limitations and Known issues

* There are some cases that conversion can produce incomplete results due to the problem of `GHC.IO.Encoding` API.
  see https://gitlab.haskell.org/ghc/ghc/-/issues/15553 for details.
  
