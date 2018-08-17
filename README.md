# bytestring-encoding: ByteString ↔ Text converter based on GHC.IO.Encoding

[![Build Status (Travis CI)](https://travis-ci.org/msakai/bytestring-encoding.svg?branch=master)](https://travis-ci.org/msakai/bytestring-encoding)
[![Build status (AppVeyor)](https://ci.appveyor.com/api/projects/status/8pwtxsky05ge0ooc/branch/master?svg=true)](https://ci.appveyor.com/project/msakai/bytestring-encoding/branch/master)

These library provides converter between `ByteString` and `Text` based
on `GHC.IO.Encoding`.
Compared to the [text-icu](http://hackage.haskell.org/package/text-icu)
package, it has only limited feature and platform dependent, but it is
light-weight and consistent with conversion by `System.IO`.
