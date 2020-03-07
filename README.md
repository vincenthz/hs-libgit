haskell libgit
==============

[![Build Status](https://travis-ci.org/vincenthz/hs-libgit.png?branch=master)](https://travis-ci.org/vincenthz/hs-libgit)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

hs-libgit is a haskell wrapper for git.

It provides lowlevel operations (list/cat object, ..) and some high level operation
(commit, checkout, diff...). it requires the git binary available on the system.

Example usage, in ghci

```
Prelude>:m +Lib.Git
Prelude Lib.Git> :m +System.Directory
Prelude Lib.Git System.Directory> createDirectoryIfMissing True "/tmp/repodir"
Prelude Lib.Git System.Directory> let cfg = makeConfig "/tmp/repodir" Nothing
Prelude Lib.Git System.Directory> runGit cfg (initDB False)
```

You will now have an initialised git repo in /tmp/repodir. 

TODO
----

- clean the commit/tree parsing
- make it more robust/better error checking
- split modules into multiples files (lowlevel, monad, highlevel)
