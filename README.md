# hffp-book

This repository contains source code, exercises, related topics and some additional material that I've got (directly or indirectly) from the amazing [Haskell programming from first principles](http://haskellbook.com/) book.

## Places worth to check

There is a lot of random unstructured stuff here, but there are a few good places to check:

The base library definitions and implementations:
* [Monoid, Functor, Applicative, Monad, Foldable and Traversable class definitions](ch21-traversable/src/Sand/ClassDef.hs) 
* [Identity](ch21-traversable/src/Sand/Identity.hs)
* [Constant](ch21-traversable/src/Sand/Constant.hs)
* [Maybe](ch21-traversable/src/Sand/Maybe.hs)
* [Either](ch21-traversable/src/Sand/Either.hs)
* [Tuple](ch21-traversable/src/Sand/Tuple.hs)
* [List](ch21-traversable/src/Sand/List.hs)
* [Tree](ch21-traversable/src/Sand/Tree.hs)
* [Function](ch21-traversable/src/Sand/Func.hs)

Reader and State:
 * [Visualizing Reader](ch22-reader/src/Mread.hs)
 * [Visualizing State (+quick batch)](ch23-state/src/Moi.hs)

Parsers:
* [Semantic version parser](ch24-parser-combinators/src/chex/SemVer.hs)
* [Activity log file parser](ch24-parser-combinators/src/chex/LogFile.hs)
* [IPv4/IPv6 address parser](ch24-parser-combinators/src/chex/IpParse.hs)

Monad transformers:
* [MaybeT, ExceptT, ReaderT, StateT instances](ch26-monad-transformers/src/sand/Inst.hs)
* [Scotty ActionT breakdown](ch26-monad-transformers/src/Scotty.hs)
* [HitCounter (scotty, ReaderT, IORef)](ch26-monad-transformers/src/HitCounter.hs)
* [Morra game](ch26-monad-transformers/src/Morra.hs)

Exceptions:
* [Exception hierarchy](ch30-when-things-go-wrong/src/Hierarchy.hs)

Others:
* [Very simple TCP based server that uses SQLite and RWST monad](sandbox/shady)
