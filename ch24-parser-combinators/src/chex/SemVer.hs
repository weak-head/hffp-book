----------------------
-- https://semver.org/
----------------------

module SemVer where

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

-- | Defines the semantic version.
--
-- Semanti version has the following structure:
--  1    .    1    .    5   -  y.4.x.7   +  exp.511f2.2fs
--  |         |         |         |               |
-- Major    Minor     Patch    Release         Metadata
--
-- Examples:
--  * 1.2.0
--  * 1.0.1-alpha
--  * 2.7.3-x.7.z.92+sha.258
--
data SemVer =
  SemVer { major :: Major
         , minor :: Minor
         , patch :: Patch
         , release :: Release
         , metadata :: Metadata
         } deriving (Show, Eq)
