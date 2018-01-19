----------------------
-- https://semver.org/
----------------------

module SemVer where

import Control.Applicative
import Text.Trifecta

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
-- Semantic version has the following structure:
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
  SemVer { major    :: Major
         , minor    :: Minor
         , patch    :: Patch
         , release  :: Release
         , metadata :: Metadata
         } deriving (Show, Eq)

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf ".") >>
  (NOSI <$> integer) <|> (NOSS <$> some letter)

parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  some parseNos

parseMetadata :: Parser Metadata
parseMetadata = do
  _ <- char '+'
  some parseNos

parseSemVer :: Parser SemVer
parseSemVer = do
  mj <- integer
  _ <- char '.'
  mn <- integer
  _ <- char '.'
  pt <- integer
  try (liftA2 (SemVer mj mn pt) (try parseRelease) (try parseMetadata))
    <|> try (liftA2 (SemVer mj mn pt) (try parseRelease) (pure []))
    <|> try (SemVer mj mn pt [] <$> (try parseMetadata))
    <|> (return $ SemVer mj mn pt [] [])



main :: IO ()
main = do
  let parse = parseString parseSemVer mempty
  print $ parse "2.1.1"
  print $ parse "1.2.3-x.7.z.92"
  print $ parse "1.7.4+20130313144700"
  print $ parse "1.4.7+exp.sha.5114f85"
  print $ parse "0.9.7-beta+exp.sha.5114f85"
  print $ parse "3.1.7-x.17.92+e2"

  -- SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
