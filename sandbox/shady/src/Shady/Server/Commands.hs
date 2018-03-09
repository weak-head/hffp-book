module Shady.Server.Commands
  (
    Command (..)
  , parseCommand
  , parseCommandStr
  )
where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.ByteString hiding (length)
import qualified Data.ByteString.Char8 as BSC
import           Text.Trifecta

-- | Defines all known and supported commands.
data Command =
    Register String
  | Login String
  | Logout
  deriving (Show, Eq)

type UserName = String
type ParsingError = String

-- | Parses the command string.
parseCommandStr :: String -> Either ParsingError Command
parseCommandStr = parseCommand . BSC.pack

-- | Parses the command.
parseCommand :: ByteString -> Either ParsingError Command
parseCommand str =
  case parseByteString parseCommand' mempty str of
    Success a -> Right a
    _         -> Left "Failed to parse command"

-- | Command parser.
parseCommand' :: Parser Command
parseCommand' =
  parseRegister <|>
  parseLogin <|>
  parseLogout

----------------------------------------

-- | Parses register command.
parseRegister :: Parser Command
parseRegister = do
  string "register"
  userName <- parseUserName
  eof
  return $ Register userName

-- | Parses login command.
parseLogin :: Parser Command
parseLogin = do
  string "login"
  userName <- parseUserName
  eof
  return $ Login userName

-- | Parses logout command.
parseLogout :: Parser Command
parseLogout =
  string "logout" >>
  skipMany space >> eof >>
  return Logout

-- | Parses user name.
parseUserName :: Parser UserName
parseUserName = do
  skipMany space
  uname <- manyTill anyChar (void newline <|> eof <|> void space)
  skipMany space
  if length uname < 3
    then fail "User name should be at least 3 characters long."
    else return uname
