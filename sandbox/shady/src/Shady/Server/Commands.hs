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
    Register UserName
  | Login UserName
  | Logout
  | Read From
  | Send To Message
  deriving (Show, Eq)

type From         = String
type To           = String
type Message      = String
type UserName     = String
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
  parseLogout <|>
  parseRead <|>
  parseSend

----------------------------------------

-- | Parses register command.
parseRegister :: Parser Command
parseRegister = do
  string "register" >> space
  userName <- parseUserName
  endCmd
  return $ Register userName

-- | Parses login command.
parseLogin :: Parser Command
parseLogin = do
  string "login" >> space
  userName <- parseUserName
  endCmd
  return $ Login userName

-- | Parses logout command.
parseLogout :: Parser Command
parseLogout =
  string "logout" >>
  skipMany space >>
  endCmd >>
  return Logout

-- | Parses read command.
parseRead :: Parser Command
parseRead = do
  string "read" >> space
  userName <- parseUserName
  endCmd
  return $ Read userName

-- | Parses send command.
parseSend :: Parser Command
parseSend = do
  string "send" >> space
  userName <- parseUserName
  msg <- parseMessage
  endCmd
  return $ Send userName msg

endCmd :: Parser ()
endCmd = eof <|> void newline

-- | Parses user name.
parseUserName :: Parser UserName
parseUserName = do
  skipMany space
  uname <- manyTill anyChar (void newline <|> eof <|> void space)
  skipMany space
  if length uname < 3
    then fail "User name should be at least 3 characters long."
    else return uname

-- | Parsess user message.
parseMessage :: Parser Message
parseMessage = do
  skipMany space
  msg <- manyTill anyChar (void newline <|> eof)
  return msg
