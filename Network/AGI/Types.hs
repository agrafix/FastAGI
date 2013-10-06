{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Network.AGI.Types where

import qualified Data.ByteString.Char8 as BS

type Channel = BS.ByteString
type OptChannel = Maybe Channel

data AGICommand
   = Answer
   | WaitForDigit Int
   | Hangup OptChannel
   deriving (Show, Eq)

serializeCmd :: AGICommand -> BS.ByteString
serializeCmd Answer = "ANSWER"
serializeCmd (Hangup Nothing) = "HANGUP"
serializeCmd (Hangup (Just ch)) =
    BS.concat ["HANGUP ", ch]
serializeCmd (WaitForDigit timeout) =
    BS.concat ["WAIT FOR DIGIT ", (BS.pack $ show timeout)]

data AGIResult
   = AGIFailure
   | AGIStatus Int
   | AGISuccess Digit
   deriving (Show, Eq)

data Digit
   = Pound
   | Star
   | Zero
   | One
   | Two
   | Three
   | Four
   | Five
   | Six
   | Seven
   | Eight
   | Nine
   deriving (Show, Eq, Ord, Enum)
