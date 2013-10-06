{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Network.AGI.Types where

import qualified Data.ByteString as BS

type Channel = BS.ByteString
type OptChannel = Maybe Channel

data AGICommand
   = Answer
   deriving (Show, Eq)

serializeCmd :: AGICommand -> BS.ByteString
serializeCmd Answer = "ANSWER"

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
