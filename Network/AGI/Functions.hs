module Network.AGI.Functions where

import Data.Maybe

import Network.AGI.Types
import Network.AGI.ServerTypes

-- TODO: implement everything listed there:
--       http://www.voip-info.org/wiki/view/Asterisk%20AGI

answer :: AGI AGIResult
answer =
    blockWithCommand Answer

waitForDigit :: Maybe Int -> AGI AGIResult
waitForDigit timeout =
    blockWithCommand (WaitForDigit (fromMaybe (-1) timeout))

hangup :: OptChannel -> AGI AGIResult
hangup ch =
    blockWithCommand (Hangup ch)
