module Network.AGI.Functions where

import Network.AGI.Types
import Network.AGI.ServerTypes

answer :: AGI AGIResult
answer =
    blockWithCommand Answer
