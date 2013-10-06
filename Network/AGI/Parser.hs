{-# LANGUAGE OverloadedStrings #-}
module Network.AGI.Parser
     ( pResponse )
where

import Network.AGI.Types

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

pResponse :: Parser AGIResult
pResponse = pResponse' <* endOfLine

pResponse' :: Parser AGIResult
pResponse' =
    AGIFailure <$ string "200 result=-1" <|>
    AGISuccess <$> (string "200 result=" *> pAsciiDigit) <|>
    AGIStatus <$> (string "200 result=" *> decimal)

pAsciiDigit :: Parser Digit
pAsciiDigit =
    Pound <$ string "35" <|>
    Star <$ string "42" <|>
    Zero <$ string "48" <|>
    One <$ string "49" <|>
    Two <$ string "50" <|>
    Three <$ string "51" <|>
    Four <$ string "52" <|>
    Five <$ string "53" <|>
    Six <$ string "54" <|>
    Seven <$ string "55" <|>
    Eight <$ string "56" <|>
    Nine <$ string "57"
