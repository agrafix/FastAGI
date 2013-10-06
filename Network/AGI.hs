{-# LANGUAGE DeriveGeneric, RecordWildCards, RankNTypes, DoAndIfThenElse, OverloadedStrings #-}
module Network.AGI
    ( startServer
    , ServerAppIf(..)
    , ServerApp
    )
where

import Network.AGI.Types
import Network.AGI.Parser

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.TMChan

import GHC.Generics
import Data.Conduit.Attoparsec (conduitParser, PositionRange(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Monad.Trans.Resource as RT
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Control.Concurrent hiding (yield)
import Network.Socket.Internal (SockAddr(..))
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.HashMap.Strict as HM

type ServerApp cMsg sMsg = ServerAppIf cMsg sMsg -> ResourceT IO ()

data ServerAppIf cMsg sMsg
    = ServerAppIf
    { recieveAGIResponse :: STM (Maybe cMsg)
    , sendAGICommand    :: sMsg -> STM ()
    , connId           :: String
    }

startServer :: ServerApp AGIResult AGICommand -> IO ()
startServer serverApp =
    do putStrLn "Launching AGI Server on port 4573"
       runTCPServer (serverSettings 4573 HostAny) (app serverApp)

chanSize = 15

app :: ServerApp AGIResult AGICommand -> Application IO
app runServerApp app =
    main
    where
      sid = sidFromSockAddr $ appSockAddr app
      main =
          runResourceT $
          do let mkChan name size = RT.allocate (atomically (newTBMChan size)) (closeTbm name)
                 closeTbm :: String -> TBMChan a -> IO ()
                 closeTbm n chan =
                     atomically (closeTBMChan chan)
                 mkThread :: String -> ResourceT IO () -> ResourceT IO ()
                 mkThread name action =
                     do tid <- liftIO $ forkIO (runResourceT action)
                        _ <- RT.register (killThread tid)
                        return ()

             (_rk, recvChan) <- mkChan "recv" chanSize
             (_rk', sendChan) <- mkChan "send" chanSize
             mkThread "responseThread" (source $$ ((parseResponse =$ sinkTBMChan recvChan)))
             mkThread "commandThread" ((sourceTBMChan sendChan $= serializeCommand) $$ sink)
             let readAction = fmap snd <$> readTBMChan recvChan
                 writeAction = writeTBMChan sendChan
                 serverAppIf = ServerAppIf readAction writeAction sid
             runServerApp serverAppIf
      s = fromMaybe "" (fmap ((++"_") . show) (appLocalAddr app)) ++ "to_" ++ show (appSockAddr app)
      sink = transPipe lift (appSink app)
      source = transPipe lift (appSource app)
      p s = sid ++ ": " ++ s

sidFromSockAddr addr =
    case addr of
      SockAddrInet port _host -> show port
      SockAddrInet6 port _flow _host _scope -> show port
      SockAddrUnix s -> s

parseResponse :: Conduit BS.ByteString (ResourceT IO) (PositionRange, AGIResult)
parseResponse =
    conduitParser pResponse

serializeCommand :: Conduit AGICommand (ResourceT IO) BS.ByteString
serializeCommand =
    awaitForever (yield . addNewLine . serializeCmd)
    where
      addNewLine bs =
          BS.concat [bs, "\n"]
