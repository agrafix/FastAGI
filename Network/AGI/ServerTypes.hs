module Network.AGI.ServerTypes where

import Network.AGI.Types

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

type ServerApp cMsg sMsg = ServerAppIf cMsg sMsg -> ResourceT IO ()

data ServerAppIf cMsg sMsg
    = ServerAppIf
    { recieveAGIResponse :: STM (Maybe cMsg)
    , sendAGICommand    :: sMsg -> STM ()
    , connId           :: String
    }

type AGI a = ReaderT (ServerAppIf AGIResult AGICommand) (ResourceT IO) a

blockWithCommand' :: AGICommand -> AGI ()
blockWithCommand' cmd =
    do _ <- blockWithCommand cmd
       return ()

blockWithCommand :: AGICommand -> AGI AGIResult
blockWithCommand cmd =
    do appIf <- ask
       liftIO $ atomically $ (sendAGICommand appIf) cmd
       response <- liftIO $ atomically $ recieveAGIResponse appIf
       case response of
         Just v -> return v
         Nothing -> fail "Asterisk closed the connection!"
