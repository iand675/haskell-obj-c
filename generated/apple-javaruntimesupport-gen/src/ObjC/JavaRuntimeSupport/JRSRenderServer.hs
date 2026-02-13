{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSRenderServer@.
module ObjC.JavaRuntimeSupport.JRSRenderServer
  ( JRSRenderServer
  , IsJRSRenderServer(..)
  , startRenderServer
  , sendRenderServer
  , recieveRenderServer
  , recieveRenderServerSelector
  , sendRenderServerSelector
  , startRenderServerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ startRenderServer@
startRenderServer :: IO CUInt
startRenderServer  =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    sendClassMessage cls' startRenderServerSelector

-- | @+ sendRenderServer:@
sendRenderServer :: CUInt -> IO (Id NSString)
sendRenderServer serverPort =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    sendClassMessage cls' sendRenderServerSelector serverPort

-- | @+ recieveRenderServer:@
recieveRenderServer :: IsNSString serverName => serverName -> IO CUInt
recieveRenderServer serverName =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    sendClassMessage cls' recieveRenderServerSelector (toNSString serverName)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startRenderServer@
startRenderServerSelector :: Selector '[] CUInt
startRenderServerSelector = mkSelector "startRenderServer"

-- | @Selector@ for @sendRenderServer:@
sendRenderServerSelector :: Selector '[CUInt] (Id NSString)
sendRenderServerSelector = mkSelector "sendRenderServer:"

-- | @Selector@ for @recieveRenderServer:@
recieveRenderServerSelector :: Selector '[Id NSString] CUInt
recieveRenderServerSelector = mkSelector "recieveRenderServer:"

