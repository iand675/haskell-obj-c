{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CARemoteLayerServer@.
module ObjC.QuartzCore.CARemoteLayerServer
  ( CARemoteLayerServer
  , IsCARemoteLayerServer(..)
  , sharedServer
  , serverPort
  , serverPortSelector
  , sharedServerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedServer@
sharedServer :: IO (Id CARemoteLayerServer)
sharedServer  =
  do
    cls' <- getRequiredClass "CARemoteLayerServer"
    sendClassMessage cls' sharedServerSelector

-- | @- serverPort@
serverPort :: IsCARemoteLayerServer caRemoteLayerServer => caRemoteLayerServer -> IO CUInt
serverPort caRemoteLayerServer =
  sendMessage caRemoteLayerServer serverPortSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedServer@
sharedServerSelector :: Selector '[] (Id CARemoteLayerServer)
sharedServerSelector = mkSelector "sharedServer"

-- | @Selector@ for @serverPort@
serverPortSelector :: Selector '[] CUInt
serverPortSelector = mkSelector "serverPort"

