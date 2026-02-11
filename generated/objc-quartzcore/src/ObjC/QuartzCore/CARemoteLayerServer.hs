{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CARemoteLayerServer@.
module ObjC.QuartzCore.CARemoteLayerServer
  ( CARemoteLayerServer
  , IsCARemoteLayerServer(..)
  , sharedServer
  , serverPort
  , sharedServerSelector
  , serverPortSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedServer@
sharedServer :: IO (Id CARemoteLayerServer)
sharedServer  =
  do
    cls' <- getRequiredClass "CARemoteLayerServer"
    sendClassMsg cls' (mkSelector "sharedServer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serverPort@
serverPort :: IsCARemoteLayerServer caRemoteLayerServer => caRemoteLayerServer -> IO CUInt
serverPort caRemoteLayerServer  =
  sendMsg caRemoteLayerServer (mkSelector "serverPort") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedServer@
sharedServerSelector :: Selector
sharedServerSelector = mkSelector "sharedServer"

-- | @Selector@ for @serverPort@
serverPortSelector :: Selector
serverPortSelector = mkSelector "serverPort"

