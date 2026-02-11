{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CARemoteLayerClient@.
module ObjC.QuartzCore.CARemoteLayerClient
  ( CARemoteLayerClient
  , IsCARemoteLayerClient(..)
  , initWithServerPort
  , invalidate
  , clientId
  , layer
  , setLayer
  , initWithServerPortSelector
  , invalidateSelector
  , clientIdSelector
  , layerSelector
  , setLayerSelector


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

-- | @- initWithServerPort:@
initWithServerPort :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> CUInt -> IO (Id CARemoteLayerClient)
initWithServerPort caRemoteLayerClient  port =
  sendMsg caRemoteLayerClient (mkSelector "initWithServerPort:") (retPtr retVoid) [argCUInt (fromIntegral port)] >>= ownedObject . castPtr

-- | @- invalidate@
invalidate :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO ()
invalidate caRemoteLayerClient  =
  sendMsg caRemoteLayerClient (mkSelector "invalidate") retVoid []

-- | @- clientId@
clientId :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO CUInt
clientId caRemoteLayerClient  =
  sendMsg caRemoteLayerClient (mkSelector "clientId") retCUInt []

-- | @- layer@
layer :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO (Id CALayer)
layer caRemoteLayerClient  =
  sendMsg caRemoteLayerClient (mkSelector "layer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayer:@
setLayer :: (IsCARemoteLayerClient caRemoteLayerClient, IsCALayer value) => caRemoteLayerClient -> value -> IO ()
setLayer caRemoteLayerClient  value =
withObjCPtr value $ \raw_value ->
    sendMsg caRemoteLayerClient (mkSelector "setLayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServerPort:@
initWithServerPortSelector :: Selector
initWithServerPortSelector = mkSelector "initWithServerPort:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @clientId@
clientIdSelector :: Selector
clientIdSelector = mkSelector "clientId"

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector
setLayerSelector = mkSelector "setLayer:"

