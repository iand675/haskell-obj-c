{-# LANGUAGE DataKinds #-}
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
  , clientIdSelector
  , initWithServerPortSelector
  , invalidateSelector
  , layerSelector
  , setLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithServerPort:@
initWithServerPort :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> CUInt -> IO (Id CARemoteLayerClient)
initWithServerPort caRemoteLayerClient port =
  sendOwnedMessage caRemoteLayerClient initWithServerPortSelector port

-- | @- invalidate@
invalidate :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO ()
invalidate caRemoteLayerClient =
  sendMessage caRemoteLayerClient invalidateSelector

-- | @- clientId@
clientId :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO CUInt
clientId caRemoteLayerClient =
  sendMessage caRemoteLayerClient clientIdSelector

-- | @- layer@
layer :: IsCARemoteLayerClient caRemoteLayerClient => caRemoteLayerClient -> IO (Id CALayer)
layer caRemoteLayerClient =
  sendMessage caRemoteLayerClient layerSelector

-- | @- setLayer:@
setLayer :: (IsCARemoteLayerClient caRemoteLayerClient, IsCALayer value) => caRemoteLayerClient -> value -> IO ()
setLayer caRemoteLayerClient value =
  sendMessage caRemoteLayerClient setLayerSelector (toCALayer value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServerPort:@
initWithServerPortSelector :: Selector '[CUInt] (Id CARemoteLayerClient)
initWithServerPortSelector = mkSelector "initWithServerPort:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @clientId@
clientIdSelector :: Selector '[] CUInt
clientIdSelector = mkSelector "clientId"

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id CALayer)
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector '[Id CALayer] ()
setLayerSelector = mkSelector "setLayer:"

