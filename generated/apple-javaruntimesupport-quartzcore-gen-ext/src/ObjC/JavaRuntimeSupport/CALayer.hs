{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The base layer class. *
--
-- Generated bindings for @CALayer@.
module ObjC.JavaRuntimeSupport.CALayer
  ( CALayer
  , IsCALayer(..)
  , createRemoteLayerBoundTo
  , hostRemoteLayer
  , createRemoteLayerBoundToSelector
  , hostRemoteLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | @- createRemoteLayerBoundTo:@
createRemoteLayerBoundTo :: IsCALayer caLayer => caLayer -> CUInt -> IO (Id NSObject)
createRemoteLayerBoundTo caLayer serverPort =
  sendMessage caLayer createRemoteLayerBoundToSelector serverPort

-- | @- hostRemoteLayer:@
hostRemoteLayer :: IsCALayer caLayer => caLayer -> CUInt -> IO ()
hostRemoteLayer caLayer layerID =
  sendMessage caLayer hostRemoteLayerSelector layerID

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createRemoteLayerBoundTo:@
createRemoteLayerBoundToSelector :: Selector '[CUInt] (Id NSObject)
createRemoteLayerBoundToSelector = mkSelector "createRemoteLayerBoundTo:"

-- | @Selector@ for @hostRemoteLayer:@
hostRemoteLayerSelector :: Selector '[CUInt] ()
hostRemoteLayerSelector = mkSelector "hostRemoteLayer:"

