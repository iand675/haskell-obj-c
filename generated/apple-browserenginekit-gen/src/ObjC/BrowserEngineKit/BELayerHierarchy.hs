{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BELayerHierarchy@.
module ObjC.BrowserEngineKit.BELayerHierarchy
  ( BELayerHierarchy
  , IsBELayerHierarchy(..)
  , init_
  , new
  , layerHierarchyWithError
  , invalidate
  , handle
  , layer
  , setLayer
  , handleSelector
  , initSelector
  , invalidateSelector
  , layerHierarchyWithErrorSelector
  , layerSelector
  , newSelector
  , setLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO (Id BELayerHierarchy)
init_ beLayerHierarchy =
  sendOwnedMessage beLayerHierarchy initSelector

-- | @+ new@
new :: IO (Id BELayerHierarchy)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    sendOwnedClassMessage cls' newSelector

-- | may fail if a connection to the render server cannot be established
--
-- ObjC selector: @+ layerHierarchyWithError:@
layerHierarchyWithError :: IsNSError error_ => error_ -> IO (Id BELayerHierarchy)
layerHierarchyWithError error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    sendClassMessage cls' layerHierarchyWithErrorSelector (toNSError error_)

-- | invalidate must be called before this layer hierarchy is disposed of
--
-- ObjC selector: @- invalidate@
invalidate :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO ()
invalidate beLayerHierarchy =
  sendMessage beLayerHierarchy invalidateSelector

-- | a reference to this @BELayerHierarchy@ for use with @BELayerHierarchyHostingView@
--
-- ObjC selector: @- handle@
handle :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO (Id BELayerHierarchyHandle)
handle beLayerHierarchy =
  sendMessage beLayerHierarchy handleSelector

-- | the root layer of this hierarchy
--
-- ObjC selector: @- layer@
layer :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO RawId
layer beLayerHierarchy =
  sendMessage beLayerHierarchy layerSelector

-- | the root layer of this hierarchy
--
-- ObjC selector: @- setLayer:@
setLayer :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> RawId -> IO ()
setLayer beLayerHierarchy value =
  sendMessage beLayerHierarchy setLayerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BELayerHierarchy)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BELayerHierarchy)
newSelector = mkSelector "new"

-- | @Selector@ for @layerHierarchyWithError:@
layerHierarchyWithErrorSelector :: Selector '[Id NSError] (Id BELayerHierarchy)
layerHierarchyWithErrorSelector = mkSelector "layerHierarchyWithError:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @handle@
handleSelector :: Selector '[] (Id BELayerHierarchyHandle)
handleSelector = mkSelector "handle"

-- | @Selector@ for @layer@
layerSelector :: Selector '[] RawId
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector '[RawId] ()
setLayerSelector = mkSelector "setLayer:"

