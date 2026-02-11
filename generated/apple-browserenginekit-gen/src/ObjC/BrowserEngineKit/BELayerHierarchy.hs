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
  , initSelector
  , newSelector
  , layerHierarchyWithErrorSelector
  , invalidateSelector
  , handleSelector
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

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO (Id BELayerHierarchy)
init_ beLayerHierarchy  =
    sendMsg beLayerHierarchy (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BELayerHierarchy)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | may fail if a connection to the render server cannot be established
--
-- ObjC selector: @+ layerHierarchyWithError:@
layerHierarchyWithError :: IsNSError error_ => error_ -> IO (Id BELayerHierarchy)
layerHierarchyWithError error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchy"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "layerHierarchyWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | invalidate must be called before this layer hierarchy is disposed of
--
-- ObjC selector: @- invalidate@
invalidate :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO ()
invalidate beLayerHierarchy  =
    sendMsg beLayerHierarchy (mkSelector "invalidate") retVoid []

-- | a reference to this @BELayerHierarchy@ for use with @BELayerHierarchyHostingView@
--
-- ObjC selector: @- handle@
handle :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO (Id BELayerHierarchyHandle)
handle beLayerHierarchy  =
    sendMsg beLayerHierarchy (mkSelector "handle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | the root layer of this hierarchy
--
-- ObjC selector: @- layer@
layer :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> IO RawId
layer beLayerHierarchy  =
    fmap (RawId . castPtr) $ sendMsg beLayerHierarchy (mkSelector "layer") (retPtr retVoid) []

-- | the root layer of this hierarchy
--
-- ObjC selector: @- setLayer:@
setLayer :: IsBELayerHierarchy beLayerHierarchy => beLayerHierarchy -> RawId -> IO ()
setLayer beLayerHierarchy  value =
    sendMsg beLayerHierarchy (mkSelector "setLayer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @layerHierarchyWithError:@
layerHierarchyWithErrorSelector :: Selector
layerHierarchyWithErrorSelector = mkSelector "layerHierarchyWithError:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @handle@
handleSelector :: Selector
handleSelector = mkSelector "handle"

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector
setLayerSelector = mkSelector "setLayer:"

