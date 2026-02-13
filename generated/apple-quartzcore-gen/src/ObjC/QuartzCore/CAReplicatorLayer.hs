{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAReplicatorLayer@.
module ObjC.QuartzCore.CAReplicatorLayer
  ( CAReplicatorLayer
  , IsCAReplicatorLayer(..)
  , instanceCount
  , setInstanceCount
  , preservesDepth
  , setPreservesDepth
  , instanceDelay
  , setInstanceDelay
  , instanceTransform
  , setInstanceTransform
  , instanceColor
  , setInstanceColor
  , instanceRedOffset
  , setInstanceRedOffset
  , instanceGreenOffset
  , setInstanceGreenOffset
  , instanceBlueOffset
  , setInstanceBlueOffset
  , instanceAlphaOffset
  , setInstanceAlphaOffset
  , instanceAlphaOffsetSelector
  , instanceBlueOffsetSelector
  , instanceColorSelector
  , instanceCountSelector
  , instanceDelaySelector
  , instanceGreenOffsetSelector
  , instanceRedOffsetSelector
  , instanceTransformSelector
  , preservesDepthSelector
  , setInstanceAlphaOffsetSelector
  , setInstanceBlueOffsetSelector
  , setInstanceColorSelector
  , setInstanceCountSelector
  , setInstanceDelaySelector
  , setInstanceGreenOffsetSelector
  , setInstanceRedOffsetSelector
  , setInstanceTransformSelector
  , setPreservesDepthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- instanceCount@
instanceCount :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CLong
instanceCount caReplicatorLayer =
  sendMessage caReplicatorLayer instanceCountSelector

-- | @- setInstanceCount:@
setInstanceCount :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CLong -> IO ()
setInstanceCount caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceCountSelector value

-- | @- preservesDepth@
preservesDepth :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO Bool
preservesDepth caReplicatorLayer =
  sendMessage caReplicatorLayer preservesDepthSelector

-- | @- setPreservesDepth:@
setPreservesDepth :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> Bool -> IO ()
setPreservesDepth caReplicatorLayer value =
  sendMessage caReplicatorLayer setPreservesDepthSelector value

-- | @- instanceDelay@
instanceDelay :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CDouble
instanceDelay caReplicatorLayer =
  sendMessage caReplicatorLayer instanceDelaySelector

-- | @- setInstanceDelay:@
setInstanceDelay :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CDouble -> IO ()
setInstanceDelay caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceDelaySelector value

-- | @- instanceTransform@
instanceTransform :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CATransform3D
instanceTransform caReplicatorLayer =
  sendMessage caReplicatorLayer instanceTransformSelector

-- | @- setInstanceTransform:@
setInstanceTransform :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CATransform3D -> IO ()
setInstanceTransform caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceTransformSelector value

-- | @- instanceColor@
instanceColor :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO (Ptr ())
instanceColor caReplicatorLayer =
  sendMessage caReplicatorLayer instanceColorSelector

-- | @- setInstanceColor:@
setInstanceColor :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> Ptr () -> IO ()
setInstanceColor caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceColorSelector value

-- | @- instanceRedOffset@
instanceRedOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceRedOffset caReplicatorLayer =
  sendMessage caReplicatorLayer instanceRedOffsetSelector

-- | @- setInstanceRedOffset:@
setInstanceRedOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceRedOffset caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceRedOffsetSelector value

-- | @- instanceGreenOffset@
instanceGreenOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceGreenOffset caReplicatorLayer =
  sendMessage caReplicatorLayer instanceGreenOffsetSelector

-- | @- setInstanceGreenOffset:@
setInstanceGreenOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceGreenOffset caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceGreenOffsetSelector value

-- | @- instanceBlueOffset@
instanceBlueOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceBlueOffset caReplicatorLayer =
  sendMessage caReplicatorLayer instanceBlueOffsetSelector

-- | @- setInstanceBlueOffset:@
setInstanceBlueOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceBlueOffset caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceBlueOffsetSelector value

-- | @- instanceAlphaOffset@
instanceAlphaOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceAlphaOffset caReplicatorLayer =
  sendMessage caReplicatorLayer instanceAlphaOffsetSelector

-- | @- setInstanceAlphaOffset:@
setInstanceAlphaOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceAlphaOffset caReplicatorLayer value =
  sendMessage caReplicatorLayer setInstanceAlphaOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector '[] CLong
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector '[CLong] ()
setInstanceCountSelector = mkSelector "setInstanceCount:"

-- | @Selector@ for @preservesDepth@
preservesDepthSelector :: Selector '[] Bool
preservesDepthSelector = mkSelector "preservesDepth"

-- | @Selector@ for @setPreservesDepth:@
setPreservesDepthSelector :: Selector '[Bool] ()
setPreservesDepthSelector = mkSelector "setPreservesDepth:"

-- | @Selector@ for @instanceDelay@
instanceDelaySelector :: Selector '[] CDouble
instanceDelaySelector = mkSelector "instanceDelay"

-- | @Selector@ for @setInstanceDelay:@
setInstanceDelaySelector :: Selector '[CDouble] ()
setInstanceDelaySelector = mkSelector "setInstanceDelay:"

-- | @Selector@ for @instanceTransform@
instanceTransformSelector :: Selector '[] CATransform3D
instanceTransformSelector = mkSelector "instanceTransform"

-- | @Selector@ for @setInstanceTransform:@
setInstanceTransformSelector :: Selector '[CATransform3D] ()
setInstanceTransformSelector = mkSelector "setInstanceTransform:"

-- | @Selector@ for @instanceColor@
instanceColorSelector :: Selector '[] (Ptr ())
instanceColorSelector = mkSelector "instanceColor"

-- | @Selector@ for @setInstanceColor:@
setInstanceColorSelector :: Selector '[Ptr ()] ()
setInstanceColorSelector = mkSelector "setInstanceColor:"

-- | @Selector@ for @instanceRedOffset@
instanceRedOffsetSelector :: Selector '[] CFloat
instanceRedOffsetSelector = mkSelector "instanceRedOffset"

-- | @Selector@ for @setInstanceRedOffset:@
setInstanceRedOffsetSelector :: Selector '[CFloat] ()
setInstanceRedOffsetSelector = mkSelector "setInstanceRedOffset:"

-- | @Selector@ for @instanceGreenOffset@
instanceGreenOffsetSelector :: Selector '[] CFloat
instanceGreenOffsetSelector = mkSelector "instanceGreenOffset"

-- | @Selector@ for @setInstanceGreenOffset:@
setInstanceGreenOffsetSelector :: Selector '[CFloat] ()
setInstanceGreenOffsetSelector = mkSelector "setInstanceGreenOffset:"

-- | @Selector@ for @instanceBlueOffset@
instanceBlueOffsetSelector :: Selector '[] CFloat
instanceBlueOffsetSelector = mkSelector "instanceBlueOffset"

-- | @Selector@ for @setInstanceBlueOffset:@
setInstanceBlueOffsetSelector :: Selector '[CFloat] ()
setInstanceBlueOffsetSelector = mkSelector "setInstanceBlueOffset:"

-- | @Selector@ for @instanceAlphaOffset@
instanceAlphaOffsetSelector :: Selector '[] CFloat
instanceAlphaOffsetSelector = mkSelector "instanceAlphaOffset"

-- | @Selector@ for @setInstanceAlphaOffset:@
setInstanceAlphaOffsetSelector :: Selector '[CFloat] ()
setInstanceAlphaOffsetSelector = mkSelector "setInstanceAlphaOffset:"

