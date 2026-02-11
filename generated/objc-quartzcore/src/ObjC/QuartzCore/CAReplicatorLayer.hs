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
  , instanceCountSelector
  , setInstanceCountSelector
  , preservesDepthSelector
  , setPreservesDepthSelector
  , instanceDelaySelector
  , setInstanceDelaySelector
  , instanceTransformSelector
  , setInstanceTransformSelector
  , instanceColorSelector
  , setInstanceColorSelector
  , instanceRedOffsetSelector
  , setInstanceRedOffsetSelector
  , instanceGreenOffsetSelector
  , setInstanceGreenOffsetSelector
  , instanceBlueOffsetSelector
  , setInstanceBlueOffsetSelector
  , instanceAlphaOffsetSelector
  , setInstanceAlphaOffsetSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- instanceCount@
instanceCount :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CLong
instanceCount caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceCount") retCLong []

-- | @- setInstanceCount:@
setInstanceCount :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CLong -> IO ()
setInstanceCount caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceCount:") retVoid [argCLong (fromIntegral value)]

-- | @- preservesDepth@
preservesDepth :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO Bool
preservesDepth caReplicatorLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caReplicatorLayer (mkSelector "preservesDepth") retCULong []

-- | @- setPreservesDepth:@
setPreservesDepth :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> Bool -> IO ()
setPreservesDepth caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setPreservesDepth:") retVoid [argCULong (if value then 1 else 0)]

-- | @- instanceDelay@
instanceDelay :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CDouble
instanceDelay caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceDelay") retCDouble []

-- | @- setInstanceDelay:@
setInstanceDelay :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CDouble -> IO ()
setInstanceDelay caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceDelay:") retVoid [argCDouble (fromIntegral value)]

-- | @- instanceTransform@
instanceTransform :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CATransform3D
instanceTransform caReplicatorLayer  =
  sendMsgStret caReplicatorLayer (mkSelector "instanceTransform") retCATransform3D []

-- | @- setInstanceTransform:@
setInstanceTransform :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CATransform3D -> IO ()
setInstanceTransform caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceTransform:") retVoid [argCATransform3D value]

-- | @- instanceColor@
instanceColor :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO (Ptr ())
instanceColor caReplicatorLayer  =
  fmap castPtr $ sendMsg caReplicatorLayer (mkSelector "instanceColor") (retPtr retVoid) []

-- | @- setInstanceColor:@
setInstanceColor :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> Ptr () -> IO ()
setInstanceColor caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceColor:") retVoid [argPtr value]

-- | @- instanceRedOffset@
instanceRedOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceRedOffset caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceRedOffset") retCFloat []

-- | @- setInstanceRedOffset:@
setInstanceRedOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceRedOffset caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceRedOffset:") retVoid [argCFloat (fromIntegral value)]

-- | @- instanceGreenOffset@
instanceGreenOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceGreenOffset caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceGreenOffset") retCFloat []

-- | @- setInstanceGreenOffset:@
setInstanceGreenOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceGreenOffset caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceGreenOffset:") retVoid [argCFloat (fromIntegral value)]

-- | @- instanceBlueOffset@
instanceBlueOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceBlueOffset caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceBlueOffset") retCFloat []

-- | @- setInstanceBlueOffset:@
setInstanceBlueOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceBlueOffset caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceBlueOffset:") retVoid [argCFloat (fromIntegral value)]

-- | @- instanceAlphaOffset@
instanceAlphaOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> IO CFloat
instanceAlphaOffset caReplicatorLayer  =
  sendMsg caReplicatorLayer (mkSelector "instanceAlphaOffset") retCFloat []

-- | @- setInstanceAlphaOffset:@
setInstanceAlphaOffset :: IsCAReplicatorLayer caReplicatorLayer => caReplicatorLayer -> CFloat -> IO ()
setInstanceAlphaOffset caReplicatorLayer  value =
  sendMsg caReplicatorLayer (mkSelector "setInstanceAlphaOffset:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector
setInstanceCountSelector = mkSelector "setInstanceCount:"

-- | @Selector@ for @preservesDepth@
preservesDepthSelector :: Selector
preservesDepthSelector = mkSelector "preservesDepth"

-- | @Selector@ for @setPreservesDepth:@
setPreservesDepthSelector :: Selector
setPreservesDepthSelector = mkSelector "setPreservesDepth:"

-- | @Selector@ for @instanceDelay@
instanceDelaySelector :: Selector
instanceDelaySelector = mkSelector "instanceDelay"

-- | @Selector@ for @setInstanceDelay:@
setInstanceDelaySelector :: Selector
setInstanceDelaySelector = mkSelector "setInstanceDelay:"

-- | @Selector@ for @instanceTransform@
instanceTransformSelector :: Selector
instanceTransformSelector = mkSelector "instanceTransform"

-- | @Selector@ for @setInstanceTransform:@
setInstanceTransformSelector :: Selector
setInstanceTransformSelector = mkSelector "setInstanceTransform:"

-- | @Selector@ for @instanceColor@
instanceColorSelector :: Selector
instanceColorSelector = mkSelector "instanceColor"

-- | @Selector@ for @setInstanceColor:@
setInstanceColorSelector :: Selector
setInstanceColorSelector = mkSelector "setInstanceColor:"

-- | @Selector@ for @instanceRedOffset@
instanceRedOffsetSelector :: Selector
instanceRedOffsetSelector = mkSelector "instanceRedOffset"

-- | @Selector@ for @setInstanceRedOffset:@
setInstanceRedOffsetSelector :: Selector
setInstanceRedOffsetSelector = mkSelector "setInstanceRedOffset:"

-- | @Selector@ for @instanceGreenOffset@
instanceGreenOffsetSelector :: Selector
instanceGreenOffsetSelector = mkSelector "instanceGreenOffset"

-- | @Selector@ for @setInstanceGreenOffset:@
setInstanceGreenOffsetSelector :: Selector
setInstanceGreenOffsetSelector = mkSelector "setInstanceGreenOffset:"

-- | @Selector@ for @instanceBlueOffset@
instanceBlueOffsetSelector :: Selector
instanceBlueOffsetSelector = mkSelector "instanceBlueOffset"

-- | @Selector@ for @setInstanceBlueOffset:@
setInstanceBlueOffsetSelector :: Selector
setInstanceBlueOffsetSelector = mkSelector "setInstanceBlueOffset:"

-- | @Selector@ for @instanceAlphaOffset@
instanceAlphaOffsetSelector :: Selector
instanceAlphaOffsetSelector = mkSelector "instanceAlphaOffset"

-- | @Selector@ for @setInstanceAlphaOffset:@
setInstanceAlphaOffsetSelector :: Selector
setInstanceAlphaOffsetSelector = mkSelector "setInstanceAlphaOffset:"

