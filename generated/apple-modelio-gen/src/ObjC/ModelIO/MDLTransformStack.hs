{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformStack@.
module ObjC.ModelIO.MDLTransformStack
  ( MDLTransformStack
  , IsMDLTransformStack(..)
  , init_
  , animatedValueWithName
  , count
  , keyTimes
  , transformOps
  , initSelector
  , animatedValueWithNameSelector
  , countSelector
  , keyTimesSelector
  , transformOpsSelector

  -- * Enum types
  , MDLTransformOpRotationOrder(MDLTransformOpRotationOrder)
  , pattern MDLTransformOpRotationOrderXYZ
  , pattern MDLTransformOpRotationOrderXZY
  , pattern MDLTransformOpRotationOrderYXZ
  , pattern MDLTransformOpRotationOrderYZX
  , pattern MDLTransformOpRotationOrderZXY
  , pattern MDLTransformOpRotationOrderZYX

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

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO RawId
init_ mdlTransformStack  =
    fmap (RawId . castPtr) $ sendMsg mdlTransformStack (mkSelector "init") (retPtr retVoid) []

-- | @- animatedValueWithName:@
animatedValueWithName :: (IsMDLTransformStack mdlTransformStack, IsNSString name) => mdlTransformStack -> name -> IO (Id MDLAnimatedValue)
animatedValueWithName mdlTransformStack  name =
  withObjCPtr name $ \raw_name ->
      sendMsg mdlTransformStack (mkSelector "animatedValueWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- count@
count :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO CULong
count mdlTransformStack  =
    sendMsg mdlTransformStack (mkSelector "count") retCULong []

-- | @- keyTimes@
keyTimes :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO (Id NSArray)
keyTimes mdlTransformStack  =
    sendMsg mdlTransformStack (mkSelector "keyTimes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transformOps@
transformOps :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO (Id NSArray)
transformOps mdlTransformStack  =
    sendMsg mdlTransformStack (mkSelector "transformOps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @animatedValueWithName:@
animatedValueWithNameSelector :: Selector
animatedValueWithNameSelector = mkSelector "animatedValueWithName:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector
keyTimesSelector = mkSelector "keyTimes"

-- | @Selector@ for @transformOps@
transformOpsSelector :: Selector
transformOpsSelector = mkSelector "transformOps"

