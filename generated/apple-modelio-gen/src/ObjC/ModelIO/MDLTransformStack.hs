{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , animatedValueWithNameSelector
  , countSelector
  , initSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO RawId
init_ mdlTransformStack =
  sendOwnedMessage mdlTransformStack initSelector

-- | @- animatedValueWithName:@
animatedValueWithName :: (IsMDLTransformStack mdlTransformStack, IsNSString name) => mdlTransformStack -> name -> IO (Id MDLAnimatedValue)
animatedValueWithName mdlTransformStack name =
  sendMessage mdlTransformStack animatedValueWithNameSelector (toNSString name)

-- | @- count@
count :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO CULong
count mdlTransformStack =
  sendMessage mdlTransformStack countSelector

-- | @- keyTimes@
keyTimes :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO (Id NSArray)
keyTimes mdlTransformStack =
  sendMessage mdlTransformStack keyTimesSelector

-- | @- transformOps@
transformOps :: IsMDLTransformStack mdlTransformStack => mdlTransformStack -> IO (Id NSArray)
transformOps mdlTransformStack =
  sendMessage mdlTransformStack transformOpsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @animatedValueWithName:@
animatedValueWithNameSelector :: Selector '[Id NSString] (Id MDLAnimatedValue)
animatedValueWithNameSelector = mkSelector "animatedValueWithName:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @keyTimes@
keyTimesSelector :: Selector '[] (Id NSArray)
keyTimesSelector = mkSelector "keyTimes"

-- | @Selector@ for @transformOps@
transformOpsSelector :: Selector '[] (Id NSArray)
transformOpsSelector = mkSelector "transformOps"

