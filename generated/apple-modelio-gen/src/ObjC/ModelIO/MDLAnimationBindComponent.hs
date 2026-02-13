{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLAnimationBindComponent@.
module ObjC.ModelIO.MDLAnimationBindComponent
  ( MDLAnimationBindComponent
  , IsMDLAnimationBindComponent(..)
  , skeleton
  , setSkeleton
  , jointAnimation
  , setJointAnimation
  , jointPaths
  , setJointPaths
  , jointAnimationSelector
  , jointPathsSelector
  , setJointAnimationSelector
  , setJointPathsSelector
  , setSkeletonSelector
  , skeletonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- skeleton@
skeleton :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO (Id MDLSkeleton)
skeleton mdlAnimationBindComponent =
  sendMessage mdlAnimationBindComponent skeletonSelector

-- | @- setSkeleton:@
setSkeleton :: (IsMDLAnimationBindComponent mdlAnimationBindComponent, IsMDLSkeleton value) => mdlAnimationBindComponent -> value -> IO ()
setSkeleton mdlAnimationBindComponent value =
  sendMessage mdlAnimationBindComponent setSkeletonSelector (toMDLSkeleton value)

-- | @- jointAnimation@
jointAnimation :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO RawId
jointAnimation mdlAnimationBindComponent =
  sendMessage mdlAnimationBindComponent jointAnimationSelector

-- | @- setJointAnimation:@
setJointAnimation :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> RawId -> IO ()
setJointAnimation mdlAnimationBindComponent value =
  sendMessage mdlAnimationBindComponent setJointAnimationSelector value

-- | @- jointPaths@
jointPaths :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO (Id NSArray)
jointPaths mdlAnimationBindComponent =
  sendMessage mdlAnimationBindComponent jointPathsSelector

-- | @- setJointPaths:@
setJointPaths :: (IsMDLAnimationBindComponent mdlAnimationBindComponent, IsNSArray value) => mdlAnimationBindComponent -> value -> IO ()
setJointPaths mdlAnimationBindComponent value =
  sendMessage mdlAnimationBindComponent setJointPathsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skeleton@
skeletonSelector :: Selector '[] (Id MDLSkeleton)
skeletonSelector = mkSelector "skeleton"

-- | @Selector@ for @setSkeleton:@
setSkeletonSelector :: Selector '[Id MDLSkeleton] ()
setSkeletonSelector = mkSelector "setSkeleton:"

-- | @Selector@ for @jointAnimation@
jointAnimationSelector :: Selector '[] RawId
jointAnimationSelector = mkSelector "jointAnimation"

-- | @Selector@ for @setJointAnimation:@
setJointAnimationSelector :: Selector '[RawId] ()
setJointAnimationSelector = mkSelector "setJointAnimation:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector '[] (Id NSArray)
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @setJointPaths:@
setJointPathsSelector :: Selector '[Id NSArray] ()
setJointPathsSelector = mkSelector "setJointPaths:"

