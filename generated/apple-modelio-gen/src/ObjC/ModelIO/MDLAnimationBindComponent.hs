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
  , skeletonSelector
  , setSkeletonSelector
  , jointAnimationSelector
  , setJointAnimationSelector
  , jointPathsSelector
  , setJointPathsSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- skeleton@
skeleton :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO (Id MDLSkeleton)
skeleton mdlAnimationBindComponent  =
    sendMsg mdlAnimationBindComponent (mkSelector "skeleton") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSkeleton:@
setSkeleton :: (IsMDLAnimationBindComponent mdlAnimationBindComponent, IsMDLSkeleton value) => mdlAnimationBindComponent -> value -> IO ()
setSkeleton mdlAnimationBindComponent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlAnimationBindComponent (mkSelector "setSkeleton:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- jointAnimation@
jointAnimation :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO RawId
jointAnimation mdlAnimationBindComponent  =
    fmap (RawId . castPtr) $ sendMsg mdlAnimationBindComponent (mkSelector "jointAnimation") (retPtr retVoid) []

-- | @- setJointAnimation:@
setJointAnimation :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> RawId -> IO ()
setJointAnimation mdlAnimationBindComponent  value =
    sendMsg mdlAnimationBindComponent (mkSelector "setJointAnimation:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- jointPaths@
jointPaths :: IsMDLAnimationBindComponent mdlAnimationBindComponent => mdlAnimationBindComponent -> IO (Id NSArray)
jointPaths mdlAnimationBindComponent  =
    sendMsg mdlAnimationBindComponent (mkSelector "jointPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJointPaths:@
setJointPaths :: (IsMDLAnimationBindComponent mdlAnimationBindComponent, IsNSArray value) => mdlAnimationBindComponent -> value -> IO ()
setJointPaths mdlAnimationBindComponent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlAnimationBindComponent (mkSelector "setJointPaths:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @skeleton@
skeletonSelector :: Selector
skeletonSelector = mkSelector "skeleton"

-- | @Selector@ for @setSkeleton:@
setSkeletonSelector :: Selector
setSkeletonSelector = mkSelector "setSkeleton:"

-- | @Selector@ for @jointAnimation@
jointAnimationSelector :: Selector
jointAnimationSelector = mkSelector "jointAnimation"

-- | @Selector@ for @setJointAnimation:@
setJointAnimationSelector :: Selector
setJointAnimationSelector = mkSelector "setJointAnimation:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @setJointPaths:@
setJointPathsSelector :: Selector
setJointPathsSelector = mkSelector "setJointPaths:"

