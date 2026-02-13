{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLPackedJointAnimation@.
module ObjC.ModelIO.MDLPackedJointAnimation
  ( MDLPackedJointAnimation
  , IsMDLPackedJointAnimation(..)
  , initWithName_jointPaths
  , jointPaths
  , translations
  , rotations
  , scales
  , initWithName_jointPathsSelector
  , jointPathsSelector
  , rotationsSelector
  , scalesSelector
  , translationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:jointPaths:@
initWithName_jointPaths :: (IsMDLPackedJointAnimation mdlPackedJointAnimation, IsNSString name, IsNSArray jointPaths) => mdlPackedJointAnimation -> name -> jointPaths -> IO (Id MDLPackedJointAnimation)
initWithName_jointPaths mdlPackedJointAnimation name jointPaths =
  sendOwnedMessage mdlPackedJointAnimation initWithName_jointPathsSelector (toNSString name) (toNSArray jointPaths)

-- | @- jointPaths@
jointPaths :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id NSArray)
jointPaths mdlPackedJointAnimation =
  sendMessage mdlPackedJointAnimation jointPathsSelector

-- | @- translations@
translations :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedVector3Array)
translations mdlPackedJointAnimation =
  sendMessage mdlPackedJointAnimation translationsSelector

-- | @- rotations@
rotations :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedQuaternionArray)
rotations mdlPackedJointAnimation =
  sendMessage mdlPackedJointAnimation rotationsSelector

-- | @- scales@
scales :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedVector3Array)
scales mdlPackedJointAnimation =
  sendMessage mdlPackedJointAnimation scalesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:jointPaths:@
initWithName_jointPathsSelector :: Selector '[Id NSString, Id NSArray] (Id MDLPackedJointAnimation)
initWithName_jointPathsSelector = mkSelector "initWithName:jointPaths:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector '[] (Id NSArray)
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @translations@
translationsSelector :: Selector '[] (Id MDLAnimatedVector3Array)
translationsSelector = mkSelector "translations"

-- | @Selector@ for @rotations@
rotationsSelector :: Selector '[] (Id MDLAnimatedQuaternionArray)
rotationsSelector = mkSelector "rotations"

-- | @Selector@ for @scales@
scalesSelector :: Selector '[] (Id MDLAnimatedVector3Array)
scalesSelector = mkSelector "scales"

