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
  , translationsSelector
  , rotationsSelector
  , scalesSelector


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

-- | @- initWithName:jointPaths:@
initWithName_jointPaths :: (IsMDLPackedJointAnimation mdlPackedJointAnimation, IsNSString name, IsNSArray jointPaths) => mdlPackedJointAnimation -> name -> jointPaths -> IO (Id MDLPackedJointAnimation)
initWithName_jointPaths mdlPackedJointAnimation  name jointPaths =
withObjCPtr name $ \raw_name ->
  withObjCPtr jointPaths $ \raw_jointPaths ->
      sendMsg mdlPackedJointAnimation (mkSelector "initWithName:jointPaths:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_jointPaths :: Ptr ())] >>= ownedObject . castPtr

-- | @- jointPaths@
jointPaths :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id NSArray)
jointPaths mdlPackedJointAnimation  =
  sendMsg mdlPackedJointAnimation (mkSelector "jointPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- translations@
translations :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedVector3Array)
translations mdlPackedJointAnimation  =
  sendMsg mdlPackedJointAnimation (mkSelector "translations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rotations@
rotations :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedQuaternionArray)
rotations mdlPackedJointAnimation  =
  sendMsg mdlPackedJointAnimation (mkSelector "rotations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scales@
scales :: IsMDLPackedJointAnimation mdlPackedJointAnimation => mdlPackedJointAnimation -> IO (Id MDLAnimatedVector3Array)
scales mdlPackedJointAnimation  =
  sendMsg mdlPackedJointAnimation (mkSelector "scales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:jointPaths:@
initWithName_jointPathsSelector :: Selector
initWithName_jointPathsSelector = mkSelector "initWithName:jointPaths:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @translations@
translationsSelector :: Selector
translationsSelector = mkSelector "translations"

-- | @Selector@ for @rotations@
rotationsSelector :: Selector
rotationsSelector = mkSelector "rotations"

-- | @Selector@ for @scales@
scalesSelector :: Selector
scalesSelector = mkSelector "scales"

