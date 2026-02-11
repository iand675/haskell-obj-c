{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLSkeleton@.
module ObjC.ModelIO.MDLSkeleton
  ( MDLSkeleton
  , IsMDLSkeleton(..)
  , initWithName_jointPaths
  , jointPaths
  , jointBindTransforms
  , jointRestTransforms
  , initWithName_jointPathsSelector
  , jointPathsSelector
  , jointBindTransformsSelector
  , jointRestTransformsSelector


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
initWithName_jointPaths :: (IsMDLSkeleton mdlSkeleton, IsNSString name, IsNSArray jointPaths) => mdlSkeleton -> name -> jointPaths -> IO (Id MDLSkeleton)
initWithName_jointPaths mdlSkeleton  name jointPaths =
withObjCPtr name $ \raw_name ->
  withObjCPtr jointPaths $ \raw_jointPaths ->
      sendMsg mdlSkeleton (mkSelector "initWithName:jointPaths:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_jointPaths :: Ptr ())] >>= ownedObject . castPtr

-- | @- jointPaths@
jointPaths :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id NSArray)
jointPaths mdlSkeleton  =
  sendMsg mdlSkeleton (mkSelector "jointPaths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- jointBindTransforms@
jointBindTransforms :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id MDLMatrix4x4Array)
jointBindTransforms mdlSkeleton  =
  sendMsg mdlSkeleton (mkSelector "jointBindTransforms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- jointRestTransforms@
jointRestTransforms :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id MDLMatrix4x4Array)
jointRestTransforms mdlSkeleton  =
  sendMsg mdlSkeleton (mkSelector "jointRestTransforms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:jointPaths:@
initWithName_jointPathsSelector :: Selector
initWithName_jointPathsSelector = mkSelector "initWithName:jointPaths:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @jointBindTransforms@
jointBindTransformsSelector :: Selector
jointBindTransformsSelector = mkSelector "jointBindTransforms"

-- | @Selector@ for @jointRestTransforms@
jointRestTransformsSelector :: Selector
jointRestTransformsSelector = mkSelector "jointRestTransforms"

