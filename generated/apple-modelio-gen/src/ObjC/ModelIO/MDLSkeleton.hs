{-# LANGUAGE DataKinds #-}
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
  , jointBindTransformsSelector
  , jointPathsSelector
  , jointRestTransformsSelector


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
initWithName_jointPaths :: (IsMDLSkeleton mdlSkeleton, IsNSString name, IsNSArray jointPaths) => mdlSkeleton -> name -> jointPaths -> IO (Id MDLSkeleton)
initWithName_jointPaths mdlSkeleton name jointPaths =
  sendOwnedMessage mdlSkeleton initWithName_jointPathsSelector (toNSString name) (toNSArray jointPaths)

-- | @- jointPaths@
jointPaths :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id NSArray)
jointPaths mdlSkeleton =
  sendMessage mdlSkeleton jointPathsSelector

-- | @- jointBindTransforms@
jointBindTransforms :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id MDLMatrix4x4Array)
jointBindTransforms mdlSkeleton =
  sendMessage mdlSkeleton jointBindTransformsSelector

-- | @- jointRestTransforms@
jointRestTransforms :: IsMDLSkeleton mdlSkeleton => mdlSkeleton -> IO (Id MDLMatrix4x4Array)
jointRestTransforms mdlSkeleton =
  sendMessage mdlSkeleton jointRestTransformsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:jointPaths:@
initWithName_jointPathsSelector :: Selector '[Id NSString, Id NSArray] (Id MDLSkeleton)
initWithName_jointPathsSelector = mkSelector "initWithName:jointPaths:"

-- | @Selector@ for @jointPaths@
jointPathsSelector :: Selector '[] (Id NSArray)
jointPathsSelector = mkSelector "jointPaths"

-- | @Selector@ for @jointBindTransforms@
jointBindTransformsSelector :: Selector '[] (Id MDLMatrix4x4Array)
jointBindTransformsSelector = mkSelector "jointBindTransforms"

-- | @Selector@ for @jointRestTransforms@
jointRestTransformsSelector :: Selector '[] (Id MDLMatrix4x4Array)
jointRestTransformsSelector = mkSelector "jointRestTransforms"

