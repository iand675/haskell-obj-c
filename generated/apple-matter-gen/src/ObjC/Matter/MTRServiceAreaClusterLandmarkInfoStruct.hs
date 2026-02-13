{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterLandmarkInfoStruct@.
module ObjC.Matter.MTRServiceAreaClusterLandmarkInfoStruct
  ( MTRServiceAreaClusterLandmarkInfoStruct
  , IsMTRServiceAreaClusterLandmarkInfoStruct(..)
  , landmarkTag
  , setLandmarkTag
  , relativePositionTag
  , setRelativePositionTag
  , landmarkTagSelector
  , relativePositionTagSelector
  , setLandmarkTagSelector
  , setRelativePositionTagSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- landmarkTag@
landmarkTag :: IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct => mtrServiceAreaClusterLandmarkInfoStruct -> IO (Id NSNumber)
landmarkTag mtrServiceAreaClusterLandmarkInfoStruct =
  sendMessage mtrServiceAreaClusterLandmarkInfoStruct landmarkTagSelector

-- | @- setLandmarkTag:@
setLandmarkTag :: (IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct, IsNSNumber value) => mtrServiceAreaClusterLandmarkInfoStruct -> value -> IO ()
setLandmarkTag mtrServiceAreaClusterLandmarkInfoStruct value =
  sendMessage mtrServiceAreaClusterLandmarkInfoStruct setLandmarkTagSelector (toNSNumber value)

-- | @- relativePositionTag@
relativePositionTag :: IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct => mtrServiceAreaClusterLandmarkInfoStruct -> IO (Id NSNumber)
relativePositionTag mtrServiceAreaClusterLandmarkInfoStruct =
  sendMessage mtrServiceAreaClusterLandmarkInfoStruct relativePositionTagSelector

-- | @- setRelativePositionTag:@
setRelativePositionTag :: (IsMTRServiceAreaClusterLandmarkInfoStruct mtrServiceAreaClusterLandmarkInfoStruct, IsNSNumber value) => mtrServiceAreaClusterLandmarkInfoStruct -> value -> IO ()
setRelativePositionTag mtrServiceAreaClusterLandmarkInfoStruct value =
  sendMessage mtrServiceAreaClusterLandmarkInfoStruct setRelativePositionTagSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @landmarkTag@
landmarkTagSelector :: Selector '[] (Id NSNumber)
landmarkTagSelector = mkSelector "landmarkTag"

-- | @Selector@ for @setLandmarkTag:@
setLandmarkTagSelector :: Selector '[Id NSNumber] ()
setLandmarkTagSelector = mkSelector "setLandmarkTag:"

-- | @Selector@ for @relativePositionTag@
relativePositionTagSelector :: Selector '[] (Id NSNumber)
relativePositionTagSelector = mkSelector "relativePositionTag"

-- | @Selector@ for @setRelativePositionTag:@
setRelativePositionTagSelector :: Selector '[Id NSNumber] ()
setRelativePositionTagSelector = mkSelector "setRelativePositionTag:"

