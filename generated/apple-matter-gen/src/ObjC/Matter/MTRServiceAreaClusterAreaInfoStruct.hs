{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterAreaInfoStruct@.
module ObjC.Matter.MTRServiceAreaClusterAreaInfoStruct
  ( MTRServiceAreaClusterAreaInfoStruct
  , IsMTRServiceAreaClusterAreaInfoStruct(..)
  , locationInfo
  , setLocationInfo
  , landmarkInfo
  , setLandmarkInfo
  , landmarkInfoSelector
  , locationInfoSelector
  , setLandmarkInfoSelector
  , setLocationInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- locationInfo@
locationInfo :: IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct => mtrServiceAreaClusterAreaInfoStruct -> IO (Id MTRDataTypeLocationDescriptorStruct)
locationInfo mtrServiceAreaClusterAreaInfoStruct =
  sendMessage mtrServiceAreaClusterAreaInfoStruct locationInfoSelector

-- | @- setLocationInfo:@
setLocationInfo :: (IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct, IsMTRDataTypeLocationDescriptorStruct value) => mtrServiceAreaClusterAreaInfoStruct -> value -> IO ()
setLocationInfo mtrServiceAreaClusterAreaInfoStruct value =
  sendMessage mtrServiceAreaClusterAreaInfoStruct setLocationInfoSelector (toMTRDataTypeLocationDescriptorStruct value)

-- | @- landmarkInfo@
landmarkInfo :: IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct => mtrServiceAreaClusterAreaInfoStruct -> IO (Id MTRServiceAreaClusterLandmarkInfoStruct)
landmarkInfo mtrServiceAreaClusterAreaInfoStruct =
  sendMessage mtrServiceAreaClusterAreaInfoStruct landmarkInfoSelector

-- | @- setLandmarkInfo:@
setLandmarkInfo :: (IsMTRServiceAreaClusterAreaInfoStruct mtrServiceAreaClusterAreaInfoStruct, IsMTRServiceAreaClusterLandmarkInfoStruct value) => mtrServiceAreaClusterAreaInfoStruct -> value -> IO ()
setLandmarkInfo mtrServiceAreaClusterAreaInfoStruct value =
  sendMessage mtrServiceAreaClusterAreaInfoStruct setLandmarkInfoSelector (toMTRServiceAreaClusterLandmarkInfoStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationInfo@
locationInfoSelector :: Selector '[] (Id MTRDataTypeLocationDescriptorStruct)
locationInfoSelector = mkSelector "locationInfo"

-- | @Selector@ for @setLocationInfo:@
setLocationInfoSelector :: Selector '[Id MTRDataTypeLocationDescriptorStruct] ()
setLocationInfoSelector = mkSelector "setLocationInfo:"

-- | @Selector@ for @landmarkInfo@
landmarkInfoSelector :: Selector '[] (Id MTRServiceAreaClusterLandmarkInfoStruct)
landmarkInfoSelector = mkSelector "landmarkInfo"

-- | @Selector@ for @setLandmarkInfo:@
setLandmarkInfoSelector :: Selector '[Id MTRServiceAreaClusterLandmarkInfoStruct] ()
setLandmarkInfoSelector = mkSelector "setLandmarkInfo:"

