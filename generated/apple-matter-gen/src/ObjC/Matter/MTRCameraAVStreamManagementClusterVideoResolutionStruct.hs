{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoResolutionStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoResolutionStruct
  ( MTRCameraAVStreamManagementClusterVideoResolutionStruct
  , IsMTRCameraAVStreamManagementClusterVideoResolutionStruct(..)
  , width
  , setWidth
  , height
  , setHeight
  , heightSelector
  , setHeightSelector
  , setWidthSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- width@
width :: IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> IO (Id NSNumber)
width mtrCameraAVStreamManagementClusterVideoResolutionStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoResolutionStruct widthSelector

-- | @- setWidth:@
setWidth :: (IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> value -> IO ()
setWidth mtrCameraAVStreamManagementClusterVideoResolutionStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoResolutionStruct setWidthSelector (toNSNumber value)

-- | @- height@
height :: IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> IO (Id NSNumber)
height mtrCameraAVStreamManagementClusterVideoResolutionStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoResolutionStruct heightSelector

-- | @- setHeight:@
setHeight :: (IsMTRCameraAVStreamManagementClusterVideoResolutionStruct mtrCameraAVStreamManagementClusterVideoResolutionStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoResolutionStruct -> value -> IO ()
setHeight mtrCameraAVStreamManagementClusterVideoResolutionStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoResolutionStruct setHeightSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSNumber)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSNumber] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSNumber)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSNumber] ()
setHeightSelector = mkSelector "setHeight:"

