{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsampling
--
-- This depends on Metal.framework
--
-- The MPSCNNUpsampling filter can be used to resample an existing MPSImage              using a different sampling frequency for the x and y dimensions with the purpose of              enlarging the size of an image.
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- The scaleFactor must be an integer value >= 1. The default value is 1.              If scaleFactor == 1, the filter acts as a copy kernel.
--
-- Nearest and bilinear variants are supported.
--
-- Generated bindings for @MPSCNNUpsampling@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsampling
  ( MPSCNNUpsampling
  , IsMPSCNNUpsampling(..)
  , initWithDevice
  , scaleFactorX
  , scaleFactorY
  , alignCorners
  , alignCornersSelector
  , initWithDeviceSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> RawId -> IO (Id MPSCNNUpsampling)
initWithDevice mpscnnUpsampling device =
  sendOwnedMessage mpscnnUpsampling initWithDeviceSelector device

-- | scaleFactorX
--
-- The upsampling scale factor for the x dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO CDouble
scaleFactorX mpscnnUpsampling =
  sendMessage mpscnnUpsampling scaleFactorXSelector

-- | scaleFactorY
--
-- The upsampling scale factor for the y dimension. The default value is 1.
--
-- ObjC selector: @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO CDouble
scaleFactorY mpscnnUpsampling =
  sendMessage mpscnnUpsampling scaleFactorYSelector

-- | alignCorners
--
-- If YES, the centers of the 4 corner pixels of the input and output regions are aligned,              preserving the values at the corner pixels.              The default is NO.
--
-- ObjC selector: @- alignCorners@
alignCorners :: IsMPSCNNUpsampling mpscnnUpsampling => mpscnnUpsampling -> IO Bool
alignCorners mpscnnUpsampling =
  sendMessage mpscnnUpsampling alignCornersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNUpsampling)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector '[] CDouble
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector '[] CDouble
scaleFactorYSelector = mkSelector "scaleFactorY"

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector '[] Bool
alignCornersSelector = mkSelector "alignCorners"

