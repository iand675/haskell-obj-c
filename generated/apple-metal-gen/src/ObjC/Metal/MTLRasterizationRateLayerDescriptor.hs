{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLRasterizationRateLayerDescriptor
--
-- Describes the minimum rasterization rate screen space using two piecewise linear functions.
--
-- The two piecewise linear function (PLF) describe the desired rasterization quality on the horizontal and vertical axis separately. Each quality sample in the PLF is stored in an array as single precision floating point value between 0 (lowest quality) and 1 (highest quality). The first sample in the array describes the quality at the top (vertical) or left (horizontal) edge of screen space. The last sample in the array describes the quality at the bottom (vertical) or right (horizontal) edge of screen space. All other samples are spaced equidistant in screen space. MTLRasterizationRateLayerDescriptor instances will be stored inside a MTLRasterizationRateMapDescriptor which in turn is compiled by MTLDevice into a MTLRasterizationRateMap. Because MTLDevice may not support the requested granularity, the provided samples may be rounded up (towards higher quality) during compilation.
--
-- Generated bindings for @MTLRasterizationRateLayerDescriptor@.
module ObjC.Metal.MTLRasterizationRateLayerDescriptor
  ( MTLRasterizationRateLayerDescriptor
  , IsMTLRasterizationRateLayerDescriptor(..)
  , init_
  , horizontalSampleStorage
  , verticalSampleStorage
  , horizontal
  , vertical
  , horizontalSampleStorageSelector
  , horizontalSelector
  , initSelector
  , verticalSampleStorageSelector
  , verticalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Do not use, instead use initWithNumSamples:
--
-- ObjC selector: @- init@
init_ :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateLayerDescriptor)
init_ mtlRasterizationRateLayerDescriptor =
  sendOwnedMessage mtlRasterizationRateLayerDescriptor initSelector

-- | horizontalSampleStorage
--
-- Provide direct access to the quality samples stored in the descriptor.
--
-- Returns: Pointer to the (mutable) storage array for samples on the horizontal axis.
--
-- The returned pointer points to the first element of an array of sampleCount.width elements.
--
-- ObjC selector: @- horizontalSampleStorage@
horizontalSampleStorage :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Ptr CFloat)
horizontalSampleStorage mtlRasterizationRateLayerDescriptor =
  sendMessage mtlRasterizationRateLayerDescriptor horizontalSampleStorageSelector

-- | verticalSampleStorage
--
-- Provide direct access to the quality samples stored in the descriptor.
--
-- Returns: Pointer to the (mutable) storage array for samples on the vertical axis.
--
-- The returned pointer points to the first element of an array of sampleCount.height elements.
--
-- ObjC selector: @- verticalSampleStorage@
verticalSampleStorage :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Ptr CFloat)
verticalSampleStorage mtlRasterizationRateLayerDescriptor =
  sendMessage mtlRasterizationRateLayerDescriptor verticalSampleStorageSelector

-- | horizontal
--
-- Provide convenient bounds-checked access to the quality samples stored in the descriptor.
--
-- Returns: Returns a syntactic sugar helper to get or set sample values on the horizontal axis.
--
-- ObjC selector: @- horizontal@
horizontal :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateSampleArray)
horizontal mtlRasterizationRateLayerDescriptor =
  sendMessage mtlRasterizationRateLayerDescriptor horizontalSelector

-- | vertical
--
-- Provide convenient bounds-checked access to the quality samples stored in the descriptor.
--
-- Returns: Returns a syntactic sugar helper to get or set sample values on the vertical axis.
--
-- ObjC selector: @- vertical@
vertical :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateSampleArray)
vertical mtlRasterizationRateLayerDescriptor =
  sendMessage mtlRasterizationRateLayerDescriptor verticalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTLRasterizationRateLayerDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @horizontalSampleStorage@
horizontalSampleStorageSelector :: Selector '[] (Ptr CFloat)
horizontalSampleStorageSelector = mkSelector "horizontalSampleStorage"

-- | @Selector@ for @verticalSampleStorage@
verticalSampleStorageSelector :: Selector '[] (Ptr CFloat)
verticalSampleStorageSelector = mkSelector "verticalSampleStorage"

-- | @Selector@ for @horizontal@
horizontalSelector :: Selector '[] (Id MTLRasterizationRateSampleArray)
horizontalSelector = mkSelector "horizontal"

-- | @Selector@ for @vertical@
verticalSelector :: Selector '[] (Id MTLRasterizationRateSampleArray)
verticalSelector = mkSelector "vertical"

