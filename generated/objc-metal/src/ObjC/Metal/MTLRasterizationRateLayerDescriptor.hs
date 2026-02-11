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
  , initSelector
  , horizontalSampleStorageSelector
  , verticalSampleStorageSelector
  , horizontalSelector
  , verticalSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Do not use, instead use initWithNumSamples:
--
-- ObjC selector: @- init@
init_ :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateLayerDescriptor)
init_ mtlRasterizationRateLayerDescriptor  =
  sendMsg mtlRasterizationRateLayerDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
horizontalSampleStorage mtlRasterizationRateLayerDescriptor  =
  fmap castPtr $ sendMsg mtlRasterizationRateLayerDescriptor (mkSelector "horizontalSampleStorage") (retPtr retVoid) []

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
verticalSampleStorage mtlRasterizationRateLayerDescriptor  =
  fmap castPtr $ sendMsg mtlRasterizationRateLayerDescriptor (mkSelector "verticalSampleStorage") (retPtr retVoid) []

-- | horizontal
--
-- Provide convenient bounds-checked access to the quality samples stored in the descriptor.
--
-- Returns: Returns a syntactic sugar helper to get or set sample values on the horizontal axis.
--
-- ObjC selector: @- horizontal@
horizontal :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateSampleArray)
horizontal mtlRasterizationRateLayerDescriptor  =
  sendMsg mtlRasterizationRateLayerDescriptor (mkSelector "horizontal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vertical
--
-- Provide convenient bounds-checked access to the quality samples stored in the descriptor.
--
-- Returns: Returns a syntactic sugar helper to get or set sample values on the vertical axis.
--
-- ObjC selector: @- vertical@
vertical :: IsMTLRasterizationRateLayerDescriptor mtlRasterizationRateLayerDescriptor => mtlRasterizationRateLayerDescriptor -> IO (Id MTLRasterizationRateSampleArray)
vertical mtlRasterizationRateLayerDescriptor  =
  sendMsg mtlRasterizationRateLayerDescriptor (mkSelector "vertical") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @horizontalSampleStorage@
horizontalSampleStorageSelector :: Selector
horizontalSampleStorageSelector = mkSelector "horizontalSampleStorage"

-- | @Selector@ for @verticalSampleStorage@
verticalSampleStorageSelector :: Selector
verticalSampleStorageSelector = mkSelector "verticalSampleStorage"

-- | @Selector@ for @horizontal@
horizontalSelector :: Selector
horizontalSelector = mkSelector "horizontal"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

