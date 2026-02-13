{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageConversion
--
-- The MPSImageConversion filter performs a conversion from source to destination
--
-- Generated bindings for @MPSImageConversion@.
module ObjC.MetalPerformanceShaders.MPSImageConversion
  ( MPSImageConversion
  , IsMPSImageConversion(..)
  , initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfo
  , sourceAlpha
  , destinationAlpha
  , destinationAlphaSelector
  , initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfoSelector
  , sourceAlphaSelector

  -- * Enum types
  , MPSAlphaType(MPSAlphaType)
  , pattern MPSAlphaTypeNonPremultiplied
  , pattern MPSAlphaTypeAlphaIsOne
  , pattern MPSAlphaTypePremultiplied

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a converter that can convert texture colorspace, alpha and texture format
--
-- Create a converter that can convert texture colorspace, alpha and MTLPixelFormat.               Optimized cases exist for NULL color space converter and no alpha conversion.
--
-- @device@ — The device the filter will run on
--
-- @srcAlpha@ — The alpha encoding for the source texture
--
-- @destAlpha@ — The alpha encoding for the destination texture
--
-- @backgroundColor@ — An array of CGFloats giving the background color to use when flattening an image.                                  The color is in the source colorspace.  The length of the array is the number                                   of color channels in the src colorspace. If NULL, use {0}.
--
-- @conversionInfo@ — The colorspace conversion to use. May be NULL, indicating no                                  color space conversions need to be done.
--
-- Returns: An initialized MPSImageConversion object.
--
-- ObjC selector: @- initWithDevice:srcAlpha:destAlpha:backgroundColor:conversionInfo:@
initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfo :: IsMPSImageConversion mpsImageConversion => mpsImageConversion -> RawId -> MPSAlphaType -> MPSAlphaType -> Ptr CDouble -> RawId -> IO (Id MPSImageConversion)
initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfo mpsImageConversion device srcAlpha destAlpha backgroundColor conversionInfo =
  sendOwnedMessage mpsImageConversion initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfoSelector device srcAlpha destAlpha backgroundColor conversionInfo

-- | sourceAlpha
--
-- Premultiplication description for the source texture
--
-- Most colorspace conversion operations can not work directly on premultiplied data.              Use this property to tag premultiplied data so that the source texture can              be unpremultiplied prior to application of these transforms.               Default: MPSPixelAlpha_AlphaIsOne
--
-- ObjC selector: @- sourceAlpha@
sourceAlpha :: IsMPSImageConversion mpsImageConversion => mpsImageConversion -> IO MPSAlphaType
sourceAlpha mpsImageConversion =
  sendMessage mpsImageConversion sourceAlphaSelector

-- | destinationAlpha
--
-- Premultiplication description for the destinationAlpha texture
--
-- Colorspace conversion operations produce non-premultiplied data.              Use this property to tag cases where premultiplied results are required.              If MPSPixelAlpha_AlphaIsOne is used, the alpha channel will be set to 1.               Default: MPSPixelAlpha_AlphaIsOne
--
-- ObjC selector: @- destinationAlpha@
destinationAlpha :: IsMPSImageConversion mpsImageConversion => mpsImageConversion -> IO MPSAlphaType
destinationAlpha mpsImageConversion =
  sendMessage mpsImageConversion destinationAlphaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:srcAlpha:destAlpha:backgroundColor:conversionInfo:@
initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfoSelector :: Selector '[RawId, MPSAlphaType, MPSAlphaType, Ptr CDouble, RawId] (Id MPSImageConversion)
initWithDevice_srcAlpha_destAlpha_backgroundColor_conversionInfoSelector = mkSelector "initWithDevice:srcAlpha:destAlpha:backgroundColor:conversionInfo:"

-- | @Selector@ for @sourceAlpha@
sourceAlphaSelector :: Selector '[] MPSAlphaType
sourceAlphaSelector = mkSelector "sourceAlpha"

-- | @Selector@ for @destinationAlpha@
destinationAlphaSelector :: Selector '[] MPSAlphaType
destinationAlphaSelector = mkSelector "destinationAlpha"

