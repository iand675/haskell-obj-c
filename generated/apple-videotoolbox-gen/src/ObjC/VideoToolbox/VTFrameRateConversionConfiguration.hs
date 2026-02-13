{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration that you use to set up the frame rate conversion processor.
--
-- This configuration enables the frame-rate conversion on a @VTFrameProcessor@ session.
--
-- Generated bindings for @VTFrameRateConversionConfiguration@.
module ObjC.VideoToolbox.VTFrameRateConversionConfiguration
  ( VTFrameRateConversionConfiguration
  , IsVTFrameRateConversionConfiguration(..)
  , initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision
  , init_
  , new
  , frameWidth
  , frameHeight
  , usePrecomputedFlow
  , qualityPrioritization
  , revision
  , supportedRevisions
  , defaultRevision
  , frameSupportedPixelFormats
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , supported
  , processorSupported
  , defaultRevisionSelector
  , destinationPixelBufferAttributesSelector
  , frameHeightSelector
  , frameSupportedPixelFormatsSelector
  , frameWidthSelector
  , initSelector
  , initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector
  , newSelector
  , processorSupportedSelector
  , qualityPrioritizationSelector
  , revisionSelector
  , sourcePixelBufferAttributesSelector
  , supportedRevisionsSelector
  , supportedSelector
  , usePrecomputedFlowSelector

  -- * Enum types
  , VTFrameRateConversionConfigurationQualityPrioritization(VTFrameRateConversionConfigurationQualityPrioritization)
  , pattern VTFrameRateConversionConfigurationQualityPrioritizationNormal
  , pattern VTFrameRateConversionConfigurationQualityPrioritizationQuality
  , VTFrameRateConversionConfigurationRevision(VTFrameRateConversionConfigurationRevision)
  , pattern VTFrameRateConversionConfigurationRevision1

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.VideoToolbox.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a new frame-rate conversion configuration.
--
-- Returns @nil@ if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels; the maximum value is 8192 for macOS, and 4096 for iOS.   - frameHeight: Height of source frame in pixels; the maximum value is 4320 for macOS, and 2160 for iOS.   - usePrecomputedFlow: A Boolean value that indicates whether you are providing Optical Flow. If false, optical flow is computed on the fly.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about supported levels, see ``VTFrameRateConversionConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> CLong -> CLong -> Bool -> VTFrameRateConversionConfigurationQualityPrioritization -> VTFrameRateConversionConfigurationRevision -> IO (Id VTFrameRateConversionConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision vtFrameRateConversionConfiguration frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision =
  sendOwnedMessage vtFrameRateConversionConfiguration initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision

-- | @- init@
init_ :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id VTFrameRateConversionConfiguration)
init_ vtFrameRateConversionConfiguration =
  sendOwnedMessage vtFrameRateConversionConfiguration initSelector

-- | @+ new@
new :: IO (Id VTFrameRateConversionConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO CLong
frameWidth vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration frameWidthSelector

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO CLong
frameHeight vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration frameHeightSelector

-- | Indicates that caller provides optical flow.
--
-- ObjC selector: @- usePrecomputedFlow@
usePrecomputedFlow :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO Bool
usePrecomputedFlow vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration usePrecomputedFlowSelector

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTFrameRateConversionConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO VTFrameRateConversionConfigurationQualityPrioritization
qualityPrioritization vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration qualityPrioritizationSelector

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO VTFrameRateConversionConfigurationRevision
revision vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration revisionSelector

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMessage cls' supportedRevisionsSelector

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTFrameRateConversionConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMessage cls' defaultRevisionSelector

-- | Supported pixel formats available for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtFrameRateConversionConfiguration =
  sendMessage vtFrameRateConversionConfiguration destinationPixelBufferAttributesSelector

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMessage cls' supportedSelector

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMessage cls' processorSupportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector :: Selector '[CLong, CLong, Bool, VTFrameRateConversionConfigurationQualityPrioritization, VTFrameRateConversionConfigurationRevision] (Id VTFrameRateConversionConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTFrameRateConversionConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTFrameRateConversionConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector '[] CLong
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector '[] CLong
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @usePrecomputedFlow@
usePrecomputedFlowSelector :: Selector '[] Bool
usePrecomputedFlowSelector = mkSelector "usePrecomputedFlow"

-- | @Selector@ for @qualityPrioritization@
qualityPrioritizationSelector :: Selector '[] VTFrameRateConversionConfigurationQualityPrioritization
qualityPrioritizationSelector = mkSelector "qualityPrioritization"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] VTFrameRateConversionConfigurationRevision
revisionSelector = mkSelector "revision"

-- | @Selector@ for @supportedRevisions@
supportedRevisionsSelector :: Selector '[] (Id NSIndexSet)
supportedRevisionsSelector = mkSelector "supportedRevisions"

-- | @Selector@ for @defaultRevision@
defaultRevisionSelector :: Selector '[] VTFrameRateConversionConfigurationRevision
defaultRevisionSelector = mkSelector "defaultRevision"

-- | @Selector@ for @frameSupportedPixelFormats@
frameSupportedPixelFormatsSelector :: Selector '[] (Id NSArray)
frameSupportedPixelFormatsSelector = mkSelector "frameSupportedPixelFormats"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @destinationPixelBufferAttributes@
destinationPixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
destinationPixelBufferAttributesSelector = mkSelector "destinationPixelBufferAttributes"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

-- | @Selector@ for @processorSupported@
processorSupportedSelector :: Selector '[] CUChar
processorSupportedSelector = mkSelector "processorSupported"

