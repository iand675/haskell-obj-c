{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration that you use to set up the motion blur processor.
--
-- This configuration enables the motion blur on a @VTFrameProcessor@ session.
--
-- Generated bindings for @VTMotionBlurConfiguration@.
module ObjC.VideoToolbox.VTMotionBlurConfiguration
  ( VTMotionBlurConfiguration
  , IsVTMotionBlurConfiguration(..)
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
  , VTMotionBlurConfigurationQualityPrioritization(VTMotionBlurConfigurationQualityPrioritization)
  , pattern VTMotionBlurConfigurationQualityPrioritizationNormal
  , pattern VTMotionBlurConfigurationQualityPrioritizationQuality
  , VTMotionBlurConfigurationRevision(VTMotionBlurConfigurationRevision)
  , pattern VTMotionBlurConfigurationRevision1

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

-- | Creates a new motion blur configuration.
--
-- Returns @nil@ if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels; the maximum value is 8192 for macOS, and 4096 for iOS.   - frameHeight: Height of source frame in pixels; the maximum value is 4320 for macOS, and 2160 for iOS.   - usePrecomputedFlow: Boolean value that indicates whether you will provide optical flow; if false, this    configuration computes the optical flow on the fly.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about supported    levels, see ``VTMotionBlurConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> CLong -> CLong -> Bool -> VTMotionBlurConfigurationQualityPrioritization -> VTMotionBlurConfigurationRevision -> IO (Id VTMotionBlurConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision vtMotionBlurConfiguration frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision =
  sendOwnedMessage vtMotionBlurConfiguration initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision

-- | @- init@
init_ :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id VTMotionBlurConfiguration)
init_ vtMotionBlurConfiguration =
  sendOwnedMessage vtMotionBlurConfiguration initSelector

-- | @+ new@
new :: IO (Id VTMotionBlurConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO CLong
frameWidth vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration frameWidthSelector

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO CLong
frameHeight vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration frameHeightSelector

-- | Indicates that you provide optical flow.
--
-- ObjC selector: @- usePrecomputedFlow@
usePrecomputedFlow :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO Bool
usePrecomputedFlow vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration usePrecomputedFlowSelector

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTMotionBlurConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO VTMotionBlurConfigurationQualityPrioritization
qualityPrioritization vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration qualityPrioritizationSelector

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO VTMotionBlurConfigurationRevision
revision vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration revisionSelector

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMessage cls' supportedRevisionsSelector

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTMotionBlurConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMessage cls' defaultRevisionSelector

-- | Available supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtMotionBlurConfiguration =
  sendMessage vtMotionBlurConfiguration destinationPixelBufferAttributesSelector

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMessage cls' supportedSelector

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMessage cls' processorSupportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector :: Selector '[CLong, CLong, Bool, VTMotionBlurConfigurationQualityPrioritization, VTMotionBlurConfigurationRevision] (Id VTMotionBlurConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTMotionBlurConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTMotionBlurConfiguration)
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
qualityPrioritizationSelector :: Selector '[] VTMotionBlurConfigurationQualityPrioritization
qualityPrioritizationSelector = mkSelector "qualityPrioritization"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] VTMotionBlurConfigurationRevision
revisionSelector = mkSelector "revision"

-- | @Selector@ for @supportedRevisions@
supportedRevisionsSelector :: Selector '[] (Id NSIndexSet)
supportedRevisionsSelector = mkSelector "supportedRevisions"

-- | @Selector@ for @defaultRevision@
defaultRevisionSelector :: Selector '[] VTMotionBlurConfigurationRevision
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

