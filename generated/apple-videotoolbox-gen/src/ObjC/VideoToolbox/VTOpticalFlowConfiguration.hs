{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration that you use to set up an optical flow processor
--
-- This configuration enables the optical flow on a @VTFrameProcessor@ session.
--
-- Generated bindings for @VTOpticalFlowConfiguration@.
module ObjC.VideoToolbox.VTOpticalFlowConfiguration
  ( VTOpticalFlowConfiguration
  , IsVTOpticalFlowConfiguration(..)
  , initWithFrameWidth_frameHeight_qualityPrioritization_revision
  , init_
  , new
  , frameWidth
  , frameHeight
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
  , initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector
  , newSelector
  , processorSupportedSelector
  , qualityPrioritizationSelector
  , revisionSelector
  , sourcePixelBufferAttributesSelector
  , supportedRevisionsSelector
  , supportedSelector

  -- * Enum types
  , VTOpticalFlowConfigurationQualityPrioritization(VTOpticalFlowConfigurationQualityPrioritization)
  , pattern VTOpticalFlowConfigurationQualityPrioritizationNormal
  , pattern VTOpticalFlowConfigurationQualityPrioritizationQuality
  , VTOpticalFlowConfigurationRevision(VTOpticalFlowConfigurationRevision)
  , pattern VTOpticalFlowConfigurationRevision1

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

-- | Creates a new optical flow configuration.
--
-- Returns ``nil`` if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels; the maximum value is 8192 for macOS, and 4096 for iOS.   - frameHeight: Height of source frame in pixels; the maximum value is 4320 for macOS, and 2160 for iOS.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about supported   levels, see ``VTOpticalFlowConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_qualityPrioritization_revision :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> CLong -> CLong -> VTOpticalFlowConfigurationQualityPrioritization -> VTOpticalFlowConfigurationRevision -> IO (Id VTOpticalFlowConfiguration)
initWithFrameWidth_frameHeight_qualityPrioritization_revision vtOpticalFlowConfiguration frameWidth frameHeight qualityPrioritization revision =
  sendOwnedMessage vtOpticalFlowConfiguration initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector frameWidth frameHeight qualityPrioritization revision

-- | @- init@
init_ :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id VTOpticalFlowConfiguration)
init_ vtOpticalFlowConfiguration =
  sendOwnedMessage vtOpticalFlowConfiguration initSelector

-- | @+ new@
new :: IO (Id VTOpticalFlowConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO CLong
frameWidth vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration frameWidthSelector

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO CLong
frameHeight vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration frameHeightSelector

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTOpticalFlowConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO VTOpticalFlowConfigurationQualityPrioritization
qualityPrioritization vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration qualityPrioritizationSelector

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO VTOpticalFlowConfigurationRevision
revision vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration revisionSelector

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMessage cls' supportedRevisionsSelector

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTOpticalFlowConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMessage cls' defaultRevisionSelector

-- | Supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtOpticalFlowConfiguration =
  sendMessage vtOpticalFlowConfiguration destinationPixelBufferAttributesSelector

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMessage cls' supportedSelector

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMessage cls' processorSupportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector :: Selector '[CLong, CLong, VTOpticalFlowConfigurationQualityPrioritization, VTOpticalFlowConfigurationRevision] (Id VTOpticalFlowConfiguration)
initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:qualityPrioritization:revision:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTOpticalFlowConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTOpticalFlowConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector '[] CLong
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector '[] CLong
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @qualityPrioritization@
qualityPrioritizationSelector :: Selector '[] VTOpticalFlowConfigurationQualityPrioritization
qualityPrioritizationSelector = mkSelector "qualityPrioritization"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] VTOpticalFlowConfigurationRevision
revisionSelector = mkSelector "revision"

-- | @Selector@ for @supportedRevisions@
supportedRevisionsSelector :: Selector '[] (Id NSIndexSet)
supportedRevisionsSelector = mkSelector "supportedRevisions"

-- | @Selector@ for @defaultRevision@
defaultRevisionSelector :: Selector '[] VTOpticalFlowConfigurationRevision
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

