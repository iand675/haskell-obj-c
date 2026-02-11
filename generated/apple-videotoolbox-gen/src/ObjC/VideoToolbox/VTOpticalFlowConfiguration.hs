{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector
  , initSelector
  , newSelector
  , frameWidthSelector
  , frameHeightSelector
  , qualityPrioritizationSelector
  , revisionSelector
  , supportedRevisionsSelector
  , defaultRevisionSelector
  , frameSupportedPixelFormatsSelector
  , sourcePixelBufferAttributesSelector
  , destinationPixelBufferAttributesSelector
  , supportedSelector
  , processorSupportedSelector

  -- * Enum types
  , VTOpticalFlowConfigurationQualityPrioritization(VTOpticalFlowConfigurationQualityPrioritization)
  , pattern VTOpticalFlowConfigurationQualityPrioritizationNormal
  , pattern VTOpticalFlowConfigurationQualityPrioritizationQuality
  , VTOpticalFlowConfigurationRevision(VTOpticalFlowConfigurationRevision)
  , pattern VTOpticalFlowConfigurationRevision1

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
initWithFrameWidth_frameHeight_qualityPrioritization_revision vtOpticalFlowConfiguration  frameWidth frameHeight qualityPrioritization revision =
    sendMsg vtOpticalFlowConfiguration (mkSelector "initWithFrameWidth:frameHeight:qualityPrioritization:revision:") (retPtr retVoid) [argCLong frameWidth, argCLong frameHeight, argCLong (coerce qualityPrioritization), argCLong (coerce revision)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id VTOpticalFlowConfiguration)
init_ vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTOpticalFlowConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO CLong
frameWidth vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO CLong
frameHeight vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "frameHeight") retCLong []

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTOpticalFlowConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO VTOpticalFlowConfigurationQualityPrioritization
qualityPrioritization vtOpticalFlowConfiguration  =
    fmap (coerce :: CLong -> VTOpticalFlowConfigurationQualityPrioritization) $ sendMsg vtOpticalFlowConfiguration (mkSelector "qualityPrioritization") retCLong []

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO VTOpticalFlowConfigurationRevision
revision vtOpticalFlowConfiguration  =
    fmap (coerce :: CLong -> VTOpticalFlowConfigurationRevision) $ sendMsg vtOpticalFlowConfiguration (mkSelector "revision") retCLong []

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMsg cls' (mkSelector "supportedRevisions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTOpticalFlowConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    fmap (coerce :: CLong -> VTOpticalFlowConfigurationRevision) $ sendClassMsg cls' (mkSelector "defaultRevision") retCLong []

-- | Supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "frameSupportedPixelFormats") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTOpticalFlowConfiguration vtOpticalFlowConfiguration => vtOpticalFlowConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtOpticalFlowConfiguration  =
    sendMsg vtOpticalFlowConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTOpticalFlowConfiguration"
    sendClassMsg cls' (mkSelector "processorSupported") retCUChar []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector :: Selector
initWithFrameWidth_frameHeight_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:qualityPrioritization:revision:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @qualityPrioritization@
qualityPrioritizationSelector :: Selector
qualityPrioritizationSelector = mkSelector "qualityPrioritization"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

-- | @Selector@ for @supportedRevisions@
supportedRevisionsSelector :: Selector
supportedRevisionsSelector = mkSelector "supportedRevisions"

-- | @Selector@ for @defaultRevision@
defaultRevisionSelector :: Selector
defaultRevisionSelector = mkSelector "defaultRevision"

-- | @Selector@ for @frameSupportedPixelFormats@
frameSupportedPixelFormatsSelector :: Selector
frameSupportedPixelFormatsSelector = mkSelector "frameSupportedPixelFormats"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @destinationPixelBufferAttributes@
destinationPixelBufferAttributesSelector :: Selector
destinationPixelBufferAttributesSelector = mkSelector "destinationPixelBufferAttributes"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @processorSupported@
processorSupportedSelector :: Selector
processorSupportedSelector = mkSelector "processorSupported"

