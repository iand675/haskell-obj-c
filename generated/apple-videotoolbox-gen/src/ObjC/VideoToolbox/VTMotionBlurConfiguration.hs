{-# LANGUAGE PatternSynonyms #-}
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
  , initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector
  , initSelector
  , newSelector
  , frameWidthSelector
  , frameHeightSelector
  , usePrecomputedFlowSelector
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
  , VTMotionBlurConfigurationQualityPrioritization(VTMotionBlurConfigurationQualityPrioritization)
  , pattern VTMotionBlurConfigurationQualityPrioritizationNormal
  , pattern VTMotionBlurConfigurationQualityPrioritizationQuality
  , VTMotionBlurConfigurationRevision(VTMotionBlurConfigurationRevision)
  , pattern VTMotionBlurConfigurationRevision1

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

-- | Creates a new motion blur configuration.
--
-- Returns @nil@ if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels; the maximum value is 8192 for macOS, and 4096 for iOS.   - frameHeight: Height of source frame in pixels; the maximum value is 4320 for macOS, and 2160 for iOS.   - usePrecomputedFlow: Boolean value that indicates whether you will provide optical flow; if false, this    configuration computes the optical flow on the fly.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about supported    levels, see ``VTMotionBlurConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> CLong -> CLong -> Bool -> VTMotionBlurConfigurationQualityPrioritization -> VTMotionBlurConfigurationRevision -> IO (Id VTMotionBlurConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision vtMotionBlurConfiguration  frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision =
    sendMsg vtMotionBlurConfiguration (mkSelector "initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:") (retPtr retVoid) [argCLong frameWidth, argCLong frameHeight, argCULong (if usePrecomputedFlow then 1 else 0), argCLong (coerce qualityPrioritization), argCLong (coerce revision)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id VTMotionBlurConfiguration)
init_ vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTMotionBlurConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO CLong
frameWidth vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO CLong
frameHeight vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "frameHeight") retCLong []

-- | Indicates that you provide optical flow.
--
-- ObjC selector: @- usePrecomputedFlow@
usePrecomputedFlow :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO Bool
usePrecomputedFlow vtMotionBlurConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtMotionBlurConfiguration (mkSelector "usePrecomputedFlow") retCULong []

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTMotionBlurConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO VTMotionBlurConfigurationQualityPrioritization
qualityPrioritization vtMotionBlurConfiguration  =
    fmap (coerce :: CLong -> VTMotionBlurConfigurationQualityPrioritization) $ sendMsg vtMotionBlurConfiguration (mkSelector "qualityPrioritization") retCLong []

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO VTMotionBlurConfigurationRevision
revision vtMotionBlurConfiguration  =
    fmap (coerce :: CLong -> VTMotionBlurConfigurationRevision) $ sendMsg vtMotionBlurConfiguration (mkSelector "revision") retCLong []

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMsg cls' (mkSelector "supportedRevisions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTMotionBlurConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    fmap (coerce :: CLong -> VTMotionBlurConfigurationRevision) $ sendClassMsg cls' (mkSelector "defaultRevision") retCLong []

-- | Available supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "frameSupportedPixelFormats") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTMotionBlurConfiguration vtMotionBlurConfiguration => vtMotionBlurConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtMotionBlurConfiguration  =
    sendMsg vtMotionBlurConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTMotionBlurConfiguration"
    sendClassMsg cls' (mkSelector "processorSupported") retCUChar []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector :: Selector
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:"

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

-- | @Selector@ for @usePrecomputedFlow@
usePrecomputedFlowSelector :: Selector
usePrecomputedFlowSelector = mkSelector "usePrecomputedFlow"

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

