{-# LANGUAGE PatternSynonyms #-}
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
  , sourcePixelBufferAttributesSelector
  , destinationPixelBufferAttributesSelector
  , supportedSelector
  , processorSupportedSelector

  -- * Enum types
  , VTFrameRateConversionConfigurationQualityPrioritization(VTFrameRateConversionConfigurationQualityPrioritization)
  , pattern VTFrameRateConversionConfigurationQualityPrioritizationNormal
  , pattern VTFrameRateConversionConfigurationQualityPrioritizationQuality
  , VTFrameRateConversionConfigurationRevision(VTFrameRateConversionConfigurationRevision)
  , pattern VTFrameRateConversionConfigurationRevision1

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

-- | Creates a new frame-rate conversion configuration.
--
-- Returns @nil@ if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels; the maximum value is 8192 for macOS, and 4096 for iOS.   - frameHeight: Height of source frame in pixels; the maximum value is 4320 for macOS, and 2160 for iOS.   - usePrecomputedFlow: A Boolean value that indicates whether you are providing Optical Flow. If false, optical flow is computed on the fly.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about supported levels, see ``VTFrameRateConversionConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> CLong -> CLong -> Bool -> VTFrameRateConversionConfigurationQualityPrioritization -> VTFrameRateConversionConfigurationRevision -> IO (Id VTFrameRateConversionConfiguration)
initWithFrameWidth_frameHeight_usePrecomputedFlow_qualityPrioritization_revision vtFrameRateConversionConfiguration  frameWidth frameHeight usePrecomputedFlow qualityPrioritization revision =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "initWithFrameWidth:frameHeight:usePrecomputedFlow:qualityPrioritization:revision:") (retPtr retVoid) [argCLong (fromIntegral frameWidth), argCLong (fromIntegral frameHeight), argCULong (if usePrecomputedFlow then 1 else 0), argCLong (coerce qualityPrioritization), argCLong (coerce revision)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id VTFrameRateConversionConfiguration)
init_ vtFrameRateConversionConfiguration  =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTFrameRateConversionConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO CLong
frameWidth vtFrameRateConversionConfiguration  =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO CLong
frameHeight vtFrameRateConversionConfiguration  =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "frameHeight") retCLong []

-- | Indicates that caller provides optical flow.
--
-- ObjC selector: @- usePrecomputedFlow@
usePrecomputedFlow :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO Bool
usePrecomputedFlow vtFrameRateConversionConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtFrameRateConversionConfiguration (mkSelector "usePrecomputedFlow") retCULong []

-- | A parameter you use to control quality and performance levels.
--
-- For more information about supported levels, see ``VTFrameRateConversionConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO VTFrameRateConversionConfigurationQualityPrioritization
qualityPrioritization vtFrameRateConversionConfiguration  =
  fmap (coerce :: CLong -> VTFrameRateConversionConfigurationQualityPrioritization) $ sendMsg vtFrameRateConversionConfiguration (mkSelector "qualityPrioritization") retCLong []

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO VTFrameRateConversionConfigurationRevision
revision vtFrameRateConversionConfiguration  =
  fmap (coerce :: CLong -> VTFrameRateConversionConfigurationRevision) $ sendMsg vtFrameRateConversionConfiguration (mkSelector "revision") retCLong []

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    sendClassMsg cls' (mkSelector "supportedRevisions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTFrameRateConversionConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    fmap (coerce :: CLong -> VTFrameRateConversionConfigurationRevision) $ sendClassMsg cls' (mkSelector "defaultRevision") retCLong []

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtFrameRateConversionConfiguration  =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTFrameRateConversionConfiguration vtFrameRateConversionConfiguration => vtFrameRateConversionConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtFrameRateConversionConfiguration  =
  sendMsg vtFrameRateConversionConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | @+ processorSupported@
processorSupported :: IO CUChar
processorSupported  =
  do
    cls' <- getRequiredClass "VTFrameRateConversionConfiguration"
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

