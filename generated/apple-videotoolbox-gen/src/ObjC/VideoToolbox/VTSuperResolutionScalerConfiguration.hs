{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration that you use to set up the super-resolution processor.
--
-- This configuration enables the super-resolution processor on a @VTFrameProcessor@ session.
--
-- > Important: The super-resolution processor may require ML models which the framework needs to download in order to operate. Before calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` with an instance of this class, it is important that you verify that the necessary models are present by checking ``configurationModelStatus``. If models are not available, you can trigger model download using the ``downloadConfigurationModelWithCompletionHandler:`` method. Best practice is to confirm availability of models and drive download with user awareness and interaction before engaging workflows that need this processor.
--
-- Generated bindings for @VTSuperResolutionScalerConfiguration@.
module ObjC.VideoToolbox.VTSuperResolutionScalerConfiguration
  ( VTSuperResolutionScalerConfiguration
  , IsVTSuperResolutionScalerConfiguration(..)
  , initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revision
  , init_
  , new
  , downloadConfigurationModelWithCompletionHandler
  , frameWidth
  , frameHeight
  , inputType
  , precomputedFlow
  , scaleFactor
  , qualityPrioritization
  , revision
  , supportedRevisions
  , defaultRevision
  , frameSupportedPixelFormats
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , configurationModelStatus
  , configurationModelPercentageAvailable
  , supported
  , supportedScaleFactors
  , initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revisionSelector
  , initSelector
  , newSelector
  , downloadConfigurationModelWithCompletionHandlerSelector
  , frameWidthSelector
  , frameHeightSelector
  , inputTypeSelector
  , precomputedFlowSelector
  , scaleFactorSelector
  , qualityPrioritizationSelector
  , revisionSelector
  , supportedRevisionsSelector
  , defaultRevisionSelector
  , frameSupportedPixelFormatsSelector
  , sourcePixelBufferAttributesSelector
  , destinationPixelBufferAttributesSelector
  , configurationModelStatusSelector
  , configurationModelPercentageAvailableSelector
  , supportedSelector
  , supportedScaleFactorsSelector

  -- * Enum types
  , VTSuperResolutionScalerConfigurationInputType(VTSuperResolutionScalerConfigurationInputType)
  , pattern VTSuperResolutionScalerConfigurationInputTypeVideo
  , pattern VTSuperResolutionScalerConfigurationInputTypeImage
  , VTSuperResolutionScalerConfigurationModelStatus(VTSuperResolutionScalerConfigurationModelStatus)
  , pattern VTSuperResolutionScalerConfigurationModelStatusDownloadRequired
  , pattern VTSuperResolutionScalerConfigurationModelStatusDownloading
  , pattern VTSuperResolutionScalerConfigurationModelStatusReady
  , VTSuperResolutionScalerConfigurationQualityPrioritization(VTSuperResolutionScalerConfigurationQualityPrioritization)
  , pattern VTSuperResolutionScalerConfigurationQualityPrioritizationNormal
  , VTSuperResolutionScalerConfigurationRevision(VTSuperResolutionScalerConfigurationRevision)
  , pattern VTSuperResolutionScalerConfigurationRevision1

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

-- | Creates a new super-resolution scaler processor configuration.
--
-- This processor increases resolution of an image or video. Returns @nil@ if dimensions are out of range or revision is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels. With ``VTSuperResolutionScalerConfigurationInputTypeVideo``,   maximum width is 1920 on macOS and 1440 on iOS. With ``VTSuperResolutionScalerConfigurationInputTypeImage``,   maximum width is 1920.   - frameHeight: Height of source frame in pixels. With ``VTSuperResolutionScalerConfigurationInputTypeVideo``,   maximum height is 1080. With ``VTSuperResolutionScalerConfigurationInputTypeImage``, maximum height is 1920 on   macOS and 1080 on iOS.   - scaleFactor: Indicates the scale factor between input and output.   - inputType: Indicates the type of input, either video or image.   - usePrecomputedFlow: Boolean value to indicate that you provide optical flow; if false, this configuration   computes the optical flow on the fly.   - qualityPrioritization: A level you use to prioritize quality or performance; for more information about   supported levels, see ``VTSuperResolutionScalerConfigurationQualityPrioritization``.   - revision: The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:scaleFactor:inputType:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revision :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> CLong -> CLong -> CLong -> VTSuperResolutionScalerConfigurationInputType -> Bool -> VTSuperResolutionScalerConfigurationQualityPrioritization -> VTSuperResolutionScalerConfigurationRevision -> IO (Id VTSuperResolutionScalerConfiguration)
initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revision vtSuperResolutionScalerConfiguration  frameWidth frameHeight scaleFactor inputType usePrecomputedFlow qualityPrioritization revision =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "initWithFrameWidth:frameHeight:scaleFactor:inputType:usePrecomputedFlow:qualityPrioritization:revision:") (retPtr retVoid) [argCLong frameWidth, argCLong frameHeight, argCLong scaleFactor, argCLong (coerce inputType), argCULong (if usePrecomputedFlow then 1 else 0), argCLong (coerce qualityPrioritization), argCLong (coerce revision)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO (Id VTSuperResolutionScalerConfiguration)
init_ vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTSuperResolutionScalerConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Downloads models that the system needs for the current configuration.
--
-- This method downloads model assets required for the current configuration in background. You should call this method if ``configurationModelStatus`` is ``VTSuperResolutionScalerConfigurationModelStatusDownloadRequired``. After this method is called, you can query ``configurationModelPercentageAvailable`` to determine progress of model asset download process. If the download fails, the completion handler is invoked with an @NSError@, and the ``configurationModelStatus`` goes back to ``VTSuperResolutionScalerConfigurationModelStatusDownloadRequired``. If the download succeeds, the completion handler is invoked with @nil@ NSError.
--
-- ObjC selector: @- downloadConfigurationModelWithCompletionHandler:@
downloadConfigurationModelWithCompletionHandler :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> Ptr () -> IO ()
downloadConfigurationModelWithCompletionHandler vtSuperResolutionScalerConfiguration  completionHandler =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "downloadConfigurationModelWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO CLong
frameWidth vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO CLong
frameHeight vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "frameHeight") retCLong []

-- | Indicates the type of input.
--
-- ObjC selector: @- inputType@
inputType :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO VTSuperResolutionScalerConfigurationInputType
inputType vtSuperResolutionScalerConfiguration  =
    fmap (coerce :: CLong -> VTSuperResolutionScalerConfigurationInputType) $ sendMsg vtSuperResolutionScalerConfiguration (mkSelector "inputType") retCLong []

-- | Indicates that you provide optical flow.
--
-- ObjC selector: @- precomputedFlow@
precomputedFlow :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO Bool
precomputedFlow vtSuperResolutionScalerConfiguration  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vtSuperResolutionScalerConfiguration (mkSelector "precomputedFlow") retCULong []

-- | Indicates the scale factor between input and output.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO CLong
scaleFactor vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "scaleFactor") retCLong []

-- | A parameter to control quality and performance levels.
--
-- For more information about supported levels, see ``VTSuperResolutionScalerConfigurationQualityPrioritization``.
--
-- ObjC selector: @- qualityPrioritization@
qualityPrioritization :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO VTSuperResolutionScalerConfigurationQualityPrioritization
qualityPrioritization vtSuperResolutionScalerConfiguration  =
    fmap (coerce :: CLong -> VTSuperResolutionScalerConfigurationQualityPrioritization) $ sendMsg vtSuperResolutionScalerConfiguration (mkSelector "qualityPrioritization") retCLong []

-- | The specific algorithm or configuration revision you use to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO VTSuperResolutionScalerConfigurationRevision
revision vtSuperResolutionScalerConfiguration  =
    fmap (coerce :: CLong -> VTSuperResolutionScalerConfigurationRevision) $ sendMsg vtSuperResolutionScalerConfiguration (mkSelector "revision") retCLong []

-- | Provides the collection of currently supported algorithms or configuration revisions for the class of configuration.
--
-- A property you use to introspect at runtime which revisions are available for each configuration.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerConfiguration"
    sendClassMsg cls' (mkSelector "supportedRevisions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides the default revision of a specific algorithm or configuration.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO VTSuperResolutionScalerConfigurationRevision
defaultRevision  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerConfiguration"
    fmap (coerce :: CLong -> VTSuperResolutionScalerConfigurationRevision) $ sendClassMsg cls' (mkSelector "defaultRevision") retCLong []

-- | Available supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "frameSupportedPixelFormats") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Reports the download status of models that the system needs for the current configuration.
--
-- ObjC selector: @- configurationModelStatus@
configurationModelStatus :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO VTSuperResolutionScalerConfigurationModelStatus
configurationModelStatus vtSuperResolutionScalerConfiguration  =
    fmap (coerce :: CLong -> VTSuperResolutionScalerConfigurationModelStatus) $ sendMsg vtSuperResolutionScalerConfiguration (mkSelector "configurationModelStatus") retCLong []

-- | Returns a floating point value between 0.0 and 1.0 indicating the percentage of required model assets that have been downloaded.
--
-- ObjC selector: @- configurationModelPercentageAvailable@
configurationModelPercentageAvailable :: IsVTSuperResolutionScalerConfiguration vtSuperResolutionScalerConfiguration => vtSuperResolutionScalerConfiguration -> IO CFloat
configurationModelPercentageAvailable vtSuperResolutionScalerConfiguration  =
    sendMsg vtSuperResolutionScalerConfiguration (mkSelector "configurationModelPercentageAvailable") retCFloat []

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | Reports the set of supported scale factors to use when initializing a super-resolution scaler configuration.
--
-- ObjC selector: @+ supportedScaleFactors@
supportedScaleFactors :: IO (Id NSArray)
supportedScaleFactors  =
  do
    cls' <- getRequiredClass "VTSuperResolutionScalerConfiguration"
    sendClassMsg cls' (mkSelector "supportedScaleFactors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:scaleFactor:inputType:usePrecomputedFlow:qualityPrioritization:revision:@
initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revisionSelector :: Selector
initWithFrameWidth_frameHeight_scaleFactor_inputType_usePrecomputedFlow_qualityPrioritization_revisionSelector = mkSelector "initWithFrameWidth:frameHeight:scaleFactor:inputType:usePrecomputedFlow:qualityPrioritization:revision:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @downloadConfigurationModelWithCompletionHandler:@
downloadConfigurationModelWithCompletionHandlerSelector :: Selector
downloadConfigurationModelWithCompletionHandlerSelector = mkSelector "downloadConfigurationModelWithCompletionHandler:"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @inputType@
inputTypeSelector :: Selector
inputTypeSelector = mkSelector "inputType"

-- | @Selector@ for @precomputedFlow@
precomputedFlowSelector :: Selector
precomputedFlowSelector = mkSelector "precomputedFlow"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

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

-- | @Selector@ for @configurationModelStatus@
configurationModelStatusSelector :: Selector
configurationModelStatusSelector = mkSelector "configurationModelStatus"

-- | @Selector@ for @configurationModelPercentageAvailable@
configurationModelPercentageAvailableSelector :: Selector
configurationModelPercentageAvailableSelector = mkSelector "configurationModelPercentageAvailable"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @supportedScaleFactors@
supportedScaleFactorsSelector :: Selector
supportedScaleFactorsSelector = mkSelector "supportedScaleFactors"

