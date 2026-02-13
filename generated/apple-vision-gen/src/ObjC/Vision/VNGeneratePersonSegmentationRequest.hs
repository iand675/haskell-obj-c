{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Performs person segmentation on an image generating a mask.
--
-- Generated bindings for @VNGeneratePersonSegmentationRequest@.
module ObjC.Vision.VNGeneratePersonSegmentationRequest
  ( VNGeneratePersonSegmentationRequest
  , IsVNGeneratePersonSegmentationRequest(..)
  , new
  , init_
  , initWithCompletionHandler
  , supportedOutputPixelFormatsAndReturnError
  , qualityLevel
  , setQualityLevel
  , outputPixelFormat
  , setOutputPixelFormat
  , results
  , initSelector
  , initWithCompletionHandlerSelector
  , newSelector
  , outputPixelFormatSelector
  , qualityLevelSelector
  , resultsSelector
  , setOutputPixelFormatSelector
  , setQualityLevelSelector
  , supportedOutputPixelFormatsAndReturnErrorSelector

  -- * Enum types
  , VNGeneratePersonSegmentationRequestQualityLevel(VNGeneratePersonSegmentationRequestQualityLevel)
  , pattern VNGeneratePersonSegmentationRequestQualityLevelAccurate
  , pattern VNGeneratePersonSegmentationRequestQualityLevelBalanced
  , pattern VNGeneratePersonSegmentationRequestQualityLevelFast

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNGeneratePersonSegmentationRequest)
new  =
  do
    cls' <- getRequiredClass "VNGeneratePersonSegmentationRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO (Id VNGeneratePersonSegmentationRequest)
init_ vnGeneratePersonSegmentationRequest =
  sendOwnedMessage vnGeneratePersonSegmentationRequest initSelector

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> Ptr () -> IO (Id VNGeneratePersonSegmentationRequest)
initWithCompletionHandler vnGeneratePersonSegmentationRequest completionHandler =
  sendOwnedMessage vnGeneratePersonSegmentationRequest initWithCompletionHandlerSelector completionHandler

-- | Obtain the collection of supported output pixel formats for the configured request.
--
-- ObjC selector: @- supportedOutputPixelFormatsAndReturnError:@
supportedOutputPixelFormatsAndReturnError :: (IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest, IsNSError error_) => vnGeneratePersonSegmentationRequest -> error_ -> IO (Id NSArray)
supportedOutputPixelFormatsAndReturnError vnGeneratePersonSegmentationRequest error_ =
  sendMessage vnGeneratePersonSegmentationRequest supportedOutputPixelFormatsAndReturnErrorSelector (toNSError error_)

-- | The quality level selects which techniques will be used during the person segmentation. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- qualityLevel@
qualityLevel :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO VNGeneratePersonSegmentationRequestQualityLevel
qualityLevel vnGeneratePersonSegmentationRequest =
  sendMessage vnGeneratePersonSegmentationRequest qualityLevelSelector

-- | The quality level selects which techniques will be used during the person segmentation. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- setQualityLevel:@
setQualityLevel :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> VNGeneratePersonSegmentationRequestQualityLevel -> IO ()
setQualityLevel vnGeneratePersonSegmentationRequest value =
  sendMessage vnGeneratePersonSegmentationRequest setQualityLevelSelector value

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_OneComponent32Float, kCVPixelFormatType_OneComponent16Half, and kCVPixelFormatType_OneComponent8. Default is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO CUInt
outputPixelFormat vnGeneratePersonSegmentationRequest =
  sendMessage vnGeneratePersonSegmentationRequest outputPixelFormatSelector

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_OneComponent32Float, kCVPixelFormatType_OneComponent16Half, and kCVPixelFormatType_OneComponent8. Default is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> CUInt -> IO ()
setOutputPixelFormat vnGeneratePersonSegmentationRequest value =
  sendMessage vnGeneratePersonSegmentationRequest setOutputPixelFormatSelector value

-- | @- results@
results :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO (Id NSArray)
results vnGeneratePersonSegmentationRequest =
  sendMessage vnGeneratePersonSegmentationRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNGeneratePersonSegmentationRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNGeneratePersonSegmentationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNGeneratePersonSegmentationRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @supportedOutputPixelFormatsAndReturnError:@
supportedOutputPixelFormatsAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedOutputPixelFormatsAndReturnErrorSelector = mkSelector "supportedOutputPixelFormatsAndReturnError:"

-- | @Selector@ for @qualityLevel@
qualityLevelSelector :: Selector '[] VNGeneratePersonSegmentationRequestQualityLevel
qualityLevelSelector = mkSelector "qualityLevel"

-- | @Selector@ for @setQualityLevel:@
setQualityLevelSelector :: Selector '[VNGeneratePersonSegmentationRequestQualityLevel] ()
setQualityLevelSelector = mkSelector "setQualityLevel:"

-- | @Selector@ for @outputPixelFormat@
outputPixelFormatSelector :: Selector '[] CUInt
outputPixelFormatSelector = mkSelector "outputPixelFormat"

-- | @Selector@ for @setOutputPixelFormat:@
setOutputPixelFormatSelector :: Selector '[CUInt] ()
setOutputPixelFormatSelector = mkSelector "setOutputPixelFormat:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

