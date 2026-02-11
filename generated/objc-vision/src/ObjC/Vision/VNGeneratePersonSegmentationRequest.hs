{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , initWithCompletionHandlerSelector
  , supportedOutputPixelFormatsAndReturnErrorSelector
  , qualityLevelSelector
  , setQualityLevelSelector
  , outputPixelFormatSelector
  , setOutputPixelFormatSelector
  , resultsSelector

  -- * Enum types
  , VNGeneratePersonSegmentationRequestQualityLevel(VNGeneratePersonSegmentationRequestQualityLevel)
  , pattern VNGeneratePersonSegmentationRequestQualityLevelAccurate
  , pattern VNGeneratePersonSegmentationRequestQualityLevelBalanced
  , pattern VNGeneratePersonSegmentationRequestQualityLevelFast

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

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNGeneratePersonSegmentationRequest)
new  =
  do
    cls' <- getRequiredClass "VNGeneratePersonSegmentationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO (Id VNGeneratePersonSegmentationRequest)
init_ vnGeneratePersonSegmentationRequest  =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> Ptr () -> IO (Id VNGeneratePersonSegmentationRequest)
initWithCompletionHandler vnGeneratePersonSegmentationRequest  completionHandler =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "initWithCompletionHandler:") (retPtr retVoid) [argPtr (castPtr completionHandler :: Ptr ())] >>= ownedObject . castPtr

-- | Obtain the collection of supported output pixel formats for the configured request.
--
-- ObjC selector: @- supportedOutputPixelFormatsAndReturnError:@
supportedOutputPixelFormatsAndReturnError :: (IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest, IsNSError error_) => vnGeneratePersonSegmentationRequest -> error_ -> IO (Id NSArray)
supportedOutputPixelFormatsAndReturnError vnGeneratePersonSegmentationRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnGeneratePersonSegmentationRequest (mkSelector "supportedOutputPixelFormatsAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | The quality level selects which techniques will be used during the person segmentation. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- qualityLevel@
qualityLevel :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO VNGeneratePersonSegmentationRequestQualityLevel
qualityLevel vnGeneratePersonSegmentationRequest  =
  fmap (coerce :: CULong -> VNGeneratePersonSegmentationRequestQualityLevel) $ sendMsg vnGeneratePersonSegmentationRequest (mkSelector "qualityLevel") retCULong []

-- | The quality level selects which techniques will be used during the person segmentation. There are trade-offs between performance and accuracy.
--
-- ObjC selector: @- setQualityLevel:@
setQualityLevel :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> VNGeneratePersonSegmentationRequestQualityLevel -> IO ()
setQualityLevel vnGeneratePersonSegmentationRequest  value =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "setQualityLevel:") retVoid [argCULong (coerce value)]

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_OneComponent32Float, kCVPixelFormatType_OneComponent16Half, and kCVPixelFormatType_OneComponent8. Default is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- outputPixelFormat@
outputPixelFormat :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO CUInt
outputPixelFormat vnGeneratePersonSegmentationRequest  =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "outputPixelFormat") retCUInt []

-- | Pixel format type of the output buffer. Valid values are kCVPixelFormatType_OneComponent32Float, kCVPixelFormatType_OneComponent16Half, and kCVPixelFormatType_OneComponent8. Default is kCVPixelFormatType_OneComponent8.
--
-- ObjC selector: @- setOutputPixelFormat:@
setOutputPixelFormat :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> CUInt -> IO ()
setOutputPixelFormat vnGeneratePersonSegmentationRequest  value =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "setOutputPixelFormat:") retVoid [argCUInt (fromIntegral value)]

-- | @- results@
results :: IsVNGeneratePersonSegmentationRequest vnGeneratePersonSegmentationRequest => vnGeneratePersonSegmentationRequest -> IO (Id NSArray)
results vnGeneratePersonSegmentationRequest  =
  sendMsg vnGeneratePersonSegmentationRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @supportedOutputPixelFormatsAndReturnError:@
supportedOutputPixelFormatsAndReturnErrorSelector :: Selector
supportedOutputPixelFormatsAndReturnErrorSelector = mkSelector "supportedOutputPixelFormatsAndReturnError:"

-- | @Selector@ for @qualityLevel@
qualityLevelSelector :: Selector
qualityLevelSelector = mkSelector "qualityLevel"

-- | @Selector@ for @setQualityLevel:@
setQualityLevelSelector :: Selector
setQualityLevelSelector = mkSelector "setQualityLevel:"

-- | @Selector@ for @outputPixelFormat@
outputPixelFormatSelector :: Selector
outputPixelFormatSelector = mkSelector "outputPixelFormat"

-- | @Selector@ for @setOutputPixelFormat:@
setOutputPixelFormatSelector :: Selector
setOutputPixelFormatSelector = mkSelector "setOutputPixelFormat:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

