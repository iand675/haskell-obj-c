{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Detects features in images.
--
-- This class potentially holds onto a lot of state. Hence it may be beneficial from a performance perspective to re-use the same CIDetector instance. Specifying a CIContext when creating a detector may have an impact on performance since this context may be used when analyzing an image.
--
-- Generated bindings for @CIDetector@.
module ObjC.CoreImage.CIDetector
  ( CIDetector
  , IsCIDetector(..)
  , detectorOfType_context_options
  , featuresInImage
  , featuresInImage_options
  , detectorOfType_context_optionsSelector
  , featuresInImageSelector
  , featuresInImage_optionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a new detector instance of the given type.
--
-- The type is used to specify the detection intent. This will return value if the detector type is not supported.
--
-- The context argument specifies the CIContext to be used to operate on the image. May be nil.
--
-- If the input image to -featuresInImage: is the output of a CoreImage operation, it may improve performance to specify the same context that was used to operate on that image.
--
-- The detector may do image processing in this context and if the image is on the GPU and the specified context is a GPU context this may avoid additional upload to / download from the GPU. If the input image is on the CPU (or the output from a CPU based context) specifying a GPU based context (or vice versa) may reduce performance.
--
-- //  The options parameter lets you optionally specify a accuracy / performance tradeoff. Can be nil or an empty dictionary.
--
-- ObjC selector: @+ detectorOfType:context:options:@
detectorOfType_context_options :: (IsNSString type_, IsCIContext context, IsNSDictionary options) => type_ -> context -> options -> IO (Id CIDetector)
detectorOfType_context_options type_ context options =
  do
    cls' <- getRequiredClass "CIDetector"
    sendClassMessage cls' detectorOfType_context_optionsSelector (toNSString type_) (toCIContext context) (toNSDictionary options)

-- | Returns an array of CIFeature instances in the given image. The array is sorted by confidence, highest confidence first.
--
-- ObjC selector: @- featuresInImage:@
featuresInImage :: (IsCIDetector ciDetector, IsCIImage image) => ciDetector -> image -> IO (Id NSArray)
featuresInImage ciDetector image =
  sendMessage ciDetector featuresInImageSelector (toCIImage image)

-- | Returns an array of CIFeature instances in the given image. The array is sorted by confidence, highest confidence first. The options dictionary can contain a CIDetectorImageOrientation key value.
--
-- ObjC selector: @- featuresInImage:options:@
featuresInImage_options :: (IsCIDetector ciDetector, IsCIImage image, IsNSDictionary options) => ciDetector -> image -> options -> IO (Id NSArray)
featuresInImage_options ciDetector image options =
  sendMessage ciDetector featuresInImage_optionsSelector (toCIImage image) (toNSDictionary options)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @detectorOfType:context:options:@
detectorOfType_context_optionsSelector :: Selector '[Id NSString, Id CIContext, Id NSDictionary] (Id CIDetector)
detectorOfType_context_optionsSelector = mkSelector "detectorOfType:context:options:"

-- | @Selector@ for @featuresInImage:@
featuresInImageSelector :: Selector '[Id CIImage] (Id NSArray)
featuresInImageSelector = mkSelector "featuresInImage:"

-- | @Selector@ for @featuresInImage:options:@
featuresInImage_optionsSelector :: Selector '[Id CIImage, Id NSDictionary] (Id NSArray)
featuresInImage_optionsSelector = mkSelector "featuresInImage:options:"

