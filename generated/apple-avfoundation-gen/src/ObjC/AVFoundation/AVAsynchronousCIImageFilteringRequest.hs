{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAsynchronousCIImageFilteringRequest@.
module ObjC.AVFoundation.AVAsynchronousCIImageFilteringRequest
  ( AVAsynchronousCIImageFilteringRequest
  , IsAVAsynchronousCIImageFilteringRequest(..)
  , finishWithImage_context
  , finishWithError
  , sourceImage
  , finishWithErrorSelector
  , finishWithImage_contextSelector
  , sourceImageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Callback the filter should call when filtering succeeded. If context is nil then a default context will be used, GPU-accelerated if possible.
--
-- It is safe to pass in the sourceImage in which case the filter will appear to have no effect, essentially functioning as a pass-through.
--
-- ObjC selector: @- finishWithImage:context:@
finishWithImage_context :: (IsAVAsynchronousCIImageFilteringRequest avAsynchronousCIImageFilteringRequest, IsCIImage filteredImage, IsCIContext context) => avAsynchronousCIImageFilteringRequest -> filteredImage -> context -> IO ()
finishWithImage_context avAsynchronousCIImageFilteringRequest filteredImage context =
  sendMessage avAsynchronousCIImageFilteringRequest finishWithImage_contextSelector (toCIImage filteredImage) (toCIContext context)

-- | Callback the filter should call when filtering failed. The error parameter should describe the actual error.
--
-- ObjC selector: @- finishWithError:@
finishWithError :: (IsAVAsynchronousCIImageFilteringRequest avAsynchronousCIImageFilteringRequest, IsNSError error_) => avAsynchronousCIImageFilteringRequest -> error_ -> IO ()
finishWithError avAsynchronousCIImageFilteringRequest error_ =
  sendMessage avAsynchronousCIImageFilteringRequest finishWithErrorSelector (toNSError error_)

-- | CIImage for the first enabled source video track. Unlike AVAsynchronousVideoCompositionRequest, renderContext.renderTransform is already applied to the source image.
--
-- ObjC selector: @- sourceImage@
sourceImage :: IsAVAsynchronousCIImageFilteringRequest avAsynchronousCIImageFilteringRequest => avAsynchronousCIImageFilteringRequest -> IO (Id CIImage)
sourceImage avAsynchronousCIImageFilteringRequest =
  sendMessage avAsynchronousCIImageFilteringRequest sourceImageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finishWithImage:context:@
finishWithImage_contextSelector :: Selector '[Id CIImage, Id CIContext] ()
finishWithImage_contextSelector = mkSelector "finishWithImage:context:"

-- | @Selector@ for @finishWithError:@
finishWithErrorSelector :: Selector '[Id NSError] ()
finishWithErrorSelector = mkSelector "finishWithError:"

-- | @Selector@ for @sourceImage@
sourceImageSelector :: Selector '[] (Id CIImage)
sourceImageSelector = mkSelector "sourceImage"

