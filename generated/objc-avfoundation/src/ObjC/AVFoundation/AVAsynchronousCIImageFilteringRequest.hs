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
  , finishWithImage_contextSelector
  , finishWithErrorSelector
  , sourceImageSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
finishWithImage_context avAsynchronousCIImageFilteringRequest  filteredImage context =
withObjCPtr filteredImage $ \raw_filteredImage ->
  withObjCPtr context $ \raw_context ->
      sendMsg avAsynchronousCIImageFilteringRequest (mkSelector "finishWithImage:context:") retVoid [argPtr (castPtr raw_filteredImage :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())]

-- | Callback the filter should call when filtering failed. The error parameter should describe the actual error.
--
-- ObjC selector: @- finishWithError:@
finishWithError :: (IsAVAsynchronousCIImageFilteringRequest avAsynchronousCIImageFilteringRequest, IsNSError error_) => avAsynchronousCIImageFilteringRequest -> error_ -> IO ()
finishWithError avAsynchronousCIImageFilteringRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg avAsynchronousCIImageFilteringRequest (mkSelector "finishWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- | CIImage for the first enabled source video track. Unlike AVAsynchronousVideoCompositionRequest, renderContext.renderTransform is already applied to the source image.
--
-- ObjC selector: @- sourceImage@
sourceImage :: IsAVAsynchronousCIImageFilteringRequest avAsynchronousCIImageFilteringRequest => avAsynchronousCIImageFilteringRequest -> IO (Id CIImage)
sourceImage avAsynchronousCIImageFilteringRequest  =
  sendMsg avAsynchronousCIImageFilteringRequest (mkSelector "sourceImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finishWithImage:context:@
finishWithImage_contextSelector :: Selector
finishWithImage_contextSelector = mkSelector "finishWithImage:context:"

-- | @Selector@ for @finishWithError:@
finishWithErrorSelector :: Selector
finishWithErrorSelector = mkSelector "finishWithError:"

-- | @Selector@ for @sourceImage@
sourceImageSelector :: Selector
sourceImageSelector = mkSelector "sourceImage"

