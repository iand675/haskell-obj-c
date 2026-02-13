{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Helper class to wrap optical flow.
--
-- Instances retain the backing pixel buffers that you provide.
--
-- Generated bindings for @VTFrameProcessorOpticalFlow@.
module ObjC.VideoToolbox.VTFrameProcessorOpticalFlow
  ( VTFrameProcessorOpticalFlow
  , IsVTFrameProcessorOpticalFlow(..)
  , initWithForwardFlow_backwardFlow
  , init_
  , new
  , forwardFlow
  , backwardFlow
  , backwardFlowSelector
  , forwardFlowSelector
  , initSelector
  , initWithForwardFlow_backwardFlowSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new instance of forward and backward optical flow with pixel buffers.
--
-- Create a new instance with forward and backward optical flow ``CVPixelBuffer``s. Instances retain the pixel buffers you provide to this method. Returns @nil@ if either @CVPixelBuffer@ is NULL or the @CVPixelBuffer@s are not @IOSurface@ backed.
--
-- - Parameters:   - forwardFlow: @CVPixelBuffer@ that contains forward optical flow; it must not be @nil@ and must be @IOSurface@ backed.   - backwardFlow: @CVPixelBuffer@ that contains backward optical flow; it must not be @nil@ and must be @IOSurface@ backed.
--
-- ObjC selector: @- initWithForwardFlow:backwardFlow:@
initWithForwardFlow_backwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> Ptr () -> Ptr () -> IO (Id VTFrameProcessorOpticalFlow)
initWithForwardFlow_backwardFlow vtFrameProcessorOpticalFlow forwardFlow backwardFlow =
  sendOwnedMessage vtFrameProcessorOpticalFlow initWithForwardFlow_backwardFlowSelector forwardFlow backwardFlow

-- | @- init@
init_ :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Id VTFrameProcessorOpticalFlow)
init_ vtFrameProcessorOpticalFlow =
  sendOwnedMessage vtFrameProcessorOpticalFlow initSelector

-- | @+ new@
new :: IO (Id VTFrameProcessorOpticalFlow)
new  =
  do
    cls' <- getRequiredClass "VTFrameProcessorOpticalFlow"
    sendOwnedClassMessage cls' newSelector

-- | Returns the forward optical flow @CVPixelBuffer@ that you provided when you initialized the object.
--
-- ObjC selector: @- forwardFlow@
forwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Ptr ())
forwardFlow vtFrameProcessorOpticalFlow =
  sendMessage vtFrameProcessorOpticalFlow forwardFlowSelector

-- | Returns the backward optical flow @CVPixelBuffer@ that you provided when you initialized the object.
--
-- ObjC selector: @- backwardFlow@
backwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Ptr ())
backwardFlow vtFrameProcessorOpticalFlow =
  sendMessage vtFrameProcessorOpticalFlow backwardFlowSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithForwardFlow:backwardFlow:@
initWithForwardFlow_backwardFlowSelector :: Selector '[Ptr (), Ptr ()] (Id VTFrameProcessorOpticalFlow)
initWithForwardFlow_backwardFlowSelector = mkSelector "initWithForwardFlow:backwardFlow:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTFrameProcessorOpticalFlow)
newSelector = mkSelector "new"

-- | @Selector@ for @forwardFlow@
forwardFlowSelector :: Selector '[] (Ptr ())
forwardFlowSelector = mkSelector "forwardFlow"

-- | @Selector@ for @backwardFlow@
backwardFlowSelector :: Selector '[] (Ptr ())
backwardFlowSelector = mkSelector "backwardFlow"

