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
  , initWithForwardFlow_backwardFlowSelector
  , initSelector
  , newSelector
  , forwardFlowSelector
  , backwardFlowSelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates a new instance of forward and backward optical flow with pixel buffers.
--
-- Create a new instance with forward and backward optical flow ``CVPixelBuffer``s. Instances retain the pixel buffers you provide to this method. Returns @nil@ if either @CVPixelBuffer@ is NULL or the @CVPixelBuffer@s are not @IOSurface@ backed.
--
-- - Parameters:   - forwardFlow: @CVPixelBuffer@ that contains forward optical flow; it must not be @nil@ and must be @IOSurface@ backed.   - backwardFlow: @CVPixelBuffer@ that contains backward optical flow; it must not be @nil@ and must be @IOSurface@ backed.
--
-- ObjC selector: @- initWithForwardFlow:backwardFlow:@
initWithForwardFlow_backwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> Ptr () -> Ptr () -> IO (Id VTFrameProcessorOpticalFlow)
initWithForwardFlow_backwardFlow vtFrameProcessorOpticalFlow  forwardFlow backwardFlow =
  sendMsg vtFrameProcessorOpticalFlow (mkSelector "initWithForwardFlow:backwardFlow:") (retPtr retVoid) [argPtr forwardFlow, argPtr backwardFlow] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Id VTFrameProcessorOpticalFlow)
init_ vtFrameProcessorOpticalFlow  =
  sendMsg vtFrameProcessorOpticalFlow (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTFrameProcessorOpticalFlow)
new  =
  do
    cls' <- getRequiredClass "VTFrameProcessorOpticalFlow"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns the forward optical flow @CVPixelBuffer@ that you provided when you initialized the object.
--
-- ObjC selector: @- forwardFlow@
forwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Ptr ())
forwardFlow vtFrameProcessorOpticalFlow  =
  fmap castPtr $ sendMsg vtFrameProcessorOpticalFlow (mkSelector "forwardFlow") (retPtr retVoid) []

-- | Returns the backward optical flow @CVPixelBuffer@ that you provided when you initialized the object.
--
-- ObjC selector: @- backwardFlow@
backwardFlow :: IsVTFrameProcessorOpticalFlow vtFrameProcessorOpticalFlow => vtFrameProcessorOpticalFlow -> IO (Ptr ())
backwardFlow vtFrameProcessorOpticalFlow  =
  fmap castPtr $ sendMsg vtFrameProcessorOpticalFlow (mkSelector "backwardFlow") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithForwardFlow:backwardFlow:@
initWithForwardFlow_backwardFlowSelector :: Selector
initWithForwardFlow_backwardFlowSelector = mkSelector "initWithForwardFlow:backwardFlow:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @forwardFlow@
forwardFlowSelector :: Selector
forwardFlowSelector = mkSelector "forwardFlow"

-- | @Selector@ for @backwardFlow@
backwardFlowSelector :: Selector
backwardFlowSelector = mkSelector "backwardFlow"

