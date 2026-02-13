{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A framing, consisting of an aspect ratio and a zoom factor.
--
-- An ``AVCaptureSmartFramingMonitor`` provides framing recommendations using this object.
--
-- Generated bindings for @AVCaptureFraming@.
module ObjC.AVFoundation.AVCaptureFraming
  ( AVCaptureFraming
  , IsAVCaptureFraming(..)
  , init_
  , new
  , aspectRatio
  , zoomFactor
  , aspectRatioSelector
  , initSelector
  , newSelector
  , zoomFactorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO (Id AVCaptureFraming)
init_ avCaptureFraming =
  sendOwnedMessage avCaptureFraming initSelector

-- | @+ new@
new :: IO (Id AVCaptureFraming)
new  =
  do
    cls' <- getRequiredClass "AVCaptureFraming"
    sendOwnedClassMessage cls' newSelector

-- | An aspect ratio.
--
-- One of the enumerated aspect ratios  suitable for use with the ``AVCaptureDevice`` dynamic aspect ratio APIs.
--
-- ObjC selector: @- aspectRatio@
aspectRatio :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO (Id NSString)
aspectRatio avCaptureFraming =
  sendMessage avCaptureFraming aspectRatioSelector

-- | A zoom factor.
--
-- Suitable for use with the ``AVCaptureDevice/videoZoomFactor`` property or ``AVCaptureDevice/rampToVideoZoomFactor:withRate:``.
--
-- ObjC selector: @- zoomFactor@
zoomFactor :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO CFloat
zoomFactor avCaptureFraming =
  sendMessage avCaptureFraming zoomFactorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureFraming)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureFraming)
newSelector = mkSelector "new"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector '[] (Id NSString)
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @zoomFactor@
zoomFactorSelector :: Selector '[] CFloat
zoomFactorSelector = mkSelector "zoomFactor"

