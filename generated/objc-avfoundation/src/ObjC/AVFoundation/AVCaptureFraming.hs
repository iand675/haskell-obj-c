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
  , initSelector
  , newSelector
  , aspectRatioSelector
  , zoomFactorSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO (Id AVCaptureFraming)
init_ avCaptureFraming  =
  sendMsg avCaptureFraming (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureFraming)
new  =
  do
    cls' <- getRequiredClass "AVCaptureFraming"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An aspect ratio.
--
-- One of the enumerated aspect ratios  suitable for use with the ``AVCaptureDevice`` dynamic aspect ratio APIs.
--
-- ObjC selector: @- aspectRatio@
aspectRatio :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO (Id NSString)
aspectRatio avCaptureFraming  =
  sendMsg avCaptureFraming (mkSelector "aspectRatio") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A zoom factor.
--
-- Suitable for use with the ``AVCaptureDevice/videoZoomFactor`` property or ``AVCaptureDevice/rampToVideoZoomFactor:withRate:``.
--
-- ObjC selector: @- zoomFactor@
zoomFactor :: IsAVCaptureFraming avCaptureFraming => avCaptureFraming -> IO CFloat
zoomFactor avCaptureFraming  =
  sendMsg avCaptureFraming (mkSelector "zoomFactor") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @aspectRatio@
aspectRatioSelector :: Selector
aspectRatioSelector = mkSelector "aspectRatio"

-- | @Selector@ for @zoomFactor@
zoomFactorSelector :: Selector
zoomFactorSelector = mkSelector "zoomFactor"

