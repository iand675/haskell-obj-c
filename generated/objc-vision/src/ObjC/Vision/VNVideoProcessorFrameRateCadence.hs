{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that defines a frame-based cadence for processing the video stream.
--
-- Generated bindings for @VNVideoProcessorFrameRateCadence@.
module ObjC.Vision.VNVideoProcessorFrameRateCadence
  ( VNVideoProcessorFrameRateCadence
  , IsVNVideoProcessorFrameRateCadence(..)
  , init_
  , initWithFrameRate
  , frameRate
  , initSelector
  , initWithFrameRateSelector
  , frameRateSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> IO (Id VNVideoProcessorFrameRateCadence)
init_ vnVideoProcessorFrameRateCadence  =
  sendMsg vnVideoProcessorFrameRateCadence (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithFrameRate:@
initWithFrameRate :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> CLong -> IO (Id VNVideoProcessorFrameRateCadence)
initWithFrameRate vnVideoProcessorFrameRateCadence  frameRate =
  sendMsg vnVideoProcessorFrameRateCadence (mkSelector "initWithFrameRate:") (retPtr retVoid) [argCLong (fromIntegral frameRate)] >>= ownedObject . castPtr

-- | @- frameRate@
frameRate :: IsVNVideoProcessorFrameRateCadence vnVideoProcessorFrameRateCadence => vnVideoProcessorFrameRateCadence -> IO CLong
frameRate vnVideoProcessorFrameRateCadence  =
  sendMsg vnVideoProcessorFrameRateCadence (mkSelector "frameRate") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithFrameRate:@
initWithFrameRateSelector :: Selector
initWithFrameRateSelector = mkSelector "initWithFrameRate:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector
frameRateSelector = mkSelector "frameRate"

